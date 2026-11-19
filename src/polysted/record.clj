;; src/polysted/record.clj — レコーディング (原作 RECORD.C) の変換。純粋関数のみ。MIDI 入力と演奏は src/polysted/session.clj。
;;
;; 原作で確認したこと (RECORD.C real_record・proc_next・flash・ent_dat・proc_excl・rec_conv・rec_ext・meas_adjust):
;;   - 録音中の MIDI 入力を時刻付きで貯め、終わってから 4 バイトのレコードへ変換する。
;;   - 録音するのは MODE が Mix か Rec のトラック (trmod の bit1)。そのトラックが 1 つなら録音チャンネル
;;     (RECORDING CH) の入力を、複数ならトラックごとにそのトラックのチャンネルの入力を入れる。
;;     エクスクルーシブは録音チャンネル (1 つのとき) か MIDI チャンネル 1 の回で入れる。
;;   - ノート: ノートオンから同じキー・チャンネルのノートオフ (かベロシティ 0 のノートオン) までを GT。
;;     ノートオフが無い音は捨てる。ノートの間の時間は直前の行の ST に足す (240 まで、0xDE の行と
;;     0xF0 以上の行には足さず休符を足す。エクスクルーシブの続きの行の後ろは 0x98 の行に足す)。
;;     小節の長さごとに小節線を入れる (間の時間は小節線で分ける)。GT が 240 を超える音は 240 ずつ
;;     つなぐ (GT = ST+1 の音を続けてタイにする)。
;;   - コントロールチェンジ → [EB 0 番号 値]、プログラム → [EC 0 番号 0]、ベンド → [EE 0 下位 上位]。
;;   - エクスクルーシブ: Roland の DT1 (41 dev model 12 アドレス 3 バイト データ 1〜4 バイト cs F7) は
;;     デバイス・モデルが変われば [DF 0 dev model]、[DD 0 上位 中位] と [DE 0 下位 データ] に分ける
;;     (下位が 7F を超えたら中位を 1 つ上げる。最初のデバイス・モデルは 10・16)。41 ? ? 12 で短すぎるものは捨てる。
;;     ほかは [98 0 0 0] と [F7 0 データ データ] の行。
;;   - トラックへの書き込み: 録音の開始小節まで足りなければ休符と小節線で埋め、MODE が Mix で
;;     データがあれば開始小節から時刻順に混ぜ (trk_mix_sub)、Rec なら開始小節〜終わりの小節を置き換える。
;;     最後にタイ圧縮 (tai_compres)。
;; 原作との違い【設計判断】:
;;   - アフタータッチ: 原作の RECORD.C はキーごと (A0) を 0xEA、チャンネル (D0) を 0xED にしているが、
;;     VISUAL.C (0xEA = AFTER CH.、0xED = AFTER KEY) と再生の解釈に合わせて、A0 → [ED 0 キー 値]、D0 → [EA 0 値 0]。
;;   - GT が 240 を超える音: 原作は最初に GT 255 の音を入れ、さらに同じ時刻からつなぎの音を入れる (同じ音が
;;     2 つ重なる)。ここではつなぎの音だけにする。
;;   - フィルタ (rfilt) は無し (全部入れる)。
(ns polysted.record
  (:require [polysted.edit]))

(defn- note-off? [[s k v] ch key]
  (and (= ch (bit-and s 0x0F)) (= k key)
       (or (= 0x80 (bit-and s 0xF0)) (and (= 0x90 (bit-and s 0xF0)) (zero? v)))))

(defn midi->records
  "時刻付きの MIDI 入力 → 4 バイトのレコード列 (proc_next)。
   events: 時刻順の [{:tick t :bytes [ステータス データ…]}] (エクスクルーシブは [F0 … F7])。t は録音の始まりからの tick。
   opts: {:ch 0〜15 :bar-len 1 小節の ST :end 終わりの tick :sysex? エクスクルーシブも入れるか}"
  [events {:keys [ch bar-len end sysex?]}]
  (let [events (vec events)
        out (volatile! []) held (volatile! []) barunit (volatile! 0) exc (volatile! [0x10 0x16])
        entry! (fn [r] (vswap! out conj r))
        flash! (fn [dt]
                 (loop [dt dt]
                   (when (pos? dt)
                     (let [dx (min dt 0xF0)
                           dx (if (> (+ dx @barunit) bar-len) (- bar-len @barunit) dx)]
                       ;; つないでいる長い音
                       (vreset! held (reduce (fn [acc [nt vl gt]]
                                               (if (<= gt 0xF0)
                                                 (do (entry! [nt 0 gt vl]) acc)
                                                 (do (entry! [nt 0 (inc dx) vl]) (conj acc [nt vl (- gt dx)]))))
                                             [] @held))
                       ;; 時間を直前の行の ST に足す (足せなければ休符)
                       (let [o @out n (count o) l (peek o)]
                         (cond
                           (and l (not= 0xDE (l 0)) (< (l 0) 0xF0) (<= (+ (l 1) dx) 0xF0))
                           (vswap! out update-in [(dec n) 1] + dx)
                           (and l (= 0xF7 (l 0)))
                           (let [cc (loop [c (dec n)] (if (and (pos? c) (= 0xF7 ((o c) 0))) (recur (dec c)) c))]
                             (if (<= (+ ((o cc) 1) dx) 0xF0)
                               (vswap! out update-in [cc 1] + dx)
                               (entry! [0 dx 0 0])))
                           :else (entry! [0 dx 0 0])))
                       (vswap! barunit + dx)
                       (when (= @barunit bar-len) (entry! [0xFD 0 0 0]) (vreset! barunit 0))
                       (recur (- dt dx))))))
        excl! (fn [text]                                      ; text は F0 の後ろから F7 まで
                (let [t (vec text) len (count t)]
                  (if (and (<= 9 len 12) (= 0x41 (t 0)) (= 0x12 (t 3)) (= 0xF7 (peek t)))
                    (do (when (not= [(t 1) (t 2)] @exc)
                          (vreset! exc [(t 1) (t 2)]) (entry! [0xDF 0 (t 1) (t 2)]))
                        (loop [i 7 h (t 4) m (t 5) l (t 6) hm nil]
                          (when (and (< (inc i) len) (not= 0xF7 (t i)) (not= 0xF7 (t (inc i))))
                            (let [hm (if (= hm [h m]) hm (do (entry! [0xDD 0 h m]) [h m]))]
                              (entry! [0xDE 0 l (t i)])
                              (if (= l 0x7F) (recur (inc i) h (inc m) 0 hm) (recur (inc i) h m (inc l) hm))))))
                    (when-not (and (= 0x41 (get t 0)) (= 0x12 (get t 3)) (< len 9))
                      (entry! [0x98 0 0 0])
                      (loop [i 0]
                        (when (< i len)
                          (let [a (t i) b (get t (inc i) 0xF7)]
                            (entry! [0xF7 0 a b])
                            (when-not (or (= a 0xF7) (= b 0xF7)) (recur (+ i 2))))))))))]
    (loop [k 0 prev 0 stepsum 0]
      (if (>= k (count events))
        (do (flash! (+ stepsum (max 0 (- (or end prev) prev))))
            (doseq [[nt vl gt] @held] (entry! [nt 0 (min gt 255) vl]))
            @out)
        (let [{:keys [tick bytes]} (events k)
              stepsum (+ stepsum (- tick prev))
              [s d1 d2] bytes
              cmd (bit-and s 0xF0)
              mine? (and (< s 0xF0) (= ch (bit-and s 0x0F)))
              flushed (fn [] (flash! stepsum) 0)]
          (recur (inc k) tick
                 (cond
                   (and mine? (= cmd 0x90))
                   (do (flash! stepsum)
                       (when (pos? d2)
                         (when-let [off (some (fn [j] (when (note-off? (:bytes (events j)) ch d1) (events j))) (range (inc k) (count events)))]
                           (let [gt (- (:tick off) tick)]
                             (when (pos? gt)
                               (if (<= gt 0xF0) (entry! [d1 0 gt d2]) (vswap! held conj [d1 d2 gt]))))))
                       0)
                   (and mine? (= cmd 0xA0)) (do (flushed) (entry! [0xED 0 d1 d2]) 0)
                   (and mine? (= cmd 0xB0)) (do (flushed) (entry! [0xEB 0 d1 d2]) 0)
                   (and mine? (= cmd 0xC0)) (do (flushed) (entry! [0xEC 0 d1 0]) 0)
                   (and mine? (= cmd 0xD0)) (do (flushed) (entry! [0xEA 0 d1 0]) 0)
                   (and mine? (= cmd 0xE0)) (do (flushed) (entry! [0xEE 0 d1 d2]) 0)
                   (and sysex? (= s 0xF0)) (do (flushed) (excl! (rest bytes)) 0)
                   :else stepsum)))))))

;; ---- トラックへの書き込み (rec_ext・meas_adjust) ----
(defn- rest-and-bar "ST st の休符 (240 ずつ) と小節線 (meas_adj_sub)" [st]
  (conj (vec (concat (repeat (quot (dec st) 240) [0 240 0 0]) [[0 (- st (* 240 (quot (dec st) 240))) 0 0]])) [0xFD 0 0 0]))

(defn- pad-to-measure
  "小節 top の頭までレコードが足りなければ、休符と小節線で埋める (meas_adjust)。=> [レコード 小節 top の頭の位置]"
  [recs top bar-len]
  (let [recs (vec recs)]
    (if-let [ad (and (> top 1) (polysted.edit/meas-add recs top))]
      [recs ad]
      (if (<= top 1)
        [recs 0]
        (let [n (count recs)
              last-bar (or (last (filter #(> ((recs %) 0) 0xFB) (range n))) -1)
              partial (reduce + (map #(if (< ((recs %) 0) 0xF0) ((recs %) 1) 0) (range (inc last-bar) n)))
              lft (- top (polysted.edit/meas-no recs n))
              [pad lft] (if (pos? partial) [(rest-and-bar (max 1 (- bar-len partial))) (dec lft)] [[] lft])
              pad (into pad (mapcat (fn [_] (rest-and-bar bar-len)) (range (max 0 lft))))
              recs (polysted.edit/renumber-measures (into recs pad))]
          [recs (or (polysted.edit/meas-add recs top) (count recs))])))))

(defn apply-recording
  "録音したレコードをトラックのレコードに書き込む (rec_ext)。opts: {:mix? MODE が Mix か :top 開始小節 :btm 終わりの小節
   :bar-len 1 小節の ST :ch トラックのチャンネル (タイ圧縮用)}"
  [recs new-recs {:keys [mix? top btm bar-len ch]}]
  (let [[recs ad] (pad-to-measure recs (or top 1) bar-len)
        merged (if (and mix? (seq recs))
                 (into (subvec recs 0 ad) (polysted.edit/mix-records (subvec recs ad) new-recs))
                 (let [ad2 (or (polysted.edit/meas-add recs (inc (or btm 999))) (count recs))]
                   (vec (concat (subvec recs 0 ad) new-recs (subvec recs (max ad ad2))))))
        {r :recs} (polysted.edit/tie-compress merged ch ad (count merged))]
    (polysted.edit/renumber-measures r)))

;; ---- 簡易チェック (期待値は RECORD.C を手で追って求めた) ----
(defn run-checks []
  (let [ev (fn [t & bs] {:tick t :bytes (vec bs)})
        checks
        [["ノート (和音・ノートオフ・ベロシティ 0)、コントロール、小節線をまたぐ時間、ほかのチャンネルは入れない"
          (midi->records [(ev 0 0x90 60 100) (ev 24 0x80 60 0) (ev 48 0x90 64 90) (ev 48 0x90 67 80) (ev 96 0x90 64 0)
                          (ev 100 0xB0 7 100) (ev 144 0x80 67 0) (ev 200 0xC0 5) (ev 210 0x91 60 100)]
                         {:ch 0 :bar-len 192 :end 300})
          [[60 48 24 100] [64 0 48 90] [67 52 96 80] [0xEB 92 7 100] [0xFD 0 0 0] [0 8 0 0] [0xEC 100 5 0]]]
         ["GT が 240 を超える音は小節ごとにつなぐ (GT = ST + 1)"
          (midi->records [(ev 0 0x90 60 100) (ev 500 0x80 60 0)] {:ch 0 :bar-len 192 :end 500})
          [[60 192 193 100] [0xFD 0 0 0] [60 192 193 100] [0xFD 0 0 0] [60 116 116 100]]]
         ["エクスクルーシブ: Roland の DT1 は DF DD DE、ほかは 98 と F7 の行、sysex? でなければ入れない"
          [(midi->records [(ev 0 0xF0 0x41 0x10 0x42 0x12 0x40 0x00 0x7F 0x00 0x41 0xF7)] {:ch 0 :bar-len 192 :end 0 :sysex? true})
           (midi->records [(ev 0 0xF0 0x43 0x10 0x4C 0x00 0x00 0x7E 0x00 0xF7)] {:ch 0 :bar-len 192 :end 0 :sysex? true})
           (midi->records [(ev 0 0xF0 0x43 0x10 0xF7)] {:ch 0 :bar-len 192 :end 0})]
          [[[0xDF 0 0x10 0x42] [0xDD 0 0x40 0x00] [0xDE 0 0x7F 0x00]]
           [[0x98 0 0 0] [0xF7 0 0x43 0x10] [0xF7 0 0x4C 0x00] [0xF7 0 0x00 0x7E] [0xF7 0 0x00 0xF7]]
           []]]
         ["書き込み: Rec は開始〜終わりの小節を置き換え、Mix は時刻順に混ぜる、足りない小節は休符で埋める"
          (let [recs [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [64 48 46 64] [0xFD 0 0 3]]
                nw [[67 24 20 100] [69 24 20 100] [0xFD 0 0 0]]]
            [(apply-recording recs nw {:top 2 :btm 2 :bar-len 48 :ch 1})
             (apply-recording recs nw {:mix? true :top 3 :bar-len 48 :ch 1})
             (apply-recording [[60 48 46 64] [0xFD 0 0 1]] [[67 48 46 100] [0xFD 0 0 0]] {:top 3 :bar-len 48 :ch 1})])
          [[[60 48 46 64] [0xFD 0 0 1] [67 24 20 100] [69 24 20 100] [0xFD 0 0 2] [64 48 46 64] [0xFD 0 0 3]]
           [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [64 0 46 64] [67 24 20 100] [69 24 20 100] [0xFD 0 0 3]]
           [[60 48 46 64] [0xFD 0 0 1] [0 48 0 0] [0xFD 0 0 2] [67 48 46 100] [0xFD 0 0 3]]]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted.record: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

