;; polysted_edit.clj -- STED2 編集コア (TRACK.C の trk_opt_sub / tai_compres の移植・動作未確認)
;;
;; データ: 「レコード」= [b0 b1 b2 b3] の4整数ベクタ (STED2 の4バイトレコードそのもの)
;;   ノート [キー ST GT vel] / 休符 [0 ST 0 0] / コマンド [コード ST b2 b3] ...
;; 使い方:
;;   (load-file "polysted.clj") (load-file "polysted_rcp.clj") (load-file "polysted_edit.clj")
;;   ロード時に簡易チェック (run-checks) が走ります。
(ns polysted-edit
  (:require [polysted] [polysted-rcp]))

;; polysted-rcp の非公開関数 decode-record を借りる (レコード→イベント)
(def ^:private decode (deref #'polysted-rcp/decode-record))

(defn- note-rec? [r] (< (r 0) 0x80))
(defn- silent-rec? [r] (and (note-rec? r) (or (zero? (r 2)) (zero? (r 3)))))
(defn- add-st [r n] (assoc r 1 (+ (r 1) n)))

;; ------------------------------------------------------------
;; 挿入・削除・置換 (範囲は [from, to) のレコード番号)
;; ------------------------------------------------------------
(defn insert-records [recs idx new-recs]
  (vec (concat (subvec recs 0 idx) new-recs (subvec recs idx))))

(defn delete-records [recs from to]
  (vec (concat (subvec recs 0 from) (subvec recs to))))

(defn update-records
  "範囲内で (pred rec) が真のレコードに (f rec) を適用する。置換マクロの素朴版。"
  [recs pred f from to]
  (vec (map-indexed (fn [i r] (if (and (>= i from) (< i to) (pred r)) (f r) r)) recs)))

;; ------------------------------------------------------------
;; 小節線 (EDIT.C の '*' キー / EDITS.C の meas_no・same_shift・trk_cluc)
;; ------------------------------------------------------------
(defn meas-no
  "idx 番のレコードを含む小節の番号 (1始まり)。meas_no: 手前の 0xFC 以上と、SAME の直前で数える"
  [recs idx]
  (inc (count (filter (fn [j] (or (>= ((recs j) 0) 0xFC)
                                  (= (get-in recs [(inc j) 0]) 0xFC)))
                      (range idx)))))

(defn renumber-measures
  "小節線 0xFD に小節番号を振り直す (trk_cluc。byte2=上位, byte3=下位)"
  [recs]
  (loop [i 0 meas 1 out (transient (into [] recs))]   ; subvec は transient にできないので通常のベクタに
    (if (>= i (count recs))
      (persistent! out)
      (let [a ((recs i) 0)]
        (cond
          (= a 0xFC) (recur (inc i)
                            (+ meas 1 (if (and (pos? i) (< ((recs (dec i)) 0) 0xFC)) 1 0))
                            out)
          (= a 0xFD) (recur (inc i) (inc meas)
                            (assoc! out i (assoc (recs i) 2 (bit-shift-right meas 8) 3 (bit-and meas 0xFF))))
          :else      (recur (inc i) meas out))))))

(defn meas-add
  "小節 me (1始まり) の先頭のレコード番号 (meas_add)。無ければ nil"
  [recs me]
  (if (<= me 1)
    0
    (loop [j 0 co 1]
      (when (< j (dec (count recs)))
        (let [b?  (or (>= ((recs j) 0) 0xFC) (= (get-in recs [(inc j) 0]) 0xFC))
              co' (if b? (inc co) co)]
          (if (and b? (= co' me)) (inc j) (recur (inc j) co')))))))

(defn same-ref
  "SAME レコードが指す小節 (0始まり)"
  [r]
  (+ (r 1) (* 256 (bit-and (r 2) 3))))

(defn shift-same-refs
  "same_shift の挿入時: idx に cu 小節ぶん挿入するとき、idx 以降の SAME の参照をずらす。
   参照が 1024 を超える場合の展開は省略 (1023 で止める)"
  [recs idx cu]
  (let [cur  (get recs idx)
        prev (when (pos? idx) (recs (dec idx)))
        me   (cond-> (dec (meas-no recs idx))
               (and prev (< (prev 0) 0xFC) cur (< (cur 0) 0xFC)) inc
               (and cur (> (cur 0) 0xFC)) inc)]
    (vec (map-indexed
          (fn [i r]
            (if (and (>= i idx) (= (r 0) 0xFC))
              (let [smn (same-ref r)
                    smn (if (>= smn me) (min 1023 (+ smn cu)) smn)]
                (assoc r 1 (bit-and smn 0xFF) 2 (bit-shift-right smn 8)))
              r))
          recs))))

(defn insert-line
  "idx の位置に1行 (rec) を挿入する。STED2 と同じく、小節の数が変わる場合は後ろの SAME の参照をずらす。
   - 小節線 0xFD ('*'): 「直前が 0xFC 未満で、その行が SAME」以外のとき1小節ずらす
   - SAME 0xFC ('^'): 小節の途中 (直前が 0xFC 未満で、その行が SAME でない) なら2小節、それ以外は1小節
   - その他 (line_ins_aft): 直前が 0xFC 以上で、その行が SAME のとき1小節
   小節線・SAME の挿入後は小節番号を振り直す (trk_cluc)。"
  [recs idx rec]
  (let [cur  (get recs idx)
        prev (when (pos? idx) (recs (dec idx)))
        p<fc (and prev (< (prev 0) 0xFC))
        cur= (and cur (= (cur 0) 0xFC))
        cu   (case (int (rec 0))
               0xFD (if (and p<fc cur=) 0 1)
               0xFC (if (and p<fc (not cur=)) 2 1)
               (if (and prev (not p<fc) cur=) 1 0))
        recs (if (pos? cu) (shift-same-refs recs idx cu) recs)
        recs (insert-records recs idx [rec])]
    (if (#{0xFD 0xFC} (rec 0)) (renumber-measures recs) recs)))

(defn insert-measure-line
  "idx の位置に小節線を挿入する ('*')"
  [recs idx]
  (insert-line recs idx [0xFD 0 0 0]))

;; ------------------------------------------------------------
;; 範囲のコピー・削除・貼り付け (EDIT.C の F7/F8/F9、TRACK.C same_ext_sub、EDITS.C same_shift)
;; ------------------------------------------------------------
(def ^:private bar-rec [0xFD 0 0 0])

(defn measure-content
  "小節 m0 (0始まり) の中身 (小節の先頭から 0xFC 以上の手前まで)"
  [recs m0]
  (when-let [st (meas-add recs (inc m0))]
    (vec (take-while #(< (% 0) 0xFC) (subvec recs st)))))

(defn buf-meas
  "レコード列に含まれる小節の区切りの数 (buf_meas: 0xFC 以上と、SAME の直前)"
  [buf]
  (count (filter (fn [i] (or (>= ((buf i) 0) 0xFC) (= (get-in buf [(inc i) 0]) 0xFC)))
                 (range (count buf)))))

(defn expand-same-range
  "[from, to) をコピーバッファ用に取り出す。SAME は指している小節の中身 + 小節線に展開する (same_ext_sub)。
   - SAME が自分の小節を指すときは小節線にする
   - 指す先も SAME なら、その先をたどる (先が自分自身を指す SAME ならそのまま)
   - SAME の直前で暗黙に終わる小節には、小節線を補う"
  [recs from to]
  (let [nmeas (meas-no recs (count recs))]
    (loop [i from out []]
      (if (>= i to)
        out
        (let [r   (recs i)
              out (if (and (= (r 0) 0xFC) (< (same-ref r) nmeas))
                    (loop [ref (same-ref r) depth 0]
                      (let [ad (meas-add recs (inc ref))]
                        (cond
                          (or (nil? ad) (> depth 16)) (conj out r)
                          (= ad i) (conj (if (and (seq out) (< ((peek out) 0) 0xFC)) (conj out bar-rec) out)
                                         (assoc r 0 0xFD))
                          (= ((recs ad) 0) 0xFC)
                          (let [ref2 (same-ref (recs ad))]
                            (if (= ad (meas-add recs (inc ref2))) (conj out r) (recur ref2 (inc depth))))
                          :else (conj (into out (take-while #(< (% 0) 0xFC) (subvec recs ad))) bar-rec))))
                    (conj out r))
              out (if (and (seq out) (< ((peek out) 0) 0xFC) (= (get-in recs [(inc i) 0]) 0xFC))
                    (conj out bar-rec)
                    out)]
          (recur (inc i) out))))))

(defn- set-same [r ref] (assoc r 1 (bit-and ref 0xFF) 2 (bit-shift-right ref 8)))

(defn- shift-sames-after-delete
  "same_shift の削除側: [lo, hi) を消して cu 小節減るとき、hi 以降の SAME を直す。
   消える小節を指す SAME はその場で中身に展開し、それより後ろの小節を指すものは cu 減らす"
  [recs lo hi cu]
  (let [cur (get recs lo) nxt (get recs (inc lo))
        me0 (dec (meas-no recs lo))
        me  (if (and cur nxt (> (cur 0) 0xFC) (> (nxt 0) 0xFC)) (inc me0) me0)
        me2 (+ me cu)]
    (into (subvec recs 0 hi)
          (mapcat (fn [i]
                    (let [r (recs i)]
                      (if (not= (r 0) 0xFC)
                        [r]
                        (let [ref (same-ref r)]
                          (cond
                            (and (<= me ref) (< ref me2))
                            (concat (when (< ((recs (dec i)) 0) 0xFC) [bar-rec])
                                    (measure-content recs ref) [bar-rec])
                            (>= ref me2) [(set-same r (- ref cu))]
                            :else [r]))))))
          (range hi (count recs)))))

(defn delete-range
  "[lo, hi) を削除してコピーバッファに移す (F7)。=> {:recs 削除後 :buf バッファ}
   範囲の先頭が「ノートの直後の SAME」なら、その SAME は小節線に変えて残す (原作どおり)"
  [recs lo hi]
  (let [buf  (expand-same-range recs lo hi)
        cu   (buf-meas buf)
        r2   (if (pos? cu) (shift-sames-after-delete recs lo hi cu) recs)
        sp?  (and (pos? lo) (< lo hi) (< ((r2 (dec lo)) 0) 0xFC) (= ((r2 lo) 0) 0xFC))
        r2   (if sp? (assoc r2 lo bar-rec) r2)
        lo'  (if sp? (inc lo) lo)]
    {:buf buf :recs (renumber-measures (delete-records r2 lo' hi))}))

(defn delete-line
  "idx の1行を削除する (DEL / BS / Ctrl-Y)。EDIT.C の規則で後ろの SAME の参照を直す:
   消す行が SAME、小節線 (ただしノートと SAME の間のものは除く)、
   または SAME の直前で1行だけの小節のとき、1小節 (ノートの直後の SAME を消すときは2小節) ずらす"
  [recs idx]
  (if-let [r (get recs idx)]
    (let [da (when (pos? idx) ((recs (dec idx)) 0))
          db (or (get-in recs [(inc idx) 0]) 0xFE)
          dc (r 0)
          shift? (or (= dc 0xFC)
                     (and (= dc 0xFD) (or (nil? da) (not (and (< da 0xFC) (= db 0xFC)))))
                     (and (< dc 0xFC) (or (nil? da) (>= da 0xFC)) (= db 0xFC)))
          cu (if (and (= dc 0xFC) da (< da 0xFC) (< db 0xFC)) 2 1)
          recs (if shift? (shift-sames-after-delete recs idx (inc idx) cu) recs)]
      (renumber-measures (delete-records recs idx (inc idx))))
    recs))

(defn reverse-records
  "反転貼付用にレコード列の前後を入れ替える (EDIT.C dat_rev)。
   ST が 0 でない音と、その手前の ST=0 の音 (和音) は1かたまりのまま並べ替える。
   リピートの開始 0xF9 と終了 0xF8 は入れ替える。エクスクルーシブ (0x98/0xF6 + 0xF7) は1かたまり。
   原作との違い【設計判断】: かたまりに含める手前の行はノート・コントローラ (0xF0 未満) だけにする
   (原作は ST=0 のリピート開始なども含めてしまう)。リピート回数は入れ替え後の終了側に移す
   (原作はバイトをそのまま残すので回数が開始側に残る)。"
  [buf]
  (let [buf (vec buf)
        out (loop [po (dec (count buf)) out []]
              (if (neg? po)
                out
                (let [[a b] (buf po)]
                  (cond
                    (#{0xF7 0xF6} a)              ; エクスクルーシブの続きは先頭までまとめる
                    (let [st (loop [i po] (if (and (pos? i) (#{0xF7 0xF6 0x98} ((buf i) 0))
                                                   (#{0xF7} ((buf i) 0)))
                                            (recur (dec i)) i))]
                      (recur (dec st) (into out (subvec buf st (inc po)))))
                    (>= a 0xF0)
                    (recur (dec po) (conj out (if (#{0xF8 0xF9} a) (assoc (buf po) 0 (- 497 a)) (buf po))))
                    (pos? b)
                    (let [st (loop [i po] (if (and (pos? i) (< ((buf (dec i)) 0) 0xF0) (zero? ((buf (dec i)) 1)))
                                            (recur (dec i)) i))]
                      (recur (dec st) (into out (subvec buf st (inc po)))))
                    :else (recur (dec po) (conj out (buf po)))))))]
    ;; リピート回数を、入れ替え後に対応する終了 (0xF8) へ移す
    (loop [i 0 stack [] out out]
      (if (>= i (count out))
        out
        (let [r (out i)]
          (case (int (r 0))
            0xF9 (recur (inc i) (conj stack (r 1)) (assoc out i (assoc r 1 0)))
            0xF8 (recur (inc i) (if (seq stack) (pop stack) stack)
                        (assoc out i (assoc r 1 (if (seq stack) (peek stack) (r 1)))))
            (recur (inc i) stack out)))))))

(defn mix-records
  "ミックス貼付 (Shift+F8) 用に、トラック in1 (カーソル以降) とバッファ in2 を時刻順に混ぜる (TRACK.C trk_mix_sub)。
   各イベントは ST=0 で並べ、時間の進みは直前のイベントの ST に足す (240 を超えるなら休符を足す)。
   休符そのものは出さない。小節線は in1 のものだけを使い、同じ時刻のイベントより前に置く。
   どちらかが終わったら、残りをそのまま続ける。"
  [in1 in2]
  (let [in1 (conj (vec in1) [0xFE 0 0 0])
        in2 (conj (vec in2) [0xFE 0 0 0])
        out (volatile! []) lsad (volatile! -1)
        st1 (volatile! 0) st2 (volatile! 0) ef1 (volatile! false) ef2 (volatile! false)
        ad1 (volatile! 0) ad2 (volatile! 0)
        add-time! (fn [st]
                    (let [o @out l @lsad]
                      (if (and (>= l 0) (< ((o l) 0) 0xF0) (<= (+ ((o l) 1) st) 240))
                        (vswap! out update-in [l 1] + st)
                        (do (vreset! lsad (count o)) (vswap! out conj [0 st 0 0])))))
        emit! (fn [r] (vswap! out conj r))
        excl! (fn [in ad r]                     ; 0x98/0xF6 と続きの 0xF7
                (vreset! lsad (count @out))
                (emit! [(r 0) 0 (r 2) (r 3)])
                (while (= ((in @ad) 0) 0xF7) (emit! (in @ad)) (vswap! ad inc)))
        read-1! (fn []
                  (let [[a b c d :as r] (in1 @ad1)]
                    (vswap! ad1 inc)
                    (cond
                      (#{0x98 0xF6} a) (excl! in1 ad1 r)
                      (= a 0xFE) (vreset! ef1 true)
                      (< a 0xF0) (when (or (>= a 0x80) (and (pos? c) (pos? d)))
                                   (vreset! lsad (count @out)) (emit! [a 0 c d]))
                      (and (= a 0xFD) (seq @out))
                      (let [o @out p (peek o)]
                        (if (and (< (p 0) 0xF0) (zero? (p 1)))
                          (do (vreset! out (conj (pop o) [0xFD 0 0 0] p)) (vreset! lsad (dec (count @out))))
                          (do (emit! [0xFD 0 0 0]) (vreset! lsad -1))))
                      :else (do (vreset! lsad -1) (emit! r)))
                    (when (< a 0xF0) (vswap! st1 + b))))
        read-2! (fn []
                  (let [[a b c d :as r] (in2 @ad2)]
                    (vswap! ad2 inc)
                    (cond
                      (#{0x98 0xF6} a) (excl! in2 ad2 r)
                      (= a 0xFE) (vreset! ef2 true)
                      (< a 0xF0) (when (or (>= a 0x80) (and (pos? c) (pos? d)))
                                   (vreset! lsad (count @out)) (emit! [a 0 c d]))
                      (< a 0xFD) (do (vreset! lsad -1) (emit! r))
                      :else nil)                        ; バッファの小節線は使わない
                    (when (< a 0xF0) (vswap! st2 + b))))]
    (loop [guard 0]
      (let [s1 @st1 s2 @st2]
        (cond
          (= s1 s2) (when (pos? s1) (add-time! s1) (vreset! st1 0) (vreset! st2 0))
          (> s1 s2) (when (pos? s2) (add-time! s2) (vswap! st1 - s2) (vreset! st2 0))
          :else     (when (pos? s1) (add-time! s1) (vswap! st2 - s1) (vreset! st1 0))))
      (cond
        (or (and @ef1 @ef2) (> guard 1000000)) @out
        @ef2 (do (when (pos? @st1) (add-time! @st1))
                 (into @out (take-while #(< (% 0) 0xFE) (subvec in1 @ad1))))
        @ef1 (do (when (pos? @st2) (add-time! @st2))
                 (into @out (take-while #(< (% 0) 0xFE) (subvec in2 @ad2))))
        :else (do (when (zero? @st1) (read-1!))
                  (when (zero? @st2) (read-2!))
                  (recur (inc guard)))))))

;; ------------------------------------------------------------
;; クオンタイズ (TRACK.C trk_quan)
;; ------------------------------------------------------------
(defn quantize
  "[from, to) のノートの発音位置を解像度 res (2〜48) の格子へ rate% (50〜100) 寄せる (trk_quan)。
   格子からのずれ tt1 が res/2 未満 (または res=2) なら前へ、それ以外は後ろへ。
   寄せる分は手前の行 (小節の区切りまで) の ST を増減して作り、ずれた分 (ofs) は
   次の ST>0 のノートの ST で取り戻すので、後ろの位置は変わらない。
   小節の区切り (0xFC 以上) と範囲の終わりで、残った ofs を手前の行の ST で精算する。"
  [recs from to res rate]
  (let [hf  (bit-shift-right res 1)
        out (volatile! [])
        tt1 (volatile! 0) ofs (volatile! 0)
        ;; out の ad3 より手前を後ろへたどりながら ST を動かす (f が false を返したら止める)
        walk-back! (fn [start f]
                     (loop [ad3 start]
                       (when (pos? ad3)
                         (let [ad3 (dec ad3) r (@out ad3)]
                           (when (< (r 0) 0xFC)
                             (if (< (r 0) 0xF0)
                               (when (f ad3) (recur ad3))
                               (recur ad3)))))))
        bump! (fn [ad3 d] (vswap! out update-in [ad3 1] + d))]
    (doseq [ad (range (count recs))]
      (let [[a b c d] (recs ad)
            b (volatile! b)]
        (when (and (>= ad from) (< ad to) (< a 0xF0))
          (when (< a 0x80)
            (while (>= @tt1 res) (vswap! tt1 - res))
            (when (pos? @tt1)
              (let [vt (volatile! (if (or (< @tt1 hf) (= res 2))
                                    (- (quot (* @tt1 rate) 100))
                                    (quot (* (- res @tt1) rate) 100)))]
                (walk-back! (count @out)
                            (fn [ad3]
                              (while (and (neg? @vt) (pos? ((@out ad3) 1)))
                                (bump! ad3 -1) (vswap! vt inc) (vswap! ofs inc) (vswap! tt1 dec))
                              (while (and (pos? @vt) (< ((@out ad3) 1) 255))
                                (bump! ad3 1) (vswap! vt dec) (vswap! ofs dec) (vswap! tt1 inc))
                              (and (not (zero? @vt)) (< ((@out ad3) 0) 0x80)))))))
          (vswap! tt1 + @b)
          (when (and (< a 0x80) (pos? @b))
            (while (and (pos? @ofs) (< @b 255)) (vswap! b inc) (vswap! tt1 inc) (vswap! ofs dec))
            (while (and (neg? @ofs) (pos? @b)) (vswap! b dec) (vswap! tt1 dec) (vswap! ofs inc))))
        (vswap! out conj [a @b c d])
        (when (and (or (>= a 0xFC) (= (inc ad) to)) (not (zero? @ofs)))
          (walk-back! (dec (count @out))
                      (fn [ad3]
                        (while (and (neg? @ofs) (pos? ((@out ad3) 1))) (bump! ad3 -1) (vswap! ofs inc))
                        (while (and (pos? @ofs) (< ((@out ad3) 1) 255)) (bump! ad3 1) (vswap! ofs dec))
                        (not (zero? @ofs)))))))
    @out))

;; ------------------------------------------------------------
;; ノート操作 (Shift+F1: TRACK.C note_ctrl・velo_ctrl、VISUAL.C bunkatsu)
;; ------------------------------------------------------------
(defn- sounding? [r] (and (< (r 0) 0x80) (pos? (r 2)) (pos? (r 3))))

(defn bunkatsu
  "st を co 個の整数に分ける (合計 st)"
  [st co]
  (mapv (fn [i] (- (quot (* st i) co) (quot (* st (dec i)) co))) (range 1 (inc co))))

(defn chord-groups
  "[from, to) の和音のかたまり [先頭 末尾] (どちらも行番号、2音以上)。
   ST が thr 以下のノートが続き、ST が thr より大きいノート (または範囲の最後) で終わる。
   ノート以外の行で途切れたものは対象外【設計判断】"
  [recs from to thr]
  (loop [i from start nil ln 0 out []]
    (if (>= i to)
      out
      (let [r (recs i)]
        (if (>= (r 0) 0x80)
          (recur (inc i) nil 0 out)
          (let [start (or start i) ln (inc ln)]
            (if (or (> (r 1) thr) (= i (dec to)))
              (recur (inc i) nil 0 (if (> ln 1) (conj out [start i]) out))
              (recur (inc i) start ln out))))))))

(defn sort-chords
  "和音の音を低い順 (up? true) / 高い順に並べ替える (U/D)。最後の音の ST は最後の位置に残す"
  [recs from to up?]
  (reduce (fn [recs [a b]]
            (let [last-st ((recs b) 1)
                  grp (mapv #(assoc (recs %) 1 0) (range a (inc b)))
                  grp (vec (sort-by identity (if up? compare #(compare %2 %1)) grp))
                  grp (assoc-in grp [(dec (count grp)) 1] last-st)]
              (into (subvec recs 0 a) (concat grp (subvec recs (inc b))))))
          (vec recs) (chord-groups recs from to 0)))

(defn strum-chords
  "和音をずらす (G: ST と GT、S: ST だけ)。st を音の数−1 に分け、各音の発音を順に遅らせ、
   最後の音の ST から引く (かたまり全体の長さは同じ)。G は各音の GT を遅らせた分だけ短くする。
   ST が 2 以下の音までを和音とみなす (原作どおり)"
  [recs from to st gt?]
  (reduce (fn [recs [a b]]
            (let [ln   (inc (- b a))
                  stt  (conj (bunkatsu st (dec ln)) 0)
                  stt2 (volatile! (into [0] (bunkatsu st (dec ln))))
                  out  (volatile! recs)]
              (dotimes [j ln]
                (let [ii (+ a j) r (@out ii) lst ((@out b) 1) d (stt j)]
                  (when (and (<= (+ (r 1) d) 240)
                             (or (not gt?) (> (r 2) (@stt2 j)))
                             (> lst d))
                    (vswap! out update-in [ii 1] + d)
                    (when gt? (vswap! out update-in [ii 2] - (@stt2 j)))
                    (vswap! out update-in [b 1] - d)
                    (when (< (inc j) (count @stt2))
                      (vswap! stt2 update (inc j) + (@stt2 j))))))
              @out))
          (vec recs) (chord-groups recs from to 2)))

(defn velocity-ctrl
  "ベロシティ・ST/GT の一括設定 (velo_ctrl)。対象は [from, to) の鳴るノート。
   :line   lo→hi へ直線   :chord 和音の中で lo→hi (単音は hi)   :random lo〜hi
   :offset 今の値に lo〜hi を足す (1〜127)   :stgt ST と GT に lo〜hi を足す (GT が 0 でない音)
   rng: java.util.Random。原作との違い【設計判断】: 2 音から動く (原作は 3 音以上)。
   lo=hi のとき原作はどのモードでもベロシティを lo にするが、:offset/:stgt は lo を足す"
  [recs from to mode lo hi ^java.util.Random rng]
  (let [recs  (vec recs)
        idx   (filterv #(sounding? (recs %)) (range from to))
        ct    (dec (count idx))
        diff  (- hi lo)
        rnd   (fn [] (+ lo (if (pos? diff) (.nextInt rng (inc diff)) 0)))
        setv  (fn [rs i v] (assoc-in rs [i 3] (max 1 (min 127 v))))]
    (if (< ct 1)
      recs
      (case mode
        :line   (reduce (fn [rs [c i]] (setv rs i (+ lo (quot (* diff c) ct)))) recs (map-indexed vector idx))
        :random (reduce (fn [rs i] (setv rs i (rnd))) recs idx)
        :offset (reduce (fn [rs i] (setv rs i (+ ((rs i) 3) (rnd)))) recs idx)
        :stgt   (reduce (fn [rs i]
                          (let [[_ s g] (rs i)]
                            (if (zero? g)
                              rs
                              (loop [n 0]
                                (let [f (rnd)]
                                  (cond (and (<= 0 (+ s f) 255) (<= 0 (+ g f) 255))
                                        (-> rs (assoc-in [i 1] (+ s f)) (assoc-in [i 2] (+ g f)))
                                        (> n 100) rs
                                        :else (recur (inc n))))))))
                        recs idx)
        :chord  (loop [k 0 rs recs]
                  (if (>= k (count idx))
                    rs
                    (let [i (idx k)
                          ;; この音から ST=0 で続く鳴るノートの数
                          ct2 (loop [j i n 0] (if (and (zero? ((rs j) 1)) (< (inc j) to) (sounding? (rs (inc j))))
                                                (recur (inc j) (inc n)) n))]
                      (if (pos? ct2)
                        (recur (+ k ct2 1)
                               (reduce (fn [rs c] (setv rs (+ i c) (+ lo (quot (* diff c) ct2)))) rs (range (inc ct2))))
                        (recur (inc k) (setv rs i hi))))))))))

;; ------------------------------------------------------------
;; SAME の自動変換と展開 (TRACK.C same_meas_conv、EDITS.C same_meas_extr)
;; ------------------------------------------------------------
(defn same-meas-conv
  "重複した小節を SAME に置き換える (原作の Shift+^)。=> {:recs 新しい列 :count 置き換えた小節数}
   - 鳴らないノート (GT=0 か VEL=0) は休符 [0 ST 0 0] にそろえる
   - 小節 (小節線までのレコード。SAME の直前で終わる小節はその SAME を含めない) を前の小節と比べ、
     同じものがあれば1行の SAME にする (小節線の番号は比べない。SAME で始まる小節とは比べない)
   - SAME を指す SAME は、その先を指すように直す"
  [recs]
  (let [recs   (mapv (fn [r] (if (and (< (r 0) 0x80) (or (zero? (r 2)) (zero? (r 3)))) [0 (r 1) 0 0] r)) recs)
        rf     (conj recs [0xFE 0xFE 0xFE 0xFE])
        starts (into [0] (keep (fn [i] (when (or (>= ((rf i) 0) 0xFC) (= (get-in rf [(inc i) 0]) 0xFC)) (inc i)))
                               (range (count rf))))
        m-max  (dec (count starts))
        meas   (fn [po] (loop [j po out []]
                          (let [r (get rf j)]
                            (cond (or (nil? r) (= (r 0) 0xFE)) out
                                  (> (r 0) 0xFB) (conj out r)
                                  :else (recur (inc j) (conj out r))))))
        cmp    (fn [ms] (mapv #(if (= (% 0) 0xFD) [0xFD] %) ms))
        [out ms cnt]
        (loop [i 0 out [] ms [] cnt 0]
          (if (>= i m-max)
            [out ms cnt]
            (let [m (meas (starts i))
                  m (if (and (> (count m) 1) (= ((peek m) 0) 0xFC)) (pop m) m)
                  j (when (and (seq m) (< ((first m) 0) 0xFC))
                      (some (fn [[j [_ pm]]] (when (and (= (count pm) (count m)) (seq pm) (< ((first pm) 0) 0xFC)
                                                        (= (cmp pm) (cmp m)))
                                               j))
                            (map-indexed vector ms)))]
              (if (and j (< j 1024))
                (recur (inc i) (conj out [0xFC (bit-and j 0xFF) (bit-shift-right j 8) 0]) (conj ms [(count out) [[0xFC 0 0 0]]]) (inc cnt))
                (recur (inc i) (into out m) (conj ms [(count out) m]) cnt)))))
        ;; SAME を指す SAME はその先へ
        out (mapv (fn [r] (if (= (r 0) 0xFC)
                            (let [[st _] (get ms (same-ref r))
                                  t (when st (get out st))]
                              (if (and t (= (t 0) 0xFC)) (assoc r 1 (t 1) 2 (t 2)) r))
                            r))
                  out)]
    {:recs (renumber-measures out) :count cnt}))

(defn same-meas-extract
  "[from, to) の SAME を中身に展開する (原作の Ctrl+^、same_meas_extr)。
   範囲の先頭が「ノートの直後の SAME」なら、先頭に小節線を補って小節の区切りを残す"
  [recs from to]
  (let [recs (vec recs)
        buf  (expand-same-range recs from to)
        buf  (if (and (pos? from) (< from to) (= ((recs from) 0) 0xFC) (< ((recs (dec from)) 0) 0xFC))
               (into [[0xFD 0 0 0]] buf)
               buf)]
    (renumber-measures (into (subvec recs 0 from) (concat buf (subvec recs to))))))

(defn shift-keys
  "ノートのキーを bias ずらす (0〜127 を外れる音はそのまま。replicate の BIAS)"
  [buf bias]
  (mapv (fn [r] (let [k (+ (r 0) bias)] (if (and (< (r 0) 0x80) (<= 0 k 127)) (assoc r 0 k) r))) buf))

(defn measure-range-buffer
  "小節 top〜end (1始まり) を SAME を展開して取り出し、最後が小節線でなければ小節線を足す
   (EDITS.C replicate の cpybuf_set)。小節が無ければ末尾まで"
  [recs top end]
  (let [n   (count recs)
        a   (or (meas-add recs top) n)
        b   (or (meas-add recs (inc end)) n)
        buf (if (< a b) (expand-same-range recs a b) [])]
    (if (and (seq buf) (not= ((peek buf) 0) 0xFD)) (conj buf [0xFD 0 0 0]) buf)))

;; ------------------------------------------------------------
;; ループ圧縮 (TRACK.C repeat_comp)
;; ------------------------------------------------------------
(defn repeat-compress
  "繰り返しをリピート [ … ]n に置き換える (repeat_comp)。=> {:recs :count 置き換えた数}
   0xF0 未満の行が続く区間ごとに、先頭から長さ blk 行のパターンが続けて何回現れるか (lpc) を数え、
   1 行なら 4 回以上、2 行なら 3 回以上、3 行以上なら 2 回以上で [F9] パターン [F8 lpc] にする。
   見つからなければ 1 行出して次の位置から探す。
   原作との違い【設計判断】: 回数は 254 まで (255 以上は無限ループの意味になるため)。保存前の dclr は省略"
  [recs]
  (let [recs (vec recs) n (count recs)
        lt-f0? (fn [i] (and (< i n) (< ((recs i) 0) 0xF0)))
        ;; 行 i から blk 行のパターンが何回続くか (原作は各バイトが 0xF0 以上でも不一致とする)
        reps (fn [i blk]
               (loop [k 1]
                 (let [ii (+ i (* k blk))]
                   (if (and (<= (+ ii blk) n)
                            (every? (fn [j] (let [p (recs (+ i j))]
                                              (and (every? #(< % 0xF0) p) (= p (recs (+ ii j))))))
                                    (range blk))
                            (< k 254))
                     (recur (inc k))
                     k))))
        ok? (fn [lpc blk] (or (> lpc 3) (and (> lpc 2) (> blk 1)) (and (> lpc 1) (> blk 2))))]
    (loop [i 0 out [] cnt 0]
      (if (>= i n)
        {:recs out :count cnt}
        (if-not (lt-f0? i)
          (recur (inc i) (conj out (recs i)) cnt)
          (let [ll (count (take-while lt-f0? (range i n)))]
            (if (<= ll 1)
              (recur (inc i) (conj out (recs i)) cnt)
              (let [hit (some (fn [blk] (let [lpc (reps i blk)] (when (ok? lpc blk) [blk lpc])))
                              (range 1 ll))]
                (if-let [[blk lpc] hit]
                  (recur (+ i (* blk lpc))
                         (-> out (conj [0xF9 0 0 0]) (into (subvec recs i (+ i blk))) (conj [0xF8 lpc 0 0]))
                         (inc cnt))
                  (recur (inc i) (conj out (recs i)) cnt))))))))))

;; ------------------------------------------------------------
;; 値の増減と反転 (EDIT.C val_add・val_sub・val_rev。キーは + . > と - , < と =)
;; ------------------------------------------------------------
(defn val-step
  "欄 cx の値を dir (+1 / -1) だけ変えたレコード。big? は > < (キー・ST・GT は 12、VEL・値は 10)。
   範囲を超えたら反対側へ回る (原作どおり)。same-limit はその行の小節番号 (SAME の参照の上限)"
  [[a _ c d :as r] cx dir big? same-limit]
  (cond
    (< a 0x80)
    (let [[m step] (case (int cx) 0 [128 (if big? 12 1)] 3 [128 (if big? 10 1)] [256 (if big? 12 1)])]
      (assoc r cx (mod (+ (r cx) (* dir step)) m)))

    (and (= a 0xFC) (pos? cx))
    (let [ct (+ (same-ref r) dir)]
      (if (and (<= 0 ct 1023) (< (inc ct) same-limit)) (assoc r 1 (bit-and ct 0xFF) 2 (bit-shift-right ct 8)) r))

    (and (or (< a 0xF0) (= a 0xF8) (= a 0xF5)) (pos? cx))
    (if (and (> cx 1) (= a 0xEE))
      (let [v (mod (+ c (* 128 d) (* dir (if big? 10 1))) 16384)]
        (assoc r 2 (bit-and v 0x7F) 3 (bit-shift-right v 7)))
      (let [cx (cond (and (> cx 1) (#{0xEC 0xE6} a)) 2 (= a 0xF5) 1 :else cx)
            i  (+ (r cx) (* dir (if big? (if (= cx 1) 12 10) 1)))
            i  (cond
                 (and (> cx 1) (= a 0xE6)) (cond (> i 32) (if (pos? dir) 0 32) (neg? i) 32 :else i)
                 (and (> cx 1) (< 0x7F a 0xF0) (not= a 0xE7)) (mod i 128)
                 (= a 0xF5) (mod i 32)
                 :else (mod i 256))]
        (assoc r cx i)))

    (zero? cx)
    (cond (<= 0x90 a 0x97) (assoc r 0 (+ 0x90 (mod (+ (- a 0x90) dir) 8)))
          (= a 0xEB) (assoc r 2 (mod (+ c dir) 128))
          :else r)
    :else r))

(defn val-rev
  "欄 cx の値を反転したレコード (=): ベンドの正負、調の長短、チャンネルの A/B、パン (CC10) の左右"
  [[a b c d :as r] cx]
  (cond
    (and (> cx 1) (= a 0xEE)) (let [v (mod (- 16384 (+ c (* 128 d))) 16384)] (assoc r 2 (bit-and v 0x7F) 3 (bit-shift-right v 7)))
    (and (pos? cx) (= a 0xF5)) (assoc r 1 (bit-xor b 16))
    (and (= cx 2) (= a 0xE6) (pos? c)) (assoc r 2 (if (> c 16) (- c 16) (+ c 16)))
    (and (= cx 3) (= a 0xEB) (= c 10)) (assoc r 3 (max 0 (min 127 (- 128 d))))
    :else r))

;; ------------------------------------------------------------
;; ワンタッチコード入力 (KEY_SUB.C chord_name・chord_pat・chord_ten・chord_ins)
;; ------------------------------------------------------------
(def chord-names
  ["MAJOR" "7" "maj7" "6" "maj9" "9" "7(b9)" "7(#9)" "dim" "dim7"
   "7(b5)" "m7(b5)" "aug" "aug7" "7sus4" "69" "m9" "m6" "m7" "m"])

(def chord-pats
  [[4 7] [4 7 10] [4 7 11] [4 7 9] [4 7 11 14] [4 7 10 14] [4 7 10 13] [4 7 10 15] [3 6 9] [3 6 10]
   [4 6 10] [3 6 10] [4 8] [4 8 10] [5 7 10] [4 7 9 14] [3 7 10 14] [3 7 9] [3 7 10] [3 7]])

(defn chord-keys "ルート root とコードの種類 no (0〜19) の構成音 (0〜127 のものだけ)" [root no]
  (into [root] (filter #(<= 0 % 127) (map #(+ root %) (chord-pats no)))))

(defn chord-rotate
  "転回: dir 1 は一番下の音を1オクターブ上げて上へ、-1 は一番上の音を1オクターブ下げて下へ (範囲外なら変えない)"
  [ks dir]
  (if (pos? dir)
    (let [d (+ (first ks) 12)] (if (<= d 127) (conj (vec (rest ks)) d) ks))
    (let [d (- (peek ks) 12)] (if (>= d 0) (into [d] (pop ks)) ks))))

(defn chord-records
  "ノートの行 r をコードに展開した行 (最後の音以外は ST=0、GT・VEL は元の音と同じ。chord_ins)"
  [[_ st gt vel] ks]
  (vec (map-indexed (fn [i k] [k (if (= i (dec (count ks))) st 0) gt vel]) ks)))

(declare paste-records)

(defn insert-times
  "idx にバッファを times 回挿入する (毎回、後ろの SAME の参照をずらす)"
  [recs idx buf times]
  (reduce (fn [rs _] (paste-records rs idx buf)) (vec recs) (range times)))

(defn paste-records
  "idx の位置にバッファを挿入する (F9)。増える小節の数だけ後ろの SAME の参照をずらす"
  [recs idx buf]
  (let [cu (buf-meas buf)
        recs (if (pos? cu) (shift-same-refs recs idx cu) recs)]
    (renumber-measures (insert-records recs idx buf))))

;; ------------------------------------------------------------
;; タイ圧縮 (tai_compres)
;;  GT が ST より長いノートの後に、同チャンネル・同キーのノートが続くとき、
;;  前のノートの GT に合算し(上限255)、後ろのノートを GT=0,vel=0 の休符にする。
;;  あわせて 冗長な E6(チャンネル変更) の除去と、休符の直前レコードへの結合を行う。
;; ------------------------------------------------------------
(defn- tie-scan [src n i ch]
  (let [r (src i)]
    (if-not (and (note-rec? r) (> (r 2) (r 1)) (pos? (r 3)))
      src
      (loop [j i, sum 0, c (r 2), cc ch, s src]
        (if (>= j n)
          s
          (let [x (s j)
                da (x 0)
                cc (if (= da 0xE6) (x 2) cc)
                hit? (and (> j i) (= cc ch) (= da (r 0)) (pos? (x 2)) (pos? (x 3)))
                over? (and hit? (> (+ sum (x 2)) 255))
                [s c] (if (and hit? (not over?))
                        (let [c' (+ (x 2) sum)]
                          [(-> s (assoc-in [i 2] c') (assoc-in [j 2] 0) (assoc-in [j 3] 0)) c'])
                        [s c])
                sum (if (< da 0xF0) (+ sum (x 1)) sum)]
            (if (or over?
                    (and (< da 0xF0) (or (> sum 255) (>= sum c)))
                    (>= da 0xFC) (= da 0xF8) (= da 0xF9) (= da 0xE2) (= da 0xEC))
              s
              (recur (inc j) sum c cc s))))))))

(defn tie-compress
  "戻り値: {:recs 圧縮後 :to 圧縮後の範囲終端}"
  [recs ch from to]
  (let [n (count recs)]
    (loop [i 0, src (vec recs), out [], ch (or ch 0), to' to]
      (if (>= i n)
        {:recs out :to (min to' (count out))}
        (if-not (and (>= i from) (< i to))
          (let [out (conj out (src i))]
            (recur (inc i) src out ch (if (= i (dec to)) (count out) to')))
          (let [src (tie-scan src n i ch)
                r (src i)
                ch (if (>= (r 0) 0xFC) -1 ch)
                [r out ch]
                (if (= (r 0) 0xE6)
                  (if (= (r 2) ch)
                    [[0 (r 1) 0 0] out ch]
                    (let [ch' (r 2)]
                      (if (and (seq out) (= ((peek out) 0) 0xE6))
                        [[0 (r 1) 0 0]
                         (assoc out (dec (count out)) (assoc (peek out) 2 ch'))
                         ch']
                        [r out ch'])))
                  [r out ch])
                merge? (and (> i from) (silent-rec? r) (seq out)
                            (< ((peek out) 0) 0xF0)
                            (<= (+ ((peek out) 1) (r 1)) 240))
                out (if merge?
                      (assoc out (dec (count out)) (add-st (peek out) (r 1)))
                      (conj out r))]
            (recur (inc i) src out ch (if (= i (dec to)) (count out) to'))))))))

;; ------------------------------------------------------------
;; 最適化 (trk_opt_sub)
;;  1. ST=0 の無音ノートを削除
;;  2. 直前と同じ ピッチベンド(0xEE)/CC(0xEB) は後ろを削除し ST を加算 (上限240)
;;     ※原実装は CC の番号条件が常に真になるため、全CCが対象。それをそのまま再現。
;;  3. 休符は直前レコードの ST に加算 (上限240)
;;  ※ エクスクルーシブ(0x98/0xF7)の特別扱いは未実装。
;; ------------------------------------------------------------
(defn- opt-sub [recs from to]
  (let [n (count recs)]
    (loop [i 0, out []]
      (if (>= i n)
        out
        (let [r (recs i)]
          (if-not (and (> i from) (< i to))
            (recur (inc i) (conj out r))
            (let [p (peek out)
                  p2 (when (>= (count out) 2) (nth out (- (count out) 2)))
                  after-from? (> (count out) from)]
              (cond
                (and (zero? (r 1)) (silent-rec? r))
                (recur (inc i) out)

                (and after-from? p
                     (or (= (p 0) 0xEE) (= (p 0) 0xEB))
                     (= (p 0) (r 0)) (= (p 2) (r 2)) (= (p 3) (r 3))
                     (<= (+ (p 1) (r 1)) 240))
                (recur (inc i) (assoc out (dec (count out)) (add-st p (r 1))))

                (and (silent-rec? r) after-from? p
                     (or (not= (p 0) 0xDE)
                         (and p2 (= (p2 0) 0xDE) (not= (inc (p2 2)) (p 2))))
                     (< (p 0) 0xF0)
                     (<= (+ (p 1) (r 1)) 240))
                (recur (inc i) (assoc out (dec (count out)) (add-st p (r 1))))

                :else (recur (inc i) (conj out r))))))))))

(defn optimize-records
  "ch: トラックのMIDIチャンネル(1始まり、OFFなら0)。範囲省略時は全体。"
  ([recs ch] (optimize-records recs ch 0 (count recs)))
  ([recs ch from to]
   (let [{r1 :recs to1 :to} (tie-compress recs ch from to)]
     (opt-sub r1 from to1))))

;; ------------------------------------------------------------
;; イベント(map) ⇔ レコード
;; ------------------------------------------------------------
(defn event->record [e]
  (let [st (:st e 0)]
    (case (:type e)
      :note [(:key e) st (:gt e) (:vel e)]
      :rest [0 st 0 0]
      :loop-start [0xF9 0 0 0]
      :loop-end [0xF8 (bit-and (:count e) 0xFF) 0 0]
      :bar [0xFD 0 0 0]
      :same [0xFC (bit-and (:bar e) 0xFF) (bit-and (bit-shift-right (:bar e) 8) 3) 0]
      :tempo [0xE7 st (or (:raw e) (:value e)) 0]
      :cc [0xEB st (:ctl e) (:value e)]
      :bend (let [v (+ 8192 (:value e))]
              [0xEE st (bit-and v 0x7F) (bit-and (bit-shift-right v 7) 0x7F)])
      :prog [0xEC st (:prog e) 0]
      :bank-prog [0xE2 st (:prog e) (:bank e)]
      :channel [0xE6 st (:ch e) 0]
      :chan-pressure [0xEA st (:value e) 0]
      :key-pressure [0xED st (:key e) (:value e)]
      :key-sig [0xF5 (:value e) 0 0]
      :raw [(:code e) st 0 0]          ; 注意: データ部は失われる
      nil)))

(defn events->records [es] (vec (keep event->record es)))
(defn records->events [rs] (mapv #(apply decode %) rs))

(defn optimize-track
  "トラック(map)を最適化。base-tempo(曲のヘッダテンポ)を渡すと E7 の :value を再計算する。"
  ([track] (optimize-track track nil))
  ([track base-tempo]
   (let [opt (optimize-records (events->records (:events track)) (or (:ch track) 0))
         evs (records->events opt)
         evs (if base-tempo
               (mapv (fn [e] (if (= :tempo (:type e))
                               (assoc e :value (polysted-rcp/tempo-bpm base-tempo (:raw e)))
                               e))
                     evs)
               evs)]
     (assoc track :events evs))))

;; ------------------------------------------------------------
;; 簡易チェック (期待値は TRACK.C のアルゴリズムを手で追って求めたもの)
;; ------------------------------------------------------------
(defn- check [label expected actual]
  (if (= expected actual)
    (println "OK " label)
    (do (println "NG " label)
        (println "    expected:" expected)
        (println "    actual:  " actual))))

(defn run-checks []
  (check "休符が直前のノートのSTに結合される"
         [[60 96 46 64] [62 48 46 64]]
         (optimize-records [[60 48 46 64] [0 24 0 0] [0 24 0 0] [62 48 46 64]] 1))
  (check "タイ: 同キーの後続ノートがGTに合算される"
         [[60 96 96 64] [62 48 46 64]]
         (optimize-records [[60 48 96 64] [60 48 48 64] [62 48 46 64]] 1))
  (check "同じCCが続けば STを加算して統合"
         [[0xEB 48 7 100] [60 48 46 64]]
         (optimize-records [[0xEB 0 7 100] [0xEB 24 7 100] [0xEB 24 7 100] [60 48 46 64]] 1))
  (check "ST=0 の無音ノートは消える"
         [[60 48 46 64] [62 48 46 64]]
         (optimize-records [[60 48 46 64] [61 0 0 0] [62 48 46 64]] 1))
  (check "MML → レコード → 最適化"
         [[60 96 46 64] [62 48 46 64]]
         (optimize-records (events->records (polysted/parse-mml "c4 r8 r8 d4" {})) 1))
  (check "挿入と削除"
         [[1 1 1 1] [9 9 9 9] [2 2 2 2]]
         (-> [[1 1 1 1] [2 2 2 2] [3 3 3 3]]
             (insert-records 1 [[9 9 9 9]])
             (delete-records 3 4)))
  (check "小節線の挿入: 番号を振り直す (byte2=上位 byte3=下位)"
         [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFE 0 0 0]]
         (insert-measure-line [[60 48 46 64] [62 48 46 64] [0xFD 0 0 0] [0xFE 0 0 0]] 1))
  (check "小節線の挿入: 後ろの小節を指す SAME (小節3=2) を +1、手前 (小節1=0) はそのまま"
         [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFD 0 0 3]
          [64 48 46 64] [0xFD 0 0 4] [0xFC 3 0 0] [0xFC 0 0 0]]
         (insert-measure-line [[60 48 46 64] [0xFD 0 0 0] [62 48 46 64] [0xFD 0 0 0]
                               [64 48 46 64] [0xFD 0 0 0] [0xFC 2 0 0] [0xFC 0 0 0]] 3))
  (check "小節線の挿入: 分割した小節そのもの (小節2=1) を指す SAME は変えない"
         [[0xFD 0 0 1] [60 48 46 64] [0xFD 0 0 2] [62 48 46 64] [0xFD 0 0 3] [0xFC 1 0 0] [0xFC 0 0 0]]
         (insert-measure-line [[0xFD 0 0 0] [60 48 46 64] [62 48 46 64] [0xFD 0 0 0] [0xFC 1 0 0] [0xFC 0 0 0]] 2))
  (check "SAME を小節の途中に挿入: 後ろの参照は2つずれる"
         [[60 48 46 64] [0xFC 0 0 0] [62 48 46 64] [0xFD 0 0 3] [0xFC 3 0 0]]
         (insert-line [[60 48 46 64] [62 48 46 64] [0xFD 0 0 0] [0xFC 1 0 0]] 1 [0xFC 0 0 0]))
  (check "SAME を小節の先頭に挿入: 後ろの参照は1つずれる"
         [[60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0] [62 48 46 64] [0xFD 0 0 3] [0xFC 2 0 0]]
         (insert-line [[60 48 46 64] [0xFD 0 0 0] [62 48 46 64] [0xFD 0 0 0] [0xFC 1 0 0]] 2 [0xFC 0 0 0]))
  (check "line_ins_aft: 小節線の直後の SAME の前にノートを入れると、新しい小節以降を指す参照がずれる"
         [[60 48 46 64] [0xFD 0 0 0] [64 48 46 64] [0xFC 0 0 0] [62 48 46 64] [0xFD 0 0 0] [0xFC 3 0 0]]
         (insert-line [[60 48 46 64] [0xFD 0 0 0] [0xFC 0 0 0] [62 48 46 64] [0xFD 0 0 0] [0xFC 2 0 0]]
                      2 [64 48 46 64]))
  (check "ふつうの位置にノートを入れても SAME はそのまま"
         [[62 48 46 64] [60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]]
         (insert-line [[60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]] 0 [62 48 46 64]))
  (check "meas-add: 小節の先頭のレコード番号"
         [0 2 3 nil]
         (mapv #(meas-add [[60 48 46 64] [0xFD 0 0 0] [62 48 46 64] [0xFC 0 0 0] [0xFE 0 0 0]] %) [1 2 3 9]))
  (check "コピー: SAME は中身 + 小節線に展開"
         [[62 48 46 64] [0xFD 0 0 2] [60 48 46 64] [0xFD 0 0 0]]
         (expand-same-range [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFC 0 0 0]] 2 5))
  (check "コピー: SAME の直前で終わる小節には小節線を補う"
         [[62 48 46 64] [0xFD 0 0 0]]
         (expand-same-range [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFC 0 0 0]] 2 3))
  (check "削除: 1小節消すと後ろの SAME の参照が1減り、消えた小節を指す SAME は展開される"
         [[60 48 46 64] [0xFD 0 0 1] [64 48 46 64] [0xFD 0 0 2] [0xFC 0 0 0] [62 48 46 64] [0xFD 0 0 4]]
         (:recs (delete-range [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2]
                               [64 48 46 64] [0xFD 0 0 3] [0xFC 0 0 0] [0xFC 1 0 0]] 2 4)))
  (check "削除: 小節の途中だけなら SAME はそのまま、バッファに入る"
         [[[60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]] [[62 48 46 64]]]
         ((juxt :recs :buf) (delete-range [[60 48 46 64] [62 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]] 1 2)))
  (check "貼り付け: 1小節ぶん貼ると後ろの SAME の参照が1増える"
         [[60 48 46 64] [0xFD 0 0 1] [64 48 46 64] [0xFD 0 0 2] [62 48 46 64] [0xFD 0 0 3] [0xFC 2 0 0]]
         (paste-records [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFC 1 0 0]]
                        2 [[64 48 46 64] [0xFD 0 0 0]]))
  (check "1行削除: 小節線を消すと後ろの SAME の参照が1減る"
         [[60 48 46 64] [62 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]]
         (delete-line [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFC 1 0 0]] 1))
  (check "1行削除: ふつうのノートでは SAME はそのまま"
         [[60 48 46 64] [0xFD 0 0 1] [0xFD 0 0 2] [0xFC 1 0 0]]
         (delete-line [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFC 1 0 0]] 2))
  (check "クオンタイズ: 早い音・遅い音を格子へ (レート 100)、後ろの位置は変えない"
         [[[60 48 46 64] [62 48 46 64] [64 48 46 64]] [[60 48 46 64] [62 48 46 64] [64 48 46 64]]]
         [(quantize [[60 50 46 64] [62 46 46 64] [64 48 46 64]] 0 3 48 100)
          (quantize [[60 40 46 64] [62 56 46 64] [64 48 46 64]] 0 3 48 100)])
  (check "クオンタイズ: レート 50 は半分だけ寄せる、範囲外は変えない"
         [[[60 49 46 64] [62 47 46 64] [64 48 46 64]] [[60 50 46 64] [62 46 46 64] [64 48 46 64]]]
         [(quantize [[60 50 46 64] [62 46 46 64] [64 48 46 64]] 0 3 48 50)
          (quantize [[60 50 46 64] [62 46 46 64] [64 48 46 64]] 0 1 48 100)])
  (check "bunkatsu: 7 を 3 つに (2 2 3)"
         [2 2 3] (bunkatsu 7 3))
  (check "和音の並べ替え: 低い順 / 高い順 (最後の ST は最後の位置へ)"
         [[[60 0 46 64] [64 0 46 64] [67 48 46 64]] [[67 0 46 64] [64 0 46 64] [60 48 46 64]]]
         [(sort-chords [[67 0 46 64] [60 0 46 64] [64 48 46 64]] 0 3 true)
          (sort-chords [[67 0 46 64] [60 0 46 64] [64 48 46 64]] 0 3 false)])
  (check "ストラム G 6: 3 音に 3・3 遅らせ、GT も短く、全体の長さは同じ"
         [[60 3 46 64] [64 3 43 64] [67 42 40 64]]
         (strum-chords [[60 0 46 64] [64 0 46 64] [67 48 46 64]] 0 3 6 true))
  (check "ベロシティ: 直線 40→100、和音の中で 60→90"
         [[40 70 100] [60 75 90 90]]
         [(mapv #(% 3) (velocity-ctrl [[60 48 46 1] [62 48 46 1] [64 48 46 1]] 0 3 :line 40 100 (java.util.Random. 1)))
          (mapv #(% 3) (velocity-ctrl [[60 0 46 1] [64 0 46 1] [67 48 46 1] [72 48 46 1]] 0 4 :chord 60 90 (java.util.Random. 1)))])
  (check "ベロシティ: ランダムは範囲内、オフセットは 1〜127 に収める、休符は対象外"
         [true [127 1] [0 48 0 0]]
         (let [r (velocity-ctrl (vec (repeat 50 [60 12 10 64])) 0 50 :random 70 80 (java.util.Random. 2))
               o (velocity-ctrl [[60 48 46 120] [62 48 46 5] [0 48 0 0]] 0 3 :offset 10 10 (java.util.Random. 2))
               o2 (velocity-ctrl [[60 48 46 120] [62 48 46 5]] 0 2 :offset -10 0 (java.util.Random. 3))]
           [(every? #(<= 70 (% 3) 80) r) [((o 0) 3) (max 1 (min 1 ((o2 1) 3)))] (o 2)]))
  (check "SAME 自動変換: 同じ小節を SAME に、展開で元に戻る"
         [[[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFC 0 0 0] [0xFC 1 0 0]] 2
          [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [60 48 46 64] [0xFD 0 0 3] [62 48 46 64] [0xFD 0 0 4]]]
         (let [src [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [60 48 46 64] [0xFD 0 0 3] [62 48 46 64] [0xFD 0 0 4]]
               {:keys [recs count]} (same-meas-conv src)]
           [recs count (same-meas-extract recs 0 (clojure.core/count recs))]))
  (check "SAME 自動変換: SAME を指す SAME は元の小節へ、鳴らないノートは休符にそろえる"
         [[0 48 0 0] [0xFD 0 0 1] [0xFC 0 0 0] [0xFC 0 0 0]]
         (:recs (same-meas-conv [[60 48 0 64] [0xFD 0 0 1] [0 48 0 0] [0xFD 0 0 2] [0xFC 1 0 0]])))
  (check "レプリケートの材料: 小節 2〜2 を取り出し、最後に小節線、キーを +12"
         [[74 48 46 64] [0xFD 0 0 2]]
         (shift-keys (measure-range-buffer [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [64 48 46 64]] 2 2) 12))
  (check "回数分の挿入"
         [[60 48 46 64] [62 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFD 0 0 3]]
         (insert-times [[60 48 46 64] [0xFD 0 0 1]] 1 [[62 48 46 64] [0xFD 0 0 0]] 2))
  (check "ループ圧縮: 1行×4 / 2行×3 / 3行×2、条件に満たないものはそのまま"
         [[[0xF9 0 0 0] [60 12 10 64] [0xF8 4 0 0]]
          [[0xF9 0 0 0] [60 12 10 64] [62 12 10 64] [0xF8 3 0 0]]
          [[0xF9 0 0 0] [60 12 10 64] [62 12 10 64] [64 12 10 64] [0xF8 2 0 0] [65 48 46 64]]
          [[60 12 10 64] [60 12 10 64] [60 12 10 64]]]
         (let [a [60 12 10 64] b [62 12 10 64] c [64 12 10 64]]
           [(:recs (repeat-compress [a a a a]))
            (:recs (repeat-compress [a b a b a b]))
            (:recs (repeat-compress [a b c a b c [65 48 46 64]]))
            (:recs (repeat-compress [a a a]))]))
  (check "ループ圧縮: コマンド行をまたがない、前に別の音があっても途中から見つける"
         [[67 48 46 64] [0xF9 0 0 0] [60 12 10 64] [0xF8 4 0 0] [0xFD 0 0 1] [60 12 10 64]]
         (:recs (repeat-compress [[67 48 46 64] [60 12 10 64] [60 12 10 64] [60 12 10 64] [60 12 10 64] [0xFD 0 0 1] [60 12 10 64]])))
  (check "値の増減: キー +1・+12、VEL −10 で回る、ST −1 で 255、ベンド、CC 番号、ユーザーエクスクルーシブ番号"
         [[61 48 46 64] [72 48 46 64] [60 48 46 123] [60 255 46 64] [0xEE 0 1 64] [0xEB 0 8 100] [0x97 0 0 0]
          [0xE6 0 32 0] [0xFC 1 0 0] [0xFC 0 0 0]]
         [(val-step [60 48 46 64] 0 1 false 1) (val-step [60 48 46 64] 0 1 true 1)
          (val-step [60 48 46 5] 3 -1 true 1) (val-step [60 0 46 64] 1 -1 false 1)
          (val-step [0xEE 0 0 64] 2 1 false 1) (val-step [0xEB 0 7 100] 0 1 false 1)
          (val-step [0x90 0 0 0] 0 -1 false 1) (val-step [0xE6 0 0 0] 2 -1 false 1)
          (val-step [0xFC 0 0 0] 1 1 false 3) (val-step [0xFC 0 0 0] 1 1 false 2)])
  (check "値の反転: ベンド、調の長短、チャンネル A/B、パン"
         [[0xEE 0 0 32] [0xF5 17 0 0] [0xE6 0 19 0] [0xEB 0 10 28]]
         [(val-rev [0xEE 0 0 96] 2) (val-rev [0xF5 1 0 0] 1) (val-rev [0xE6 0 3 0] 2) (val-rev [0xEB 0 10 100] 3)])
  (check "コード: C MAJOR / Cm7、転回 (上・下)、展開した行"
         [[60 64 67] [60 63 67 70] [64 67 72] [55 60 64] [[60 0 46 80] [64 0 46 80] [67 48 46 80]]]
         [(chord-keys 60 0) (chord-keys 60 18) (chord-rotate [60 64 67] 1) (chord-rotate [60 64 67] -1)
          (chord-records [60 48 46 80] [60 64 67])])
  (check "反転: 和音はかたまりのまま、順番を逆に"
         [[64 0 46 64] [67 24 22 64] [62 24 22 64] [60 48 46 64]]
         (reverse-records [[60 48 46 64] [62 24 22 64] [64 0 46 64] [67 24 22 64]]))
  (check "反転: リピートの開始と終了を入れ替え、回数は終了側へ"
         [[0xF9 0 0 0] [62 24 22 64] [60 48 46 64] [0xF8 3 0 0]]
         (reverse-records [[0xF9 0 0 0] [60 48 46 64] [62 24 22 64] [0xF8 3 0 0]]))
  (check "小節番号の振り直しは subvec でも動く"
         [[0xFD 0 0 1]]
         (renumber-measures (subvec [[60 48 46 64] [0xFD 0 0 0]] 1)))
  (check "ミックス: 2つの列を時刻順に混ぜる"
         [[60 0 46 64] [67 24 22 64] [69 24 22 64] [62 48 46 64]]
         (mix-records [[60 48 46 64] [62 48 46 64]] [[67 24 22 64] [69 24 22 64]]))
  (check "ミックス: 小節線はトラック側のものだけ、同じ時刻のイベントより前に置く"
         [[60 0 46 64] [67 48 22 64] [0xFD 0 0 0] [62 0 46 64] [69 48 22 64]]
         (mix-records [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64]] [[67 48 22 64] [0xFD 0 0 0] [69 48 22 64]]))
  (check "meas-no: SAME は1小節、SAME の直前でも小節が終わる"
         [1 2 4 5]
         (mapv #(meas-no [[60 48 46 64] [0xFC 0 0 0] [62 48 46 64] [0xFC 0 0 0] [0xFE 0 0 0]] %) [0 1 3 4]))
  (check "レコード→イベント変換"
         [{:type :note :key 60 :st 48 :gt 46 :vel 64}]
         (records->events [[60 48 46 64]])))

(run-checks)
