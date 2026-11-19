;; polysted_visual.clj — コントローラのビジュアルエディット (原作 VISUAL.C の vis_edit) の計算。純粋関数のみ。
;;
;; 原作の流れ (VISUAL.C・EDIT.C で確認):
;;   - 編集画面の V。範囲指定中なら範囲の中の既存のコントローラを編集、そうでなければカーソル位置に新しく作る。
;;   - 種類 EDIT MODE(P/V/E/M/N/C/K/T/R/U/X/nn/G): P=ピッチベンド(0xEE) M=1 V=7 E=11 N=10 (コントロール番号)
;;     C=アフタータッチ(0xEA) K=ポリフォニックアフタータッチ(0xED) T=テンポ(0xE7) X=トラックエクスクルーシブ(0x98)
;;     U=ユーザーエクスクルーシブ n (0x90+n) R=ローランドのパラメータ (0xDE、アドレス n) 数字=コントロール番号 1〜127
;;     /=記号 (spc_code) G=カーソル行の種類を使う。
;;   - 新しく作るとき: TOTAL ST (既定は 1 拍。カーソルが ST>1 の音ならその ST。2〜1小節×8) と
;;     COUNT (点の数。既定は TOTAL ST × vis_reso ÷ 100、vis_reso の既定は 100。"/n" で ST÷n、"%n" で ST×n÷100。
;;     2〜TOTAL ST かつ 768 まで)。点は全部同じ値 (カーソルより前の同じ種類の最後の値、無ければ
;;     ボリューム 100・エクスプレッション 127・パン 64・71〜74 は 64・テンポ 64、ほかは 0)、ST は等分 (bunkatsu)。
;;   - 範囲のとき (vis_a_in): 一致する行ごとに点を作り、その後の ST を足していく (240 を超えたら同じ値の点を足す)。
;;     一致する行が無く範囲の ST が 2 以上なら、範囲の ST を TOTAL ST にして新しく作る。
;;   - 波形 (vis_wave): 範囲は SUB (Space で置く) とカーソルの間、SUB が無ければ全体 (このとき ST を等分し直す)。
;;     F1/F2 は先頭・最後へ。Shift+F1 反転 (127-値、ピッチは符号)、Shift+F2 前後逆、F3 直線、Shift+F3 乱数、
;;     F4 (原作でも変化なし)、F5 1/4 周期の sin (Shift で cos)、F6 曲線表 curv_tbl (Shift で逆)、
;;     F7 山 (Shift で谷)、F8 sin 波、F9 三角波、F10 矩形波 (F8〜F10 は Shift で波の数を聞く。既定は 1 周期)。
;;   - 確定 (Enter): 同じ値の続く点をつなぎ (vis_lnk、ST 240 まで)、新規なら行を挿入 (後ろの時刻はずれる。
;;     END の位置なら小節線も入れる)。Shift+Enter なら既存のデータに時刻順に混ぜる (vis_ext_mix: 範囲の時間内の
;;     同じ種類の行を消してから trk_mix_sub)。範囲の編集は、ステップを変えていなければ値だけ書き戻す (vis_a_out)、
;;     変えていれば混ぜる。
;; 原作との違い【設計判断】: トラックエクスクルーシブ (0x98) は、カーソルの 0x98 の行から X か G で始める
;;   (続きの F7 の行を各点に写す。原作の excget)。vis_bufset・vis_a_out の vmo2 (ユーザーエクスクルーシブ・0xED・0xDE の b2) は原作では dat に書いていて
;;   出力に入らない (書き間違いと見て) b2 に入れる。前の値の検索は SAME の参照先までは追わない。
;;   範囲読み込みで 240 を超えたときに足す点の ST は、原作は前の点の ST の写しだが、その行の ST にする。
;;   ST が 240 を超える点は、その点の行を ST 240 にして残りを休符で足す。
;; 読み込み: (load-file "polysted_visual.clj")  ※ polysted_ctrl.clj・polysted_roll.clj の後、polysted_session.clj の前
(ns polysted-visual
  (:require [clojure.string :as str]
            [polysted-edit]
            [polysted-ctrl]))

;; ---- 種類 ----
(defn pitch? [vmo] (zero? vmo))

(defn kind-name "種類の表示 (原作の tmp9)" [vmo]
  (cond
    (pitch? vmo) "PITCH"
    (< vmo 128) (polysted-ctrl/ctrl-name vmo)
    (= vmo 0x98) "Tr.Exclu"
    (<= 0x90 vmo 0x97) (str "UserExc " (- vmo 0x90))
    (= vmo 0xE7) "TEMPO"
    (= vmo 0xDE) "Rol.Para"
    (= vmo 0xEA) "AFTER CH."
    (= vmo 0xED) "AFTER KEY"
    :else ""))

(defn default-value "前に同じ種類が無いときの値 (原作の lpara の既定)" [vmo]
  (cond (= vmo 7) 100 (= vmo 11) 127 (= vmo 10) 64 (<= 71 vmo 74) 64 (= vmo 0xE7) 64 :else 0))

(defn value-range [vmo] (if (pitch? vmo) [-8192 8191] [0 127]))
(defn clamp-value [vmo v] (let [[lo hi] (value-range vmo)] (-> v long (max lo) (min hi))))

(defn rec-value
  "行がこの種類なら値、違えば nil (ピッチは -8192〜8191、0xEA・0xE7 は b2、ほかは b3)"
  [[a _ c d] vmo]
  (cond
    (pitch? vmo) (when (= a 0xEE) (- (+ c (* d 128)) 8192))
    (< vmo 128)  (when (and (= a 0xEB) (= c vmo)) d)
    (= a vmo)    (if (#{0xEA 0xE7} vmo) c d)
    :else nil))

(def exc-cursor-msg "トラックエクスクルーシブはカーソルを 0x98 の行に置いて X か G です")

(defn parse-mode
  "EDIT MODE の入力 → {:vmo :vmo2} か {:error}。cur は カーソル行 (G で使う)。
   P V E M N C K T、数字 1〜127、U0〜U7、R0〜R127、/記号、G。X (トラックエクスクルーシブ) はカーソルが 0x98 の行のとき
   (原作は G で 0x98 の行から始めたときだけ続きのデータを写す。X だけでは続きの無い行になるので、カーソルの行を使う【設計判断】)"
  [text cur]
  (let [t (str/upper-case (str/trim (or text "")))
        num (fn [s] (when (re-matches #"\d{1,3}" s) (Long/parseLong s)))
        letters {"P" 0 "M" 1 "V" 7 "E" 11 "N" 10 "C" 0xEA "K" 0xED "T" 0xE7}]
    (cond
      (letters t) {:vmo (letters t) :vmo2 0}
      (= t "X") (if (= 0x98 (first cur)) {:vmo 0x98 :vmo2 (cur 2)} {:error exc-cursor-msg})
      (num t) (let [n (num t)] (if (<= 1 n 127) {:vmo n :vmo2 0} {:error "コントロール番号は 1〜127 です"}))
      (re-matches #"U\d" t) (let [n (num (subs t 1))] (if (<= n 7) {:vmo (+ 0x90 n) :vmo2 0} {:error "U0〜U7 です"}))
      (re-matches #"R\d{1,3}" t) (let [n (num (subs t 1))] (if (<= n 127) {:vmo 0xDE :vmo2 n} {:error "R0〜R127 です"}))
      (str/starts-with? t "/")
      (let [{:keys [code ctl]} (polysted-ctrl/spc-code t)]
        (cond (nil? code) {:error "記号が違います"}
              (= code 0xEE) {:vmo 0 :vmo2 0}
              (= code 0x98) (if (= 0x98 (first cur)) {:vmo 0x98 :vmo2 (cur 2)} {:error exc-cursor-msg})
              (and ctl (>= ctl 0)) {:vmo ctl :vmo2 0}
              :else {:vmo code :vmo2 0}))
      (= t "G")
      (let [[a _ c] cur]
        (cond (nil? a) {:error "カーソル行がありません"}
              (or (<= 0x90 a 0x98) (#{0xDE 0xED} a)) {:vmo a :vmo2 c}
              (= a 0xEE) {:vmo 0 :vmo2 0}
              (= a 0xEB) {:vmo c :vmo2 0}
              (and (>= a 0x80) (< a 0xF0)) {:vmo a :vmo2 0}
              :else {:error "コントローラの行ではありません"}))
      :else {:error "P V E M N C K T、1〜127、U0〜U7、R0〜R127、/記号、G のどれかです"})))

;; ---- 点を作る ----
(defn bunkatsu "ST の合計 st を co 個に等分 (原作 bunkatsu: 端数は後ろへ)" [st co]
  (vec (for [i (range 1 (inc co))] (- (quot (* st i) co) (quot (* st (dec i)) co)))))

(defn last-value "pos より前の同じ種類の最後の値。無ければ既定" [recs pos vmo]
  (or (some #(rec-value % vmo) (rseq (subvec (vec recs) 0 (min pos (count recs)))))
      (default-value vmo)))

(defn default-total-st "TOTAL ST の既定: 1拍 (1小節の 1/4)。カーソルが ST>1 の音ならその ST" [cur barlen]
  (if (and cur (< (cur 0) 127) (> (cur 1) 1)) (cur 1) (quot barlen 4)))

(defn parse-count "COUNT の入力 → 数 (\"/n\" は st÷n、\"%n\" は st×n÷100)。不正なら nil" [text st]
  (let [t (str/trim (or text ""))
        n (fn [s] (when (re-matches #"\d{1,4}" s) (Long/parseLong s)))]
    (cond
      (str/starts-with? t "/") (when-let [d (n (subs t 1))] (when (>= d 1) (quot (min st 768) d)))
      (str/starts-with? t "%") (when-let [d (n (subs t 1))] (when (>= d 1) (quot (* (min st 768) d) 100)))
      :else (n t))))

(defn count-ok? "COUNT は 2 以上、TOTAL ST 以下、768 以下" [co st] (and co (<= 2 co) (<= co st) (<= co 768)))

(defn new-points "新しく作る点: 全部前の値、ST は等分" [recs pos vmo st co]
  {:para (vec (repeat co (last-value recs pos vmo))) :step (bunkatsu st co)})

(defn read-points
  "範囲 [from, to) の行から点を読む (vis_a_in)。=> {:para :step :matched 一致した行の数 :st 範囲の ST}"
  [recs from to vmo]
  (loop [i from para [] step [] matched 0 st 0]
    (if (>= i (min to (count recs)))
      {:para para :step step :matched matched :st st}
      (let [[a b :as r] (recs i)
            v (rec-value r vmo)
            [para step matched] (if v [(conj para v) (conj step 0) (inc matched)] [para step matched])
            timed? (and (< a 0xF0) (pos? b))
            st (if timed? (+ st b) st)
            [para step] (cond
                          (not (and (seq para) timed?)) [para step]
                          (<= (+ (peek step) b) 240) [para (conj (pop step) (+ (peek step) b))]
                          :else [(conj para (peek para)) (conj step b)])]
        (recur (inc i) para step matched st)))))

;; ---- 点の加工 ----
(def curv-tbl
  [0 4 7 10 13 17 20 24 27 30 33 36 39 42 44 46 49 51 53 55 57 59 61 63 65 67 69 71 72 74 75 77
   78 80 81 82 84 85 86 87 89 90 91 92 93 94 95 96 97 98 99 100 100 101 102 103 103 104 105 105 106 107 107 108
   108 109 109 110 110 111 111 112 112 113 113 113 114 114 115 115 116 116 116 117 117 117 118 118 118 119 119 119 119 120 120 120
   120 121 121 121 121 121 122 122 122 122 122 122 123 123 123 123 123 123 123 123 124 124 124 124 124 124 124 124 124 125 125 125
   125 125 125 125 125 125 125 125 125 125 126 126 126 126 126 126 126 126 126 126 126 126 126 126 126 126 126 126 127 127 127 127
   127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127])

(defn- fix-round "原作の fix: 0.5 で丸める (負は 0 から離す。ピッチ以外は負を正に)" [f vmo]
  (long (cond (pos? f) (+ f 0.5) (neg? f) (let [g (- f 0.5)] (if (pitch? vmo) g (- g))) :else f)))

(defn wave
  "波形・加工 (vis_wave)。md 0〜9 は F1〜F10 (F1・F2 は Shift のときだけ: 反転・前後逆)。
   opts: :cx :ccx (nil なら全体、このとき ST を等分し直す) :shift? :xc (波の数×2、既定 2) :st (合計 ST) :vmo :rnd (0〜1 の乱数の関数)
   => {:para :step}"
  [{:keys [para step]} md {:keys [cx ccx shift? xc st vmo rnd] :or {xc 2 rnd rand}}]
  (let [co (count para)
        [xx ln] (cond (nil? ccx) [0 co] (< ccx cx) [ccx (inc (- cx ccx))] :else [cx (inc (- ccx cx))])]
    (if (< ln 2)
      {:para para :step step}
      (let [step (if (nil? ccx) (bunkatsu st co) step)
            seg (subvec para xx (+ xx ln))
            ct (seg 0) ct2 (peek seg) ct3 (- ct2 ct)
            [md sh] (if (<= 3 md 5)
                      (if (= ct ct2) [2 false] [md (if (neg? ct3) (not shift?) shift?)])
                      [md shift?])
            ct4 (if (neg? ct3) (dec ct3) (inc ct3))
            mx (apply max seg) mn (apply min seg)
            d (dec ln)
            f (fn [i v]
                (case (int md)
                  0 (if (pitch? vmo) (- v) (- 127 v))
                  1 (seg (- d i))
                  2 (if (or (not sh) (= ct ct2))
                      (+ ct (quot (* ct3 i) d))
                      (min mx (+ mn (long (Math/floor (* (rnd) (inc (- mx mn))))))))
                  3 v
                  4 (if (and (pos? i) (< i d))
                      (if-not sh
                        (long (+ ct (* ct4 (Math/sin (/ (* (/ Math/PI 2) i) d)))))
                        (long (- ct2 (* ct4 (Math/cos (/ (* (/ Math/PI 2) i) d))))))
                      v)
                  5 (if-not sh
                      (+ ct (quot (* ct3 (curv-tbl (quot (* 191 i) d))) 127))
                      (- ct2 (quot (* ct3 (curv-tbl (- 191 (quot (* 191 i) d)))) 127)))
                  6 (fix-round (if-not sh
                                 (+ mn (* (Math/sin (/ (* Math/PI i) d)) (- mx mn)))
                                 (- mx (* (Math/sin (/ (* Math/PI i) d)) (- mx mn))))
                               vmo)
                  7 (fix-round (+ (bit-shift-right (+ mx mn) 1)
                                  (* (Math/sin (/ (* Math/PI i xc) d)) (bit-shift-right (inc (- mx mn)) 1)))
                               vmo)
                  8 (if (= mx mn)
                      v
                      (loop [a (+ (bit-shift-right (+ mx mn) 1) (quot (* (- mx mn) xc (inc i)) ln))]
                        (cond (> a mx) (recur (- mx (- a mx)))
                              (< a mn) (recur (- mn (- a mn)))
                              :else a)))
                  9 (if (even? (quot i (max 1 (quot ln xc)))) mx mn)))
            seg2 (vec (map-indexed f seg))]
        {:para (into (into (subvec para 0 xx) (map #(clamp-value vmo %) seg2)) (subvec para (+ xx ln)))
         :step step}))))

(defn line-between "マウスで描いたとき、点 a と b の間を直線でつなぐ (端の値はそのまま)" [pts a b vmo]
  (if (< (Math/abs (long (- a b))) 2)
    pts
    (wave pts 2 {:cx a :ccx b :vmo vmo :st 0})))

(defn link-points "同じ値の続く点をつなぐ (vis_lnk、ST 240 まで)" [{:keys [para step]}]
  (reduce (fn [{p :para s :step :as acc} [v st]]
            (if (and (seq p) (= (peek p) v) (<= (+ (peek s) st) 240))
              (assoc acc :step (conj (pop s) (+ (peek s) st)))
              {:para (conj p v) :step (conj s st)}))
          {:para [] :step []}
          (map vector para step)))

(defn cut-points "ST を n 以下に分ける (vis_cut、768 点まで)" [{:keys [para step]} n]
  (let [ps (for [[v st] (map vector para step)
                 s (concat (repeat (quot (dec st) n) n) [(- st (* n (quot (dec st) n)))])
                 :when (pos? s)]
             [v s])
        ps (take 768 ps)]
    {:para (mapv first ps) :step (mapv second ps)}))

(defn zoom-steps "ST の合計を out に伸縮 (vis_zom: 累計で比例配分)" [step out]
  (let [in (reduce + step)]
    (if (zero? in)
      (vec step)
      (first (reduce (fn [[acc cum stx] s]
                       (let [cum (+ cum s) dt (- (quot (* cum out) in) stx)]
                         [(conj acc dt) cum (+ stx dt)]))
                     [[] 0 0] step)))))

(defn insert-step "点 cx の前に ST j の点を入れる (値は cx の値。原作の I)" [{:keys [para step]} cx j]
  {:para (vec (concat (subvec para 0 cx) [(para cx)] (subvec para cx)))
   :step (vec (concat (subvec step 0 cx) [j] (subvec step cx)))})

(defn delete-point "点 cx を消す (原作の D)" [{:keys [para step]} cx]
  {:para (vec (concat (subvec para 0 cx) (subvec para (inc cx))))
   :step (vec (concat (subvec step 0 cx) (subvec step (inc cx))))})

(defn delete-st "点 cx から ST を j だけ消す (原作の Shift+D)" [pts cx j]
  (loop [{:keys [step] :as pts} pts j j]
    (cond
      (or (<= j 0) (>= cx (count step))) pts
      (> (step cx) j) (update-in pts [:step cx] - j)
      :else (recur (delete-point pts cx) (- j (step cx))))))

;; ---- 行にする・書き戻す ----
(defn- split-long "ST が 240 を超える行は ST 240 にして、残りを休符で足す" [recs]
  (vec (mapcat (fn [[a b c d :as r]]
                 (if (and (< a 0xF0) (> b 240))
                   (let [rest-st (- b 240)]
                     (concat [[a 240 c d]] (repeat (quot rest-st 240) [0 240 0 0])
                             (when (pos? (mod rest-st 240)) [[0 (mod rest-st 240) 0 0]])))
                   [r]))
               recs)))

;; 種類 kind = {:vmo :vmo2 :exc}。:exc はトラックエクスクルーシブ (0x98) の続きの行 (F7) で、
;; 原作の excget と同じく、カーソルの 0x98 の行のものを各点に写す。
(defn point-recs
  "1点の行 (最後の行が ST s を持つ)。0x98 は [98 0 vmo2 値] + 続きの行 + 休符 [0 s 0 0] (vis_bufset の excget)"
  [{:keys [vmo vmo2 exc]} v s]
  (let [v7 (-> v (max 0) (min 127))]
    (cond
      (pitch? vmo) (let [j (-> (+ v 8192) (max 0) (min 16383))] [[0xEE s (bit-and j 0x7F) (bit-shift-right j 7)]])
      (< vmo 128) [[0xEB s vmo v7]]
      (#{0xEA 0xE7} vmo) [[vmo s v7 0]]
      (= vmo 0x98) (vec (concat [[0x98 0 (or vmo2 0) v7]] exc [[0 s 0 0]]))
      :else [[vmo s (if (or (< vmo 0x98) (#{0xED 0xDE} vmo)) (or vmo2 0) 0) v7]])))

(defn points->records
  "点 → 行 (vis_bufset)。bar-at (小節の中の開始位置) と barlen を渡すと、小節線を入れる (END の位置で作るとき)"
  ([pts kind] (points->records pts kind nil nil))
  ([{:keys [para step]} kind bar-at barlen]
   (split-long
    (if-not barlen
      (vec (mapcat (fn [v s] (point-recs kind v s)) para step))
      ;; 小節線をまたぐ点は、小節線までの ST にして、残りを休符で続ける
      (let [emit (fn [[out st] [v s]]
                   (let [g (point-recs kind v s)]
                     (loop [r (peek g) s s st st out (into out (pop g))]
                       (cond
                         (< (+ st s) barlen) [(conj out (assoc r 1 s)) (+ st s)]
                         (= (+ st s) barlen) [(conj out (assoc r 1 s) [0xFD 0 0 0]) 0]
                         :else (let [ss (- barlen st) out (conj out (assoc r 1 ss) [0xFD 0 0 0]) s (- s ss)]
                                 (if (pos? s) (recur [0 s 0 0] s 0 out) [out 0]))))))]
        (first (reduce emit [(vec (repeat (quot bar-at barlen) [0xFD 0 0 0])) (mod bar-at barlen)]
                       (map vector para step))))))))

(defn overwrite
  "範囲 [from, to) の同じ種類の行の値を点の値で書き換える (vis_a_out。行の数と ST は変えない)"
  [recs from to {:keys [vmo] :as kind} para]
  (loop [i from k 0 recs (vec recs)]
    (if (or (>= i (min to (count recs))) (>= k (count para)))
      recs
      (let [r (recs i)]
        (if (rec-value r vmo)
          (let [[a b] r [_ _ c d] (first (point-recs kind (para k) b))]
            (recur (inc i) (inc k) (assoc recs i [a b c d])))
          (recur (inc i) k recs))))))

(defn exc-tail "行 pos の 0x98 に続く F7 の行" [recs pos]
  (vec (take-while #(= 0xF7 (% 0)) (drop (inc pos) recs))))

(defn mix-in
  "pos からのデータに点を時刻順に混ぜる (vis_ext_mix)。先に、点の時間 (最後の点の手前までの ST+1、
   ost のほうが長ければ ost) の中にある同じ種類の行を休符にする (0x98 は続きの行も消す)"
  [recs pos {:keys [step] :as pts} {:keys [vmo] :as kind} ost]
  (let [recs (vec recs)
        ttc (max (or ost 0) (inc (reduce + (butlast step))))
        cleared (loop [i pos ttc ttc recs recs]
                  (if (or (>= i (count recs)) (<= ttc 0))
                    recs
                    (let [[a b] (recs i)
                          hit? (rec-value (recs i) vmo)
                          recs (if hit? (assoc recs i [0 b 0 0]) recs)
                          recs (if (and hit? (= vmo 0x98))
                                 (reduce #(assoc %1 %2 [0 0 0 0]) recs
                                         (take-while #(= 0xF7 ((recs %) 0)) (range (inc i) (count recs))))
                                 recs)]
                      (recur (inc i) (if (< a 0xF0) (- ttc b) ttc) recs))))]
    (into (subvec cleared 0 pos)
          (polysted-edit/mix-records (subvec cleared pos) (points->records pts kind)))))

;; ---- 簡易チェック (期待値は VISUAL.C を手で追って求めた) ----
(defn run-checks []
  (let [recs [[0xEB 0 7 90] [60 48 46 64] [0xEB 24 7 80] [0xEB 24 7 70] [62 48 46 64] [0xEE 0 0 64] [0xFE 0 0 0]]
        pts {:para [0 40 80 120] :step [12 12 12 12]}
        w (fn [md opts] (:para (wave pts md (merge {:cx 3 :ccx 0 :vmo 7 :st 48} opts))))
        checks
        [["種類: 文字・数字・U・R・記号・G、エクスクルーシブはカーソルが 0x98 の行のとき"
          (conj (mapv #(parse-mode % [0xEB 0 11 100]) ["v" "p" "74" "u3" "r12" "/H" "g" "x" "0"])
                (parse-mode "x" [0x98 0 5 0]) (parse-mode "g" [0x98 0 5 0]))
          [{:vmo 7 :vmo2 0} {:vmo 0 :vmo2 0} {:vmo 74 :vmo2 0} {:vmo 0x93 :vmo2 0} {:vmo 0xDE :vmo2 12}
           {:vmo 64 :vmo2 0} {:vmo 11 :vmo2 0} {:error exc-cursor-msg}
           {:error "コントロール番号は 1〜127 です"} {:vmo 0x98 :vmo2 5} {:vmo 0x98 :vmo2 5}]]
         ["前の値・既定・等分・TOTAL ST と COUNT の既定"
          [(last-value recs 4 7) (last-value recs 6 0) (last-value recs 0 7) (last-value recs 3 10)
           (bunkatsu 10 4) (default-total-st [60 36 30 64] 192) (default-total-st [0xEB 0 7 1] 192)
           (parse-count "/4" 48) (parse-count "%50" 48) (parse-count "20" 48) (count-ok? 49 48)]
          [70 0 100 64 [2 3 2 3] 36 48 12 24 20 false]]
         ["範囲の読み込み: 一致した行から点、後の ST を足す"
          (read-points recs 0 6 7)
          {:para [90 80 70] :step [48 24 72] :matched 3 :st 144}]
         ["波形: 反転・前後逆・直線・曲線表・矩形 (SUB とカーソルの間)"
          [(w 0 {}) (w 1 {}) (:para (wave {:para [0 0 0 0 90] :step [1 1 1 1 1]} 2 {:cx 0 :ccx 4 :vmo 7}))
           (:para (wave {:para [0 0 0 127] :step [1 1 1 1]} 5 {:cx 0 :ccx 3 :vmo 7})) (w 9 {})]
          [[127 87 47 7] [120 80 40 0] [0 22 45 67 90] [0 108 125 127] [120 120 0 0]]]
         ["波形: SUB なしは全体で ST を等分し直す、乱数は最小〜最大"
          [(:step (wave {:para [0 10] :step [5 43]} 2 {:cx 0 :ccx nil :vmo 7 :st 48}))
           (:para (wave pts 2 {:cx 0 :ccx 3 :vmo 7 :shift? true :rnd (constantly 0.999)}))]
          [[24 24] [120 120 120 120]]]
         ["つなぐ・分ける・伸縮・挿入・削除"
          [(link-points {:para [5 5 6 6 6] :step [100 100 100 100 100]})
           (cut-points {:para [1 2] :step [10 3]} 4) (zoom-steps [12 12 12 12] 24)
           (insert-step pts 1 6) (delete-st pts 1 18)]
          [{:para [5 6 6] :step [200 200 100]} {:para [1 1 1 2] :step [4 4 2 3]} [6 6 6 6]
           {:para [0 40 40 80 120] :step [12 6 12 12 12]} {:para [0 80 120] :step [12 6 12]}]]
         ["行にする: ピッチ・CC・テンポ、小節線を入れる、240 を超える ST"
          [(points->records {:para [-8192 0 8191] :step [1 1 1]} {:vmo 0 :vmo2 0})
           (points->records {:para [100 64] :step [24 1]} {:vmo 0xE7 :vmo2 0})
           (points->records {:para [1 2 3] :step [24 48 48]} {:vmo 7 :vmo2 0} 168 192)
           (points->records {:para [9] :step [500]} {:vmo 11 :vmo2 0})]
          [[[0xEE 1 0 0] [0xEE 1 0 64] [0xEE 1 127 127]] [[0xE7 24 100 0] [0xE7 1 64 0]]
           [[0xEB 24 7 1] [0xFD 0 0 0] [0xEB 48 7 2] [0xEB 48 7 3]]
           [[0xEB 240 11 9] [0 240 0 0] [0 20 0 0]]]]
         ["書き戻す・混ぜる"
          [(overwrite recs 0 6 {:vmo 7 :vmo2 0} [1 2 3])
           (mix-in [[60 48 46 64] [0xEB 24 7 9] [62 24 46 64] [0xFE 0 0 0]] 0 {:para [10 20] :step [24 24]} {:vmo 7 :vmo2 0} nil)]
          [[[0xEB 0 7 1] [60 48 46 64] [0xEB 24 7 2] [0xEB 24 7 3] [62 48 46 64] [0xEE 0 0 64] [0xFE 0 0 0]]
           [[60 0 46 64] [0xEB 24 7 10] [0xEB 24 7 20] [0xEB 24 7 9] [62 24 46 64]]]]]
        checks (conj checks
                     ["トラックエクスクルーシブ: 点ごとに続きの行を写して休符で ST、混ぜるときは続きの行ごと置き換え"
                      (let [kind {:vmo 0x98 :vmo2 5 :exc [[0xF7 0 0x41 0x81]]}]
                        [(points->records {:para [10 20] :step [24 24]} kind)
                         (exc-tail [[0x98 0 5 9] [0xF7 0 0x41 0x81] [60 48 46 64]] 0)
                         (mix-in [[0x98 0 5 9] [0xF7 0 0x41 0x81] [60 48 46 64] [0xFE 0 0 0]] 0 {:para [30] :step [48]} kind 48)])
                      [[[0x98 0 5 10] [0xF7 0 0x41 0x81] [0 24 0 0] [0x98 0 5 20] [0xF7 0 0x41 0x81] [0 24 0 0]]
                       [[0xF7 0 0x41 0x81]]
                       [[0x98 0 5 30] [0xF7 0 0x41 0x81] [60 48 46 64]]]])
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted-visual: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(run-checks)
