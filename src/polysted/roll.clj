;; src/polysted/roll.clj — ピアノロール表示の計算 (純粋関数。描画は src/polysted/gui.clj)
;;
;; 原作 STED2 にピアノロール画面は無い。GUI ならではの表示として加えたもの【設計判断】。
;; 時間軸は「編集画面の行の並び」に合わせる: レコードを書かれた順に ST を足していく。
;;   - SAME (0xFC) は参照先の小節の中身をその位置に展開し、音は SAME の行に対応づける (:same? true)。
;;   - リピート (F9/F8) は展開しない (編集画面と同じく1回分だけ並ぶ)【設計判断】。
;;     このため再生時の時間とはずれることがある。
;;   - 音符は K# < 0x80 で GT・VEL がともに正の行。長さは GT (タイで ST を超えてもそのまま)。
;; 編集 (insert-note / remove-note / move-note) は「時刻とキー」の操作をレコード列の操作に置き換える【設計判断】:
;;   - 置く: その時刻に始まる休符は音に変える。休符・音・コントローラの途中なら、その行の ST を分けて
;;     後ろに音を入れる (残りの ST を新しい音が持つ)。ちょうど始まる時刻なら ST=0 で前に入れる (和音)。
;;     最後より後なら休符で間を埋めて END の前に足す。SAME の小節の中には置けない。
;;   - 消す: ST=0 ならその行を消す。直前の行が音か休符なら、その ST に足して消す (255 を超えるときを除く。原作の最適化で
;;     休符を前の音に結合するのと同じ形。ほかの音の時刻は変わらない)。直前がコントローラ・小節線などなら
;;     同じ ST の休符にする。
;;   - 動かす: 消してから置く。
(ns polysted.roll
  (:require [polysted.edit]))

(defn- meas-body
  "小節 m (1始まり) の中身 (小節線・SAME の手前まで)。無ければ空"
  [recs m]
  (if-let [ad (polysted.edit/meas-add recs m)]
    (vec (take-while #(< (% 0) 0xFC) (subvec recs (min ad (count recs)))))
    []))

(defn- note? [[a _ c d]] (and (< a 0x80) (pos? c) (pos? d)))

(defn roll-data
  "レコード列 → {:notes [{:idx :tick :len :key :vel :same?}] :bars [小節の先頭 tick] :ticks [各行の先頭 tick] :end 最後の tick}"
  [recs]
  (let [recs (vec recs)
        evs  (polysted.edit/records->events recs)
        n    (count recs)]
    (loop [i 0 t 0 notes (transient []) bars [0] ticks (transient [])]
      (if (>= i n)
        {:notes (persistent! notes) :bars (vec (distinct bars)) :ticks (persistent! ticks) :end t}
        (let [[a b c :as r] (recs i)
              ticks (conj! ticks t)]
          (cond
            (= a 0xFE) (recur n t notes bars ticks)
            (= a 0xFC)                       ; SAME: 参照先の小節を展開
            (let [body (meas-body recs (+ b (* 256 (bit-and c 3))))
                  [notes t2] (reduce (fn [[ns tt] br]
                                       (let [st (:st (first (polysted.edit/records->events [br])) 0)]
                                         [(cond-> ns (note? br) (conj! {:idx i :tick tt :len (br 2) :key (br 0)
                                                                        :vel (br 3) :same? true}))
                                          (+ tt (or st 0))]))
                                     [notes t] body)]
              (recur (inc i) t2 notes (conj bars t2) ticks))
            (= a 0xFD) (recur (inc i) t notes (conj bars t) ticks)
            :else
            (let [st (:st (evs i) 0)]
              (recur (inc i) (+ t (or st 0))
                     (cond-> notes (note? r) (conj! {:idx i :tick t :len (r 2) :key a :vel (r 3)}))
                     bars ticks))))))))

(defn cursor-tick "行 idx の先頭の tick (END より後なら最後)" [{:keys [ticks end]} idx]
  (get ticks idx end))

(defn note-under
  "tick・キーの位置にある音 (roll-data の :notes の要素。重なっていれば後の音、SAME でない音を優先)。無ければ nil"
  [{:keys [notes]} tick key]
  (let [hits (filter #(and (= key (:key %)) (<= (:tick %) tick) (< tick (+ (:tick %) (max 1 (:len %))))) notes)]
    (or (last (remove :same? hits)) (last hits))))

(defn note-at "tick・キーの位置にある音の行番号。無ければ nil" [d tick key]
  (:idx (note-under d tick key)))

(defn idx-at-tick
  "tick の位置の行 (その tick までに始まった最後の行。同じ tick の行が複数なら最後のもの)"
  [{:keys [ticks]} tick]
  (or (last (keep-indexed (fn [i t] (when (<= t tick) i)) ticks)) 0))

;; ---- 編集 ----
(defn- vec-insert [v i xs] (vec (concat (subvec v 0 i) xs (subvec v i))))
(defn- vec-remove [v i] (vec (concat (subvec v 0 i) (subvec v (inc i)))))
(defn note-rec? "音の行 (K# < 0x80 で GT・VEL が正)" [r] (boolean (and r (note? r))))

(defn remove-note
  "行 idx の音を消す (ほかの音の時刻は変えない)。音の行でなければそのまま"
  [recs idx]
  (let [recs (vec recs) r (get recs idx)]
    (cond
      (not (note-rec? r)) recs
      (zero? (r 1)) (vec-remove recs idx)
      (let [p (get recs (dec idx))] (and p (< (p 0) 0x80) (<= (+ (p 1) (r 1)) 255)))
      (-> recs (update-in [(dec idx) 1] + (r 1)) (vec-remove idx))
      :else (assoc recs idx [0 (r 1) 0 0]))))

(defn insert-note
  "tick にキー key・GT gt・VEL vel の音を置く。=> {:recs 新しいレコード列 :idx 置いた行} か {:error 理由}"
  [recs tick key gt vel]
  (let [recs (vec recs)
        {:keys [ticks end]} (roll-data recs)
        n (count recs)
        span (fn [i] (- (get ticks (inc i) end) (ticks i)))
        fe (or (first (keep-indexed (fn [i r] (when (= 0xFE (r 0)) i)) recs)) n)]
    (if (>= tick end)
      (let [gap (- tick end)]
        (if (pos? gap)
          {:recs (vec-insert recs fe [[0 gap 0 0] [key gt gt vel]]) :idx (inc fe)}
          {:recs (vec-insert recs fe [[key gt gt vel]]) :idx fe}))
      (let [i (last (filter #(and (<= (ticks %) tick) (pos? (span %))) (range (min n (count ticks)))))
            r (recs i) t0 (ticks i)]
        (cond
          (= 0xFC (r 0)) {:error "SAME の小節の中には置けません (SAME を展開してから)"}
          (= t0 tick)
          (let [j (first (filter #(and (= tick (ticks %)) (not (#{0xFD 0xFC} ((recs %) 0)))) (range (inc i))))]
            (if (and (= j i) (< (r 0) 0x80) (not (note? r)))
              {:recs (assoc recs i [key (r 1) gt vel]) :idx i}                 ; 休符を音に
              {:recs (vec-insert recs j [[key 0 gt vel]]) :idx j}))            ; 和音として前に
          (not= (r 1) (span i)) {:error "この位置には置けません"}
          :else                                                                ; 行の途中: ST を分ける
          {:recs (-> recs (assoc-in [i 1] (- tick t0)) (vec-insert (inc i) [[key (- (+ t0 (span i)) tick) gt vel]]))
           :idx (inc i)})))))

(defn move-note
  "行 idx の音を tick・キー key へ動かす (GT・VEL はそのまま)。=> {:recs :idx} か {:error}"
  [recs idx tick key]
  (let [r (get (vec recs) idx)]
    (if-not (note-rec? r)
      {:error "音の行ではありません"}
      (insert-note (remove-note recs idx) tick key (r 2) (r 3)))))

(defn key-window
  "表示するいちばん低いキー。nrows 行に収まれば音域の中央、収まらなければ cursor-key を中央に。
   音が無ければ C4 (60) のあたり"
  [{:keys [notes]} cursor-key nrows]
  (let [ks (map :key notes)
        clamp (fn [lo] (-> lo (max 0) (min (max 0 (- 128 nrows)))))]
    (if (empty? ks)
      (clamp (- 60 (quot nrows 2)))
      (let [lo (apply min ks) hi (apply max ks)]
        (clamp (if (<= (inc (- hi lo)) nrows)
                 (- (quot (+ lo hi) 2) (quot nrows 2))
                 (- (or cursor-key (quot (+ lo hi) 2)) (quot nrows 2))))))))

(defn scroll-offset
  "横の表示開始 tick。カーソルが表示幅の 10%〜85% にあれば prev のまま、外れたら 25% の位置に来るよう動かす"
  [prev cursor width]
  (let [prev (or prev 0)]
    (if (and (>= cursor (+ prev (* 0.1 width))) (<= cursor (+ prev (* 0.85 width))))
      prev
      (max 0 (long (- cursor (* 0.25 width)))))))

;; ---- 簡易チェック (期待値は手で追って求めた) ----
(defn run-checks []
  (let [recs [[60 0 46 64] [64 48 40 80] [0 48 0 0] [0xFD 0 0 1]   ; 小節1: C4+E4 の和音、休符
              [0xEB 12 7 100] [67 36 30 90] [0xFD 0 0 2]          ; 小節2: コントローラ (ST 12) と G4
              [0xFC 1 0 0]                                         ; 小節3: SAME 1
              [72 48 96 64] [0xFE 0 0 0]]                          ; 小節4: C5 (タイで GT > ST)
        d (roll-data recs)
        checks
        [["音符: 和音・休符・コントローラの ST・SAME の展開・GT > ST"
          (mapv (juxt :idx :tick :len :key :same?) (:notes d))
          [[0 0 46 60 nil] [1 0 40 64 nil] [5 108 30 67 nil] [7 144 46 60 true] [7 144 40 64 true] [8 240 96 72 nil]]]
         ["小節の先頭と各行の tick、最後"
          [(:bars d) (:ticks d) (:end d)]
          [[0 96 144 240] [0 0 48 96 96 108 144 144 240 288] 288]]
         ["当たり判定: 音の上・SAME の音・何も無い所"
          [(note-at d 10 64) (note-at d 150 60) (note-at d 50 60) (idx-at-tick d 100) (idx-at-tick d 999) (idx-at-tick d 300)]
          [1 7 nil 4 9 9]]
         ["表示するキー: 収まる・収まらない・音が無い"
          [(key-window d 60 24) (key-window d 60 8) (key-window {:notes []} nil 24) (key-window {:notes [{:key 127}]} 127 24)]
          [54 56 48 104]]
         ["横スクロール: 範囲内はそのまま、外れたら 25% の位置"
          [(scroll-offset 0 100 400) (scroll-offset 0 390 400) (scroll-offset 500 520 400) (scroll-offset 500 100 400)]
          [0 290 420 0]]
         ["消す: ST=0 の音、和音の最後の音 (ST を前へ)、ふつうの音 (休符に)"
          [(take 2 (remove-note recs 0)) (take 2 (remove-note recs 1)) (get (remove-note recs 5) 5) (= recs (remove-note recs 3))
           (remove-note [[60 48 46 64] [62 48 46 64] [0xFE 0 0 0]] 1) (remove-note [[60 200 46 64] [62 100 46 64]] 1)]
          [[[64 48 40 80] [0 48 0 0]] [[60 48 46 64] [0 48 0 0]] [0 36 0 0] true
           [[60 96 46 64] [0xFE 0 0 0]] [[60 200 46 64] [0 100 0 0]]]]
         ["置く: 休符の頭・休符の途中・和音・コントローラの途中"
          (let [f (fn [t k] (let [{r :recs i :idx} (insert-note recs t k 20 100)] [i (subvec r (max 0 (dec i)) (+ i 2))]))]
            [(f 48 62) (f 72 62) (f 0 67) (f 100 62)])
          [[2 [[64 48 40 80] [62 48 20 100] [0xFD 0 0 1]]]
           [3 [[0 24 0 0] [62 24 20 100] [0xFD 0 0 1]]]
           [0 [[67 0 20 100] [60 0 46 64]]]
           [5 [[0xEB 4 7 100] [62 8 20 100] [67 36 30 90]]]]]
         ["置く: SAME の中は不可、最後の位置、最後より後 (休符で埋める)"
          [(insert-note recs 150 62 20 100)
           (let [{r :recs i :idx} (insert-note recs 288 62 20 100)] [i (subvec r 9)])
           (let [{r :recs i :idx} (insert-note recs 300 62 20 100)] [i (subvec r 9)])]
          [{:error "SAME の小節の中には置けません (SAME を展開してから)"}
           [9 [[62 20 20 100] [0xFE 0 0 0]]]
           [10 [[0 12 0 0] [62 20 20 100] [0xFE 0 0 0]]]]]
         ["動かす: G4 (108) → F4 の 48、音でない行は不可"
          [(let [{r :recs i :idx} (move-note recs 5 48 65)] [i (r 2) (r 5)]) (move-note recs 3 0 60)]
          [[2 [65 48 30 90] [0 36 0 0]] {:error "音の行ではありません"}]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted.roll: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

