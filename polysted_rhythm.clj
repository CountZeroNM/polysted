;; polysted_rhythm.clj — リズムパターン編集の変換 (原作 REDIT.C の rtopat / pattor)
;;
;; 小節を「ステップ」の並びとして見る。1ステップは ST=0 で続くノートと、最後の ST>0 の音 (または休符) の
;; かたまりで、そのステップの長さ ST を持つ。各ステップは、リズム割り当て表の 32 音それぞれの
;; [ベロシティ GT] を持つ。表に無いキーの音 (:extra) とコントローラなどの行 (:raw) はそのまま保持する。
;; 純粋関数のみ。読み込み: (load-file "polysted_rhythm.clj")  ※ロード時に run-checks が走る
(ns polysted-rhythm
  (:require [polysted-edit]))

;; STED2.CNF の既定 #rhy_vel (音の強さ 1〜9 のベロシティ。0 は OFF)
(def rhy-vel [0 14 28 42 56 70 84 98 112 127])

(defn vel->level
  "ベロシティ → 強さ 0〜9 (REDIT.C の vlcl: rhy-vel の区間で決める)"
  [v]
  (cond (<= v 0) 0
        :else (or (some (fn [i] (when (<= v (rhy-vel i)) i)) (range 1 10)) 9)))

;; 定義ファイル SC55.DEF の既定のリズム割り当て (名前・キー・GT)
(def default-assign
  [["Hand Clap" 39 1] ["Mt HiConga" 62 1] ["High Conga" 63 1] ["Low Conga" 64 1]
   ["Crash Cym" 49 1] ["Ride Cym" 51 1] ["OpenHiHat2" 44 1] ["OpenHiHat1" 46 1]
   ["Clsd HiHat" 42 1] ["Acou HiTom" 48 1] ["AcouMidTom" 45 1] ["AcouLowTom" 41 1]
   ["Rim Shot" 37 1] ["Elec SD" 40 1] ["Acou SD" 38 1] ["Acou BD" 36 1]
   ["Hi Timbale" 65 1] ["LowTimbale" 66 1] ["Cowbell" 56 1] ["High Bongo" 60 1]
   ["Low Bongo" 61 1] ["High Agogo" 67 1] ["Low Agogo" 68 1] ["Tambourine" 54 1]
   ["Claves" 75 1] ["Maracas" 70 1] ["SmbaWhis L" 72 1] ["SmbaWhis S" 71 1]
   ["Cabasa" 69 1] ["Quijada" 73 1] ["" 0 1] ["" 0 1]])

(defn effective-assign
  "曲のリズム割り当て表 (32 × [名前 キー GT])。キーがすべて 0 (空の表) なら SC55.DEF の既定【設計判断】"
  [table]
  (if (and (seq table) (some #(pos? (second %)) table))
    (vec (take 32 (concat table (repeat ["" 0 1]))))
    default-assign))

(defn- empty-step [st] {:kind :note :st st :hits {} :extra [] :raw []})

(defn measure->pattern
  "小節 meas (1始まり) → {:steps [...] :same? :too-many?} (rtopat)。
   空の最後の小節 (END、または小節線・SAME の直後が END) は bar-st ÷ step-st 個の空ステップ。
   :steps の要素: {:kind :note|:raw :st ST :hits {楽器番号 [vel gt]} :extra [表に無い音の行] :raw [コマンド行]}"
  [recs meas assign bar-st step-st]
  (let [recs (vec recs) n (count recs)
        ad   (or (polysted-edit/meas-add recs meas) n)
        at   (fn [i] (get recs i [0xFE 0 0 0]))]
    (cond
      (= 0xFC ((at ad) 0)) {:steps [] :same? true}
      (or (>= ((at ad) 0) 0xFE) (and (>= ((at ad) 0) 0xFC) (>= ((at (inc ad)) 0) 0xFE)))
      {:steps (vec (repeat (quot bar-st step-st) (empty-step step-st)))}
      :else
      ;; キー → 楽器番号 (同じキーが複数あれば番号の小さい方)
      (let [key->idx (into {} (for [i (range 31 -1 -1) :let [k (second (assign i))] :when (pos? k)] [k i]))]
        (loop [i ad cur nil steps []]
          (let [[a b c d :as r] (at i)
                flush (fn [steps cur] (if cur (conj steps cur) steps))]
            (cond
              (> (count steps) 63) {:steps steps :too-many? true}
              (> a 0xFB) {:steps (flush steps cur)}
              (< a 0x80)
              (let [cur (if (or (nil? cur) (= :raw (:kind cur))) (empty-step 0) cur)
                    cur (if (or (pos? c) (pos? d))
                          (if (and (pos? c) (pos? d) (key->idx a))
                            (assoc-in cur [:hits (key->idx a)] [d c])
                            (update cur :extra conj [a 0 c d]))
                          cur)]
                (if (pos? b)
                  (recur (inc i) nil (conj (if (= :raw (:kind cur)) steps steps) (assoc cur :st b)))
                  (recur (inc i) cur steps)))
              :else
              (let [steps (if (and cur (or (= :note (:kind cur)) (#{0xF8 0xF9} a))) (conj steps cur) steps)
                    cur   (if (and cur (= :raw (:kind cur)) (not (#{0xF8 0xF9} a))) cur {:kind :raw :st 0 :hits {} :extra [] :raw []})
                    cur   (-> cur (update :raw conj r) (cond-> (< a 0xF6) (update :st + b)))
                    nxt   (at (inc i))
                    end?  (or (and (= a 0xF7) (not= (nxt 0) 0xF7)) (#{0xF8 0xF9} a) (< (nxt 0) 0x80) (pos? b))]
                (if end?
                  (recur (inc i) nil (conj steps cur))
                  (recur (inc i) cur steps))))))))))

(defn pattern->records
  "パターン → 小節のレコード (小節線で終わる) (pattor)。
   ノートのステップは、鳴る楽器を表の順に [キー 0 GT VEL] で並べ (無ければ休符 [0 ST 0 0])、
   表に無い音を足して、最後の行に ST を入れる。コマンドのステップはそのまま"
  [{:keys [steps]} assign]
  (conj
   (vec (mapcat (fn [{:keys [kind st hits extra raw]}]
                  (if (= kind :raw)
                    raw
                    (let [notes (vec (for [i (range 32) :let [[v g] (hits i)] :when (and v (pos? v))]
                                       [(second (assign i)) 0 (or g 1) v]))
                          rs (into (if (and (empty? notes) (empty? extra)) [[0 st 0 0]] notes) extra)]
                      (if (or (pos? st) (seq rs))
                        (assoc-in rs [(dec (count rs)) 1] st)
                        []))))
                steps))
   [0xFD 0 0 0]))

(defn replace-measure
  "小節 meas の中身 (小節線まで) を new-recs に置き換える。SAME の小節は変えない。
   空の最後の小節 (END の直前) なら末尾に足す"
  [recs meas new-recs]
  (let [recs (vec recs) n (count recs)
        ad   (or (polysted-edit/meas-add recs meas) n)
        r    (get recs ad)]
    (if (and r (= 0xFC (r 0)))
      recs
      (let [end (loop [j ad] (cond (>= j n) n (> ((recs j) 0) 0xFB) (inc j) :else (recur (inc j))))]
        (polysted-edit/renumber-measures (vec (concat (subvec recs 0 ad) new-recs (subvec recs end))))))))

;; ---- 簡易チェック (期待値は REDIT.C を手で追って求めた) ----
(defn run-checks []
  (let [as default-assign
        bd 36 sd 38 hh 42            ; Acou BD=15, Acou SD=14, Clsd HiHat=8
        recs [[bd 0 1 112] [hh 12 1 84] [hh 12 1 56] [sd 0 1 127] [hh 12 1 84] [0 12 0 0] [0xFD 0 0 1]]
        p (measure->pattern recs 1 as 192 12)
        checks
        [["強さ: ベロシティ → 1〜9" (mapv vel->level [0 1 14 15 112 127 200]) [0 1 1 2 8 9 9]]
         ["小節 → ステップ (和音・休符)"
          [(count (:steps p)) (mapv :st (:steps p)) (get-in p [:steps 0 :hits]) (get-in p [:steps 2 :hits])]
          [4 [12 12 12 12] {15 [112 1] 8 [84 1]} {14 [127 1] 8 [84 1]}]]
         ["ステップ → 小節 (表の順、最後に ST、小節線)"
          (pattern->records p as)
          [[hh 0 1 84] [bd 12 1 112] [hh 12 1 56] [hh 0 1 84] [sd 12 1 127] [0 12 0 0] [0xFD 0 0 0]]]
         ["空の最後の小節は 192÷12 = 16 の空ステップ"
          (let [q (measure->pattern [[bd 48 1 100] [0xFD 0 0 1]] 2 as 192 12)] [(count (:steps q)) (:st (first (:steps q)))])
          [16 12]]
         ["コントローラの行は :raw のステップで保つ"
          (pattern->records (measure->pattern [[0xEB 0 7 100] [bd 48 1 100] [0xFD 0 0 1]] 1 as 192 12) as)
          [[0xEB 0 7 100] [bd 48 1 100] [0xFD 0 0 0]]]
         ["表に無いキーの音は :extra で保つ"
          (pattern->records (measure->pattern [[99 0 5 90] [bd 48 1 100] [0xFD 0 0 1]] 1 as 192 12) as)
          [[bd 0 1 100] [99 48 5 90] [0xFD 0 0 0]]]
         ["小節の置き換え (番号を振り直す)、SAME は変えない"
          [(replace-measure [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2]] 2 [[bd 96 1 100] [0xFD 0 0 0]])
           (replace-measure [[60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]] 2 [[bd 96 1 100] [0xFD 0 0 0]])]
          [[[60 48 46 64] [0xFD 0 0 1] [bd 96 1 100] [0xFD 0 0 2]]
           [[60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]]]]
         ["空の表は SC55.DEF の既定、表があればそれ"
          [(first (effective-assign (vec (repeat 32 ["" 0 0])))) (first (effective-assign [["Kick" 35 2]]))]
          [["Hand Clap" 39 1] ["Kick" 35 2]]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted-rhythm: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(run-checks)
