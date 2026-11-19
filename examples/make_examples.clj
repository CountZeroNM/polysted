;; examples/make_examples.clj -- サンプル曲 (examples/*.edn) を作り直すスクリプト
;;   clojure -M examples/make_examples.clj        (プロジェクトのルートから)
;;
;; 曲は polysted.session の純粋関数 (insert-mml・insert-measure・insert-same・insert-ctrl …) で組み立てる。
;; REPL の s/mml や s/bar と同じ関数なので、REPL で 1 行ずつ打っても同じ曲になる。
;; 1 小節は要素のベクタで書く: 文字列 = MML、[:ctl 記号 ST 値…] = コントローラ、[:recs [[…]]] = レコードそのまま、
;; [:rep-start] / [:rep-end n] = リピート。小節の終わりに小節線を入れる。[:same j] は小節 j の SAME (それ自体が 1 小節)。
(ns make-examples
  (:require [polysted.session :as ss]
            [polysted.edn :as edn]))

(defn- item [s x]
  (cond
    (string? x) (ss/insert-mml s x)
    :else (let [[op & args] x]
            (case op
              :ctl       (apply ss/insert-ctrl s args)
              :recs      (ss/insert-recs s (first args))
              :rep-start (ss/insert-repeat-start s)
              :rep-end   (ss/insert-repeat-end s (first args))))))

(defn- measure [s m]
  (if (= :same (first m))
    (ss/insert-same s (second m))
    (ss/insert-measure (reduce item s m))))

(defn- check-msg [s where]
  (when-let [m (:msg s)] (throw (ex-info (str where ": " m) {})))
  s)

(defn track
  "s に新しいトラック (先頭は既存の TR1) を作り、小節を順に入れる"
  [s {:keys [first? ch comment rhythm? measures]}]
  (let [s (if first? (assoc-in s [:song :tracks 0 :ch] ch) (ss/add-track s ch))
        tr (:tr s)
        s (-> s
              (assoc-in [:song :tracks tr :comment] comment)
              (assoc-in [:song :tracks tr :rhythm?] (boolean rhythm?)))]
    (reduce (fn [s [i m]] (check-msg (measure s m) (str comment " の " (inc i) " 小節目")))
            s (map-indexed vector measures))))

(defn song [{:keys [title memo tempo key time-sig tracks]}]
  (let [s (-> (ss/new-session)
              (assoc-in [:song :title] title)
              (assoc-in [:song :memo] memo)
              (assoc-in [:song :tempo] tempo)
              (assoc-in [:song :key] key)
              (assoc-in [:song :time-sig] time-sig))]
    (:song (reduce track s (cons (assoc (first tracks) :first? true) (rest tracks))))))

;; ------------------------------------------------------------
;; 1. きらきら星 (フランス民謡「Ah! vous dirai-je, maman」)。SAME で同じ小節を指す
;; ------------------------------------------------------------
(def kirakira
  (let [C "o3 'ceg'96,92", F "o3 'cfa'96,92", G "o3 'dgb'96,92"]
    {:title "きらきら星 (フランス民謡) - polysted のサンプル"
     :memo ["フランス民謡 Ah! vous dirai-je, maman" "編曲: polysted のサンプル (MIT)"
            "TR1 メロディ / TR2 和音 / TR3 ベース" "7〜12 小節目は SAME (小節複写)"]
     :tempo 100 :key 0 :time-sig [4 4]
     :tracks
     [{:ch 1 :comment "Melody (Piano)"
       :measures [[[:ctl "G" 0 1] [:ctl "V" 0 100] "v100 l4 o4 c c g g"]
                  ["l4 o4 a a l2 g"] ["l4 o4 f f e e"] ["l4 o4 d d l2 c"]
                  ["l4 o4 g g f f"] ["l4 o4 e e l2 d"]
                  [:same 5] [:same 6] [:same 1] [:same 2] [:same 3] [:same 4]]}
      {:ch 2 :comment "Chords (Strings)"
       :measures [[[:ctl "G" 0 49] [:ctl "V" 0 80] [:ctl "N" 0 40] (str "v70 " C " " C)]
                  [(str F " " C)] [(str F " " C)] [(str G " " C)]
                  [(str C " " F)] [(str C " " G)]
                  [:same 5] [:same 6] [:same 1] [:same 2] [:same 3] [:same 4]]}
      {:ch 3 :comment "Bass"
       :measures [[[:ctl "G" 0 33] [:ctl "V" 0 100] [:ctl "N" 0 88] "v90 l2 o2 c c"]
                  ["l2 o2 f c"] ["l2 o2 f c"] ["l2 o2 g c"]
                  ["l2 o2 c f"] ["l2 o2 c g"]
                  [:same 5] [:same 6] [:same 1] [:same 2] [:same 3] [:same 4]]}]}))

;; ------------------------------------------------------------
;; 2. メヌエット ト長調 (BWV Anh. 114、クリスティアン・ペツォールト作とされる)。
;;    調号 (ト長調) で f が F♯ になる。3/4 拍子、全体をリピート 2 回
;; ------------------------------------------------------------
(def minuet
  (let [dh (fn [k vel] [:recs [[k 144 140 vel]]])]          ; 付点 2 分音符 (MML の l では書けない長さ)
    {:title "メヌエット ト長調 BWV Anh.114 - polysted のサンプル"
     :memo ["メヌエット ト長調 BWV Anh. 114 (ペツォールト作とされる)" "編曲: polysted のサンプル (MIT)、左手は簡単にした"
            "調号 G major: MML の f は F# になる" "1〜16 小節をリピート 2 回"]
     :tempo 120 :key 1 :time-sig [3 4]
     :tracks
     [{:ch 1 :comment "Right hand (Harpsichord)"
       :measures [[[:ctl "G" 0 7] [:ctl "V" 0 100] [:rep-start] "v96 l4 o5 d l8 o4 g a b o5 c"]
                  ["l4 o5 d o4 g g"] ["l4 o5 e l8 c d e f"] ["l4 o5 g o4 g g"]
                  ["l4 o5 c l8 d c o4 b a"] ["l4 o4 b l8 o5 c o4 b a g"]
                  ["l4 o4 f l8 g a b g"] [(dh 69 96)]
                  ["l4 o5 d l8 o4 g a b o5 c"] ["l4 o5 d o4 g g"] ["l4 o5 e l8 c d e f"] ["l4 o5 g o4 g g"]
                  ["l4 o5 c l8 d c o4 b a"] ["l4 o4 b l8 o5 c o4 b a g"]
                  ["l4 o4 a l8 b a g f"] [(dh 67 96) [:rep-end 2]]]}
      {:ch 2 :comment "Left hand (Harpsichord)"
       :measures (let [bass [55 59 60 59 57 55 50 50 55 59 60 59 57 55 50 43]]
                   (vec (map-indexed
                         (fn [i k]
                           (cond-> []
                             (zero? i) (into [[:ctl "G" 0 7] [:ctl "V" 0 90] [:rep-start]])
                             true      (conj (dh k 80))
                             (= i 15)  (conj [:rep-end 2])))
                         bass)))}]}))

;; ------------------------------------------------------------
;; 3. エイトビート (polysted のオリジナル)。リズムトラック・コントローラ・SAME
;; ------------------------------------------------------------
(defn- drum-bar
  "8 分音符 8 ステップ。steps は各ステップの [キー ベロシティ] の列 (空なら休符)"
  [steps]
  [[:recs (vec (mapcat (fn [hits]
                         (if (empty? hits)
                           [[0 24 0 0]]
                           (let [n (count hits)]
                             (map-indexed (fn [i [k v]] [k (if (= i (dec n)) 24 0) 20 v]) hits))))
                       steps))]])

(def eight-beat
  (let [bd [36 110], sd [38 100], hh [42 80], ho [46 80], cr [49 100]
        basic [[bd hh] [hh] [sd hh] [hh] [bd hh] [bd hh] [sd hh] [ho]]
        fill  [[bd hh] [hh] [sd hh] [hh] [sd] [sd] [sd] [sd]]
        crash (fn [steps] (update steps 0 #(cons cr (remove #{cr} %))))]
    {:title "Eight Beat - polysted のサンプル"
     :memo ["polysted のオリジナル (MIT)" "TR1 リズム (RHY ON、ch 10) / TR2 ベース / TR3 エレピ"
            "リズムパターン編集 (Ctrl-G) で TR1 を開ける" "2〜3・5〜8 小節目は SAME"]
     :tempo 120 :key 0 :time-sig [4 4]
     :tracks
     [{:ch 10 :rhythm? true :comment "Drums"
       :measures [(drum-bar (crash basic))
                  [:same 1] [:same 1]
                  (drum-bar fill)
                  (drum-bar (crash basic))
                  [:same 1] [:same 1] [:same 4]]}
      {:ch 2 :comment "Bass (Finger)"
       :measures [[[:ctl "G" 0 34] [:ctl "V" 0 100] [:ctl "N" 0 60] "v100 l8 o2 c c c c c c c c"]
                  ["l8 o2 a a a a a a a a"] ["l8 o2 f f f f f f f f"] ["l8 o2 g g g g g g a b"]
                  [:same 1] [:same 2] [:same 3] [:same 4]]}
      {:ch 3 :comment "E.Piano"
       :measures [[[:ctl "G" 0 5] [:ctl "V" 0 85] [:ctl "N" 0 76] "v80 l8 r o4 'ceg'24,18 r 'ceg'24,18 r 'ceg'24,18 r 'ceg'24,18"]
                  ["l8 r o4 'cea'24,18 r 'cea'24,18 r 'cea'24,18 r 'cea'24,18"]
                  ["l8 r o4 'cfa'24,18 r 'cfa'24,18 r 'cfa'24,18 r 'cfa'24,18"]
                  ["l8 r o3 'bdg'24,18 r 'bdg'24,18 r 'bdg'24,18 r 'bdg'24,18"]
                  [:same 1] [:same 2] [:same 3] [:same 4]]}]}))

(doseq [[file spec] [["examples/01_kirakira.edn" kirakira]
                     ["examples/02_minuet_g.edn" minuet]
                     ["examples/03_eight_beat.edn" eight-beat]]]
  (edn/save-song! (song spec) file)
  (println "wrote" file))
