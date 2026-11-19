;; polysted_score.clj — 楽譜表示の計算 (原作 SCORE.C gra_gakufu・st_cv・put_sharp、KEY_SUB.C key_shift)。純粋関数のみ。描画は polysted_gui.clj。
;;
;; 原作の楽譜表示 (編集画面の Ctrl-G で切り替える補助表示の 1 つ) で確認したこと:
;;   - ト音記号とヘ音記号の大譜表。横の位置は ST に比例 (タイムベース 48 換算で 1 tick = 1 ドット)。
;;   - 1 つの「ステップ」= ST=0 で続く音 (和音) と、ST>0 の行。音の長さには、続くコントローラの行の ST を足す。
;;     音の無いステップ (休符だけ、ST>0 のコントローラ) は休符で、続く休符の ST もまとめる。
;;   - 音符の種類は長さで 12 段階 (st_cv: 192 144 96 72 48 36 24 18 12 9 6 3 を中心に)。
;;   - 高さは調号 key_shi で決める: 調で変化する音はその幹音の位置に臨時記号なし、調で変化する幹音を
;;     そのまま弾くならナチュラル、それ以外の黒鍵はシャープ (key_shift)。キー 10〜107 の音だけ描く。
;;   - 調は曲の調 (bkey) から始まり、0xF5 (調の変更) でその小節の残りが変わり、小節線で曲の調に戻る。
;;     リズムトラック (RHY ON) は調号なし。
;;   - 小節線 (0xFD・SAME 0xFC) は縦線、リピート (0xF9 / 0xF8) は括弧。小節線などの後は 2 ドット、
;;     途中の調の変更は調号のために 44 ドット空ける。
;; 本実装の違い【設計判断】: 表示の開始位置 (原作はカーソルの約 160 tick 前から) と最初の長いステップの圧縮は
;;   GUI 側でスクロールにする。0xF6 (コメント) は描かない。0xF5 の調の値はタイムベースで割らない
;;   (原作は ST と同じく割っている)。
;; 読み込み: (load-file "polysted_score.clj")  ※ polysted_edit.clj の後
(ns polysted-score
  (:require [polysted-edit]))

(def onp-ps "半音 → 幹音の段 (C=0 … B=6)" [0 0 1 1 2 3 3 4 4 5 5 6])
(def ^:private shfl [0 1 0 1 0 0 1 0 1 0 1 0])

;; KEY_SUB.C key_shi: 調 (skey & 15) ごとに、その半音が調号で +1 (シャープ) / -1 (フラット) されるか
(def key-shi
  [[0 0 0 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 1 0 0 0 0 0 0] [1 0 0 0 0 1 0 0 0 0 0 0] [1 0 0 0 0 1 0 1 0 0 0 0]
   [1 0 1 0 0 1 0 1 0 0 0 0] [1 0 1 0 0 1 0 1 0 1 0 0] [1 0 1 0 1 1 0 1 0 1 0 0] [1 0 1 0 1 1 0 1 0 1 0 1]
   [0 0 0 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0 0 0 0 -1] [0 0 0 0 -1 0 0 0 0 0 0 -1] [0 0 0 0 -1 0 0 0 0 -1 0 -1]
   [0 0 -1 0 -1 0 0 0 0 -1 0 -1] [0 0 -1 0 -1 0 0 -1 0 -1 0 -1] [-1 0 -1 0 -1 0 0 -1 0 -1 0 -1] [-1 0 -1 0 -1 -1 0 -1 0 -1 0 -1]])

;; 調号の位置 (幹音の段)。put_sharp の sharp_yp をト音記号の段に直したもの (ヘ音記号は 14 段下)
(def sharp-steps "シャープの順 F5 C5 G5 D5 A4 E5 B4" [45 42 46 43 40 44 41])
(def flat-steps "フラットの順 B4 E5 A4 D5 G4 C5 F4" [41 44 40 43 39 42 38])

(defn key-sig "調の値 → [:sharp か :flat 個数] (put_sharp: 個数は skey&7、(skey&15)<8 ならシャープ)" [skey]
  [(if (< (bit-and skey 15) 8) :sharp :flat) (bit-and skey 7)])

(defn st-type
  "長さ (タイムベース 48 換算) → 音符の種類 0〜11 (st_cv)。
   0 全 1 付点2分 2 2分 3 付点4分 4 4分 5 付点8分 6 8分 7 付点16分 8 16分 9 付点32分 10 32分 11 64分"
  [st]
  (let [ts [[168 0] [120 1] [84 2] [60 3] [42 4] [30 5] [21 6] [15 7] [11 8] [8 9] [5 10]]]
    (or (some (fn [[t k]] (when (>= st t) k)) ts) 11)))

(defn note-pos
  "キー → {:f 幹音の段 (オクターブ×7 + C〜B) :acc nil / :sharp / :natural} (key_shift)"
  [key skey]
  (let [f (bit-shift-right (* key 43) 9) a (- key (* f 12)) row (key-shi (bit-and skey 15))]
    (if-not (zero? (row a))
      {:f (+ (onp-ps a) (* f 7)) :acc :natural}
      (let [[f a] (let [n (mod (dec a) 12)] (if (pos? (row n)) [(if (and (= n 11) (pos? f)) (dec f) f) n] [f a]))
            [f a] (let [n (mod (inc a) 12)] (if (neg? (row n)) [(if (and (zero? n) (< f 10)) (inc f) f) n] [f a]))]
        {:f (+ (onp-ps a) (* f 7)) :acc (when (= 1 (shfl a)) :sharp)}))))

(defn score-items
  "トラックのレコード → 楽譜の要素 (gra_gakufu)。x・len はタイムベース 48 換算の tick。
   要素: {:kind :chord :notes [{:key :f :acc}] | :rest | :bar (:same?) | :rep-start | :rep-end | :key (:skey)
          :x :len (音符と休符) :type (st-type) :from :to (対応する行の範囲)}
   opts: {:timebase :key 曲の調 :rhythm? }  => {:items [...] :end 最後の x}"
  [recs {:keys [timebase key rhythm?]}]
  (let [recs (vec recs) n (count recs)
        tb (max 1 (quot (or timebase 48) 48))
        dv (fn [v] (if (> tb 1) (quot v tb) v))
        base (if rhythm? 0 (or key 0))]
    (loop [i 0 x 0 skey base kno [] from 0 items []]
      (if (or (>= i n) (= 0xFE ((recs i) 0)))
        {:items items :end x}
        (let [[a b c d] (recs i)]
          (cond
            (#{0xF6 0xF7} a) (recur (inc i) x skey kno from items)
            (> a 0xF4)
            (let [item (cond (= a 0xF9) {:kind :rep-start} (= a 0xF8) {:kind :rep-end}
                             (>= a 0xFC) {:kind :bar :same? (= a 0xFC)} :else {:kind :key :skey (if rhythm? 0 b)})
                  gap (if (and (= a 0xF5) (pos? x)) 44 2)]
              (recur (inc i) (+ x gap) (cond (>= a 0xFC) base (= a 0xF5) (if rhythm? 0 b) :else skey)
                     [] (inc i) (conj items (assoc item :x x :gap gap :from i :to i))))
            :else
            (let [kno (if (< a 128) (conj kno (if (or (zero? c) (zero? d)) :rest a)) kno)
                  b (dv b)]
              (if-not (pos? b)
                (recur (inc i) x skey kno from items)
                (let [rest? (and (<= (count kno) 1) (every? #{:rest} kno))
                      ;; 続く行: コントローラの ST は足す。休符だけのステップなら続く休符もまとめる
                      [bb j] (loop [j (inc i) bb 0]
                               (if (>= j n)
                                 [bb j]
                                 (let [[e f g h] (recs j)]
                                   (cond (#{0xF6 0xF7} e) (recur (inc j) bb)
                                         (or (< e 0x80) (>= e 0xF0))
                                         (if (and (< e 0x80) (or (zero? g) (zero? h)) rest?) (recur (inc j) (+ bb f)) [bb j])
                                         :else (recur (inc j) (+ bb f))))))
                      len (+ b (dv bb))
                      ks (remove #{:rest} kno)
                      item (if (empty? ks)
                             {:kind :rest}
                             {:kind :chord :notes (vec (for [k ks :when (< 9 k 108)] (assoc (note-pos k skey) :key k)))})]
                  (recur j (+ x len) skey [] j
                         (conj items (assoc item :x x :len len :type (st-type len) :from from :to (dec j)))))))))))))

(defn item-at "行 pos を含む要素 (無ければ nil。END なら nil)" [items pos]
  (first (filter #(<= (:from %) pos (:to %)) items)))

(defn cursor-x "カーソルの x (音符・休符はその中央、ほかは要素の x、END なら最後)" [{:keys [items end]} pos]
  (if-let [it (item-at items pos)]
    (+ (:x it) (quot (or (:len it) 0) 2))
    end))

;; ---- 表示の横位置 ----
;; 原作は ST に比例 (1 tick = 1 ドット) で、短い音符が詰まる。見やすさのため、要素の幅を
;; 「最小の幅 (符頭・旗・臨時記号が収まる幅) と、長さ × px の大きいほう」にし、音符は区間の頭に置く【設計判断】
(defn layout
  "要素に表示の位置を付ける。opts (ピクセル): :px (1 tick の幅) :note-w (音符の最小の幅) :acc-w (臨時記号の分)
   :head-w (符頭の幅) :rest-w (休符の最小の幅) :bar-w (小節線などの幅) :key-w (調号 1 つの幅)
   => {:items 要素に :dx (左端) :dw (幅) :hx (音符・休符の中心) を足したもの :dend 最後}"
  [{:keys [items]} {:keys [px note-w acc-w head-w rest-w bar-w key-w]}]
  (let [width (fn [{:keys [kind len notes skey]}]
                (case kind
                  :chord (max (* px len) (+ note-w (if (some :acc notes) acc-w 0)))
                  :rest  (max (* px len) rest-w)
                  :key   (+ bar-w (* key-w (second (key-sig skey))))
                  bar-w))
        [its dx] (reduce (fn [[acc dx] it]
                           (let [dw (width it)
                                 hx (case (:kind it)
                                      :chord (+ dx (if (some :acc (:notes it)) acc-w 0) (/ head-w 2.0))
                                      :rest (+ dx (/ rest-w 2.0))
                                      nil)]
                             [(conj acc (cond-> (assoc it :dx dx :dw dw) hx (assoc :hx hx))) (+ dx dw)]))
                         [[] 0.0] items)]
    {:items its :dend dx}))

(defn cursor-dx "カーソルの表示の x (音符・休符はその中心、ほかは左端、END なら最後)" [{:keys [items dend]} pos]
  (if-let [it (item-at items pos)] (or (:hx it) (:dx it)) dend))

(defn hit-dx "表示の x → その要素の最初の行 (最後より後なら最後の要素の次の行、要素が無ければ 0)" [{:keys [items]} dx]
  (if-let [it (last (filter #(<= (:dx %) dx) items))]
    (if (< dx (+ (:dx it) (:dw it))) (:from it) (inc (:to it)))
    0))

;; 楽譜の x (タイムベース 48 換算の tick と小節線などの間) → 表示の x。要素の中は比例で割り振る
(defn x->dx [{:keys [items dend]} px]
  (let [its (vec items)
        end-x (if-let [l (peek its)] (+ (:x l) (or (:len l) (:gap l) 0)) 0)]
    (fn [t]
      (if (>= t end-x)
        (+ dend (* px (- t end-x)))
        (let [it (or (last (filter #(<= (:x %) t) its)) (first its))
              span (max 1 (or (:len it) (:gap it) 1))]
          (+ (:dx it) (* (:dw it) (/ (double (- t (:x it))) span))))))))

;; ---- コントローラのグラフ (SCORE.C gra_special) ----
;; 6 つの値 (ピッチベンドは上位 7 ビット) を段の形で、音はベロシティの高さで GT の長さの棒で描く。
;; 前に値が無いときの値はピッチ 64 (中央)・ボリューム 100・エクスプレッション 127・モジュレーション 0・
;; データエントリ 0・パン 64。SAME の小節を通るとき、参照先の小節の最後の値にする (原作は表示の始まりの値を
;; 探すときに SAME の参照先をたどる)。横の位置は楽譜と同じ (小節線などの間も同じ)【設計判断: 楽譜と並べるため】
(def graph-ctls "グラフの値と既定 (原作の描く順)" [[:pitch 64] [:vol 100] [:expr 127] [:modu 0] [:entry 0] [:pan 64]])
(def ^:private cc->ctl {7 :vol 1 :modu 11 :expr 6 :entry 10 :pan})

(defn- rec-ctl "行がグラフの値を変えるなら [名前 値]" [[a _ c d]]
  (cond (= a 0xEE) [:pitch (bit-shift-right (+ (* d 128) c) 7)]
        (and (= a 0xEB) (cc->ctl c)) [(cc->ctl c) d]
        :else nil))

(defn ctrl-graph
  "トラックのレコード → {:changes [{:x :ctl :v}] (値が変わる位置) :vels [{:x :len :vel :legato? :idx}] :bars [x] :end}
   x・len はタイムベース 48 換算 (楽譜と同じ x)"
  [recs {:keys [timebase]}]
  (let [recs (vec recs) n (count recs)
        tb (max 1 (quot (or timebase 48) 48))
        dv (fn [v] (if (> tb 1) (quot v tb) v))
        step-of (fn [i] (or (some (fn [j] (let [[a b] (recs j)] (when (and (pos? b) (< a 0xF7)) b))) (range i n)) 0))]
    (loop [i 0 x 0 changes [] vels [] bars []]
      (if (or (>= i n) (= 0xFE ((recs i) 0)))
        {:changes changes :vels vels :bars bars :end x}
        (let [[a b c d :as r] (recs i)]
          (cond
            (#{0xF6 0xF7} a) (recur (inc i) x changes vels bars)
            (> a 0xF4)
            (let [same-vals (when (= a 0xFC)
                              (when-let [ad (polysted-edit/meas-add recs (+ b (* 256 (bit-and c 3))))]
                                (keep rec-ctl (take-while #(< (% 0) 0xFC) (subvec recs (min ad n))))))
                  gap (if (and (= a 0xF5) (pos? x)) 44 2)]
              (recur (inc i) (+ x gap) (into changes (map (fn [[k v]] {:x x :ctl k :v v}) same-vals)) vels
                     (if (>= a 0xFC) (conj bars x) bars)))
            :else
            (let [changes (if-let [[k v] (rec-ctl r)] (conj changes {:x x :ctl k :v v}) changes)
                  vels (if (and (< a 128) (pos? c) (pos? d))
                         (conj vels {:x x :len (dv c) :vel d :legato? (> c (step-of i)) :idx i})
                         vels)]
              (recur (inc i) (+ x (dv b)) changes vels bars))))))))

;; ---- 簡易チェック (期待値は SCORE.C・KEY_SUB.C を手で追って求めた) ----
(defn run-checks []
  (let [recs [[0xEB 0 7 100] [60 0 46 64] [64 48 46 64] [0xEB 12 10 64] [0 24 0 0] [0 12 0 0] [0xFD 0 0 1]
              [67 96 90 64] [0xF5 1 0 0] [66 24 20 64] [0xFC 1 0 0] [0xFE 0 0 0]]
        sc (score-items recs {:timebase 48 :key 0})
        checks
        [["音符の種類 (st_cv の境目)"
          (mapv st-type [192 168 167 144 96 72 60 59 48 42 41 36 24 18 12 9 6 3 2])
          [0 0 1 1 2 3 3 4 4 4 5 5 6 7 8 9 10 11 11]]
         ["高さと臨時記号: ハ長調の C4 C#4 E4、ト長調の F#4 (記号なし) と F4 (ナチュラル)、ヘ長調の Bb4 (記号なし) と B4"
          [(note-pos 60 0) (note-pos 61 0) (note-pos 64 0) (note-pos 66 1) (note-pos 65 1) (note-pos 70 9) (note-pos 71 9)]
          [{:f 35 :acc nil} {:f 35 :acc :sharp} {:f 37 :acc nil} {:f 38 :acc nil} {:f 38 :acc :natural}
           {:f 41 :acc nil} {:f 41 :acc :natural}]]
         ["要素: 和音 (コントローラの ST を足す)・休符 (続く休符をまとめる)・小節線・調の変更・SAME"
          (mapv #(dissoc % :notes) (:items sc))
          [{:kind :chord :x 0 :len 60 :type 3 :from 0 :to 3} {:kind :rest :x 60 :len 36 :type 5 :from 4 :to 5}
           {:kind :bar :same? false :x 96 :gap 2 :from 6 :to 6} {:kind :chord :x 98 :len 96 :type 2 :from 7 :to 7}
           {:kind :key :skey 1 :x 194 :gap 44 :from 8 :to 8} {:kind :chord :x 238 :len 24 :type 6 :from 9 :to 9}
           {:kind :bar :same? true :x 262 :gap 2 :from 10 :to 10}]]
         ["和音の音・調の変更の後 (F#4 は記号なし)・最後の x・カーソルの x"
          [(:notes ((:items sc) 0)) (:notes ((:items sc) 5)) (:end sc) (cursor-x sc 1) (cursor-x sc 6) (cursor-x sc 11)]
          [[{:f 35 :acc nil :key 60} {:f 37 :acc nil :key 64}] [{:f 38 :acc nil :key 66}] 264 30 96 264]]
         ["タイムベース 96 は ST を半分に、リズムトラックは調号なし、調号の向き"
          [(:type (first (:items (score-items [[60 96 90 64]] {:timebase 96 :key 0}))))
           (:acc (first (:notes (first (:items (score-items [[66 24 20 64]] {:timebase 48 :key 1 :rhythm? true}))))))
           (key-sig 3) (key-sig 10)]
          [4 :sharp [:sharp 3] [:flat 2]]]
         ["表示の位置: 短い音は最小の幅、臨時記号の分、長い音は長さ × px、調号の数、カーソルとクリック"
          (let [lay (layout {:items [{:kind :chord :len 12 :notes [{:f 35}] :from 0 :to 0}
                                     {:kind :chord :len 12 :notes [{:f 35 :acc :sharp}] :from 1 :to 1}
                                     {:kind :chord :len 96 :notes [{:f 35}] :from 2 :to 2}
                                     {:kind :bar :from 3 :to 3} {:kind :key :skey 2 :from 4 :to 4}
                                     {:kind :rest :len 12 :from 5 :to 5}]}
                            {:px 2 :note-w 50 :acc-w 20 :head-w 20 :rest-w 30 :bar-w 10 :key-w 15})]
            [(mapv (juxt :dx :dw) (:items lay)) (:dend lay) (cursor-dx lay 1) (cursor-dx lay 4) (cursor-dx lay 9)
             (hit-dx lay 75) (hit-dx lay 400) (hit-dx lay -5)])
          [[[0.0 50] [50.0 70] [120.0 192] [312.0 10] [322.0 40] [362.0 30]] 392.0 80.0 322.0 392.0 1 6 0]]
         ["楽譜の x → 表示の x: 要素の中は比例、最後より後は px で延ばす"
          (let [f (x->dx {:items [{:x 0 :len 48 :dx 0.0 :dw 100} {:x 48 :gap 2 :dx 100.0 :dw 10} {:x 50 :len 12 :dx 110.0 :dw 50}]
                          :dend 160.0} 2)]
            (mapv f [0 24 48 49 50 56 62 70]))
          [0.0 50.0 100.0 105.0 110.0 135.0 160.0 176.0]]
         ["コントローラのグラフ: 値の変化 (ピッチは上位 7 ビット)、ベロシティの棒 (GT>ST はつながる)、小節線、SAME の値"
          (ctrl-graph [[0xEB 0 7 90] [60 0 30 64] [64 48 60 80] [0xEE 12 0 80] [0 36 0 0] [0xFD 0 0 1]
                       [0xEB 0 10 20] [67 96 90 100] [0xFD 0 0 2] [0xFC 1 0 0] [0xFE 0 0 0]] {:timebase 48})
          {:changes [{:x 0 :ctl :vol :v 90} {:x 48 :ctl :pitch :v 80} {:x 98 :ctl :pan :v 20}
                     {:x 196 :ctl :vol :v 90} {:x 196 :ctl :pitch :v 80}]
           :vels [{:x 0 :len 30 :vel 64 :legato? false :idx 1} {:x 0 :len 60 :vel 80 :legato? true :idx 2}
                  {:x 98 :len 90 :vel 100 :legato? false :idx 7}]
           :bars [96 194 196] :end 198}]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted-score: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(run-checks)
