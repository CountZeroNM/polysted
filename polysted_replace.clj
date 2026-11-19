;; polysted_replace.clj — 置換・削除・検索マクロ (原作の F3 / Shift+F3 / F4・F5)
;;
;; EDITS.C replace・rep_match・tai_check の移植。指定の書式 (マニュアル STED2.MAN の F3):
;;   検索{:置換}{,検索{:置換}…}   欄は NOTE(コード) / ST / GT / VEL の順。'|' で区切って複数を順に実行
;;   検索:  空=すべて (NOTE 欄はノートだけ) / 値 / 値>(未満) / 値<(より大きい) / a_b(範囲。片側省略可)
;;          C*(オクターブ不問) / !(否定) / &(タイの音だけ。!& はタイでない音) /
;;          /記号 (コントローラ。/S=全コントローラ、/ALL=全データ)
;;   置換:  値 (NOTE 欄は音名も可) / ^値 (絶対値) / +-*/% の式を左から順に (例 -64*1.2+64)、
;;          a_b は a〜b の乱数 / @0 @1 @2 は ST・GT・VEL の値を写す / /記号 (CC・アフタータッチの種類を変える)
;;   先頭の % は「元のデータを残し、同じ時刻に変換後を足す」、削除で先頭の @ は「消さずに同じ ST の休符にする」
;; 未対応 (エラーにする): $ の文字列検索、ST 欄の + - (和音の音数)、/CM (コメント)
;;
;; 読み込み: (load-file "polysted_replace.clj")  ※ロード時に run-checks が走る
(ns polysted-replace
  (:require [clojure.string :as str]
            [polysted-notename]))

;; ------------------------------------------------------------
;; 文字列 → 数
;; ------------------------------------------------------------
(defn str-val
  "str_val: 先頭の空白を飛ばし、末尾 H か先頭 $ なら16進、それ以外は atoi (数字が無ければ 0)"
  [s]
  (let [s (str/upper-case (str/triml (str s)))]
    (if (or (str/starts-with? s "$") (str/ends-with? s "H"))
      (let [h (re-find #"[0-9A-F]+" (str/replace s #"^\$" ""))] (if h (Long/parseLong h 16) 0))
      (if-let [m (re-find #"^[+-]?\d+" s)] (Long/parseLong (str/replace m #"^\+" "")) 0))))

(defn- atof [s] (if-let [m (re-find #"^[+-]?\d*\.?\d+" s)] (Double/parseDouble m) 0.0))

(defn- ctc
  "音名かキー番号 → キー (0〜127)。基準のオクターブは 4 (ctc の da=60)"
  [s]
  (polysted-notename/parse-token (str/trim s) {:last-key 60 :key-sig 0}))

;; ------------------------------------------------------------
;; 指定の解析
;; ------------------------------------------------------------
(defn- parse-cond
  "1欄の検索条件 → {:f 種類 :neg? :t :t2}。nm は欄 (0 = NOTE)"
  [nm s]
  (let [neg? (str/starts-with? s "!")
        s    (if neg? (subs s 1) s)
        num  (fn [x] (if (zero? nm) (ctc x) (str-val x)))]
    (cond
      (str/blank? s) {:f :all :neg? neg?}
      (str/includes? s "_")
      (let [[a b] (str/split s #"_" 2)
            t  (if (str/blank? a) -9999 (num a))
            t2 (if (str/blank? b) 9999 (num b))]
        (when (and t t2) {:f :range :neg? neg? :t (min t t2) :t2 (max t t2)}))
      (str/includes? s ">") (when-let [t (num (str/replace s ">" ""))] {:f :lt :neg? neg? :t t})
      (str/includes? s "<") (when-let [t (num (str/replace s "<" ""))] {:f :gt :neg? neg? :t t})
      (and (zero? nm) (str/includes? s "*"))
      (when-let [t (ctc (str/replace s "*" "0"))] {:f :oct :neg? neg? :t t})
      :else (when-let [t (num s)] {:f :eq :neg? neg? :t t}))))

(defn parse-spec
  "1つの指定 (| で区切った1つ) → {:recover? :add? :conds [4] :reps [4] :tie} か {:error 理由}。
   opts: {:spc-code (fn [記号] {:code :ctl} or nil) :prog-min 1}"
  [line {:keys [spc-code prog-min] :or {prog-min 1}}]
  (let [line     (str/trim line)
        recover? (str/starts-with? line "@")
        line     (if recover? (subs line 1) line)
        add?     (str/starts-with? line "%")
        line     (if add? (subs line 1) line)
        parts    (vec (take 4 (str/split line #"," -1)))
        parts    (into parts (repeat (- 4 (count parts)) ""))
        split1   (fn [p] (let [i (str/index-of p ":")] (if i [(subs p 0 i) (subs p (inc i))] [p ""])))
        pairs    (mapv split1 parts)
        tie      (some (fn [[nm [c _]]] (let [c (str/replace c "!" "")]
                                         (when (str/starts-with? c "&")
                                           {:field nm :neg? (str/starts-with? (first (pairs nm)) "!")})))
                       (map-indexed vector pairs))
        pairs    (mapv (fn [[c r]] [(str/replace c #"^(!?)&" "$1") r]) pairs)]
    (cond
      (str/starts-with? line "$") {:error "文字列の検索 ($) は未対応です"}
      (re-find #"^[+-]" (first (pairs 1))) {:error "ST 欄の + - (和音の音数) は未対応です"}
      :else
      (let [[c0 r0] (pairs 0)
            ctrl    (when (str/starts-with? c0 "/")
                      (let [sym (str/upper-case (subs c0 1))]
                        (cond
                          (= sym "S")   {:cond {:f :all-spc}}
                          (= sym "ALL") (if (and (str/blank? (second (pairs 2))) (str/blank? (second (pairs 3))))
                                          {:cond {:f :all-data}}
                                          {:error "/ALL では GT・VEL 欄の置換はできません"})
                          (= sym "CM")  {:error "/CM (コメント) は未対応です"}
                          :else (if-let [{:keys [code ctl]} (spc-code sym)]
                                  {:cond {:f :eq :t code} :ctl (when (>= ctl 0) ctl)}
                                  {:error (str "コントローラの記号が不正です: " c0)}))))]
        (if (:error ctrl)
          ctrl
          (let [conds (mapv (fn [nm [c _]] (if (and (zero? nm) ctrl) (:cond ctrl) (parse-cond nm c)))
                            (range 4) pairs)
                reps  (mapv second pairs)
                ;; コントローラの番号 (例 /V → CC7) は GT 欄の条件
                conds (if-let [ctl (:ctl ctrl)] (assoc conds 2 {:f :eq :t ctl}) conds)
                reps  (if (:ctl ctrl) (assoc reps 2 (if (str/starts-with? r0 "/") (reps 2) "")) reps)
                code  (get-in ctrl [:cond :t])
                ;; 置換側の /記号: CC・アフタータッチの種類を変える
                [reps err] (if (and code (str/starts-with? r0 "/") (#{0xEB 0xEA 0xED} code))
                             (if-let [{c2 :code l2 :ctl} (spc-code (subs r0 1))]
                               (if (#{0xEB 0xEA 0xED} c2)
                                 [(cond-> (assoc reps 0 (str c2))
                                    (>= l2 0) (assoc 2 (str l2))
                                    (and (= code 0xEA) (not= c2 0xEA)) (-> (assoc 3 "@1") (cond-> (neg? l2) (assoc 2 "0")))
                                    (and (not= code 0xEA) (= c2 0xEA)) (assoc 2 "@2" 3 "0"))
                                  nil]
                                 [reps "置換先のコントローラが不正です"])
                               [reps "置換先のコントローラの記号が不正です"])
                             [(if ctrl (assoc reps 0 "") reps) nil])
                ;; 音色・ベンドは値を VEL 欄に書いてもよい (GT 欄へ寄せる)
                [conds reps] (if (#{0xEC 0xEE} code)
                               [(if (= :all (:f (conds 2))) (assoc conds 2 (conds 3) 3 {:f :all}) conds)
                                (if (str/blank? (reps 2)) (assoc reps 2 (reps 3) 3 "") reps)]
                               [conds reps])
                conds (if (and (#{0xEC 0xE2} code) (= 1 prog-min) (#{:eq :lt :gt :range} (:f (conds 2))))
                        (update conds 2 (fn [c] (cond-> (update c :t dec) (:t2 c) (update :t2 dec))))
                        conds)]
            (cond
              err {:error err}
              (some nil? conds) {:error (str "指定に間違いが有ります: " line)}
              :else {:recover? recover? :add? add? :conds conds :reps reps :tie tie :code code})))))))

(defn parse-specs
  "'|' で区切った指定全体 → [指定...] か {:error}"
  [text opts]
  (let [specs (mapv #(parse-spec % opts) (str/split (str text) #"\|"))]
    (or (first (filter :error specs)) specs)))

;; ------------------------------------------------------------
;; 照合 (rep_match・tai_check)
;; ------------------------------------------------------------
(defn- match? [{:keys [f neg? t t2]} a m]
  (let [note-ok (or (not= m 0) (< a 128))]
    (if-not neg?
      (case f
        :eq (= a t) :all note-ok :lt (< a t) :gt (and (> a t) note-ok)
        :oct (and (< a 128) (= (mod a 12) (mod t 12)))
        :all-spc (and (> a 127) (or (< a 0xF0) (= a 0xF7)))
        :all-data (and (< a 0xFC) (or (< a 0xF6) (> a 0xF9)))
        :range (and (<= t a t2) note-ok)
        false)
      (case f
        :eq (and (not= a t) (or (not= m 0) (and (< t 128) (< a 128)) (and (< 127 t 0xF0) (< 127 a 0xF0))))
        :all (and (> a 127) (< a 0xFC) (or (< a 0xF6) (> a 0xF9)))
        :lt (and (>= a t) note-ok) :gt (<= a t)
        :oct (and (< a 128) (not= (mod a 12) (mod t 12)))
        :all-spc (< a 128)
        :range (and (not (<= t a t2)) note-ok)
        false))))

(defn tied?
  "tai_check: i 番のノートが次の同じキーの音へタイでつながるか"
  [recs i]
  (let [[a b c d] (recs i)]
    (boolean
     (when (and (< a 128) (> c b) (pos? d))
       (loop [j i sum 0]
         (when (< j (count recs))
           (let [r (recs j) da (r 0)]
             (cond
               (and (> j i) (= da a) (pos? (r 2)) (pos? (r 3))) (< sum c)
               (and (< da 0xF0) (>= (+ sum (r 1)) c)) false
               (#{0xFC 0xFE} da) true
               :else (recur (inc j) (if (< da 0xF0) (+ sum (r 1)) sum))))))))))

(defn- values-for-match
  "照合に使う [code ST GT VEL]。ベンドは GT 欄に符号付きの値。和音の途中の音は ST を後ろの音から取る"
  [recs i spec]
  (let [[a b c d] (recs i)
        [c d] (if (= a 0xEE) [(- (+ c (* 128 d)) 8192) 0] [c d])
        st-cond (get-in spec [:conds 1])
        b (if (and (< a 128) (zero? b) (#{:eq :lt :gt :range} (:f st-cond)) (pos? (or (:t st-cond) 0)))
            (or (some (fn [j] (let [r (recs j)] (cond (> (r 0) 127) 0 (pos? (r 1)) (r 1)))) (range (inc i) (count recs))) 0)
            b)]
    [a b c d]))

(defn matches?
  "i 番の行が指定に合うか"
  [recs i spec]
  (let [[a b c d] (values-for-match recs i spec)
        {:keys [conds tie]} spec]
    (and (match? (conds 0) a 0) (match? (conds 1) b 1) (match? (conds 2) c 2) (match? (conds 3) d 3)
         (or (nil? tie) (let [tt (tied? recs i)] (if (:neg? tie) (not tt) tt))))))

;; ------------------------------------------------------------
;; 置換の式
;; ------------------------------------------------------------
(defn- eval-rep
  "置換の文字列を今の値 cd に適用 (replace の式の解釈)。=> 新しい値"
  [rep cd j a mv ^java.util.Random rng prog-min]
  (cond
    (and (pos? j) (re-matches #"@[012]" rep)) (mv (- (int (.charAt ^String rep 1)) (int \0)))
    :else
    (loop [s rep cd cd]
      (if (str/blank? s)
        cd
        (let [si (first s)]
          (if (#{\+ \- \* \/ \%} si)
            (let [s1   (subs s 1)
                  ml   (atof s1)
                  s2   (str/replace s1 #"^\d+" "")
                  ml   (if (str/starts-with? s2 "_")
                         (let [hi (atof (subs s2 1))] (+ ml (Math/floor (* (.nextDouble rng) (inc (- hi ml))))))
                         ml)
                  cd   (case si
                         \+ (long (+ cd ml)) \- (long (- cd ml)) \* (long (* cd ml))
                         \/ (if (zero? ml) cd (long (/ cd ml)))
                         \% (long (* cd (/ (max 1 (min 300 ml)) 100.0))))
                  nxt  (some (fn [k] (when (#{\+ \- \* \/ \%} (.charAt ^String s1 k)) k)) (range 1 (count s1)))]
              (recur (if nxt (subs s1 nxt) "") cd))
            (let [s (str/replace s #"^\^" "")]
              (if (and (zero? j) (< a 128))
                (or (polysted-notename/parse-token (str/trim s) {:last-key 60 :key-sig 0}) cd)
                (cond-> (str-val s) (and (#{0xEC 0xE2} a) (= j 2) (= 1 prog-min)) dec)))))))))

(defn- clamp-field [a j cd]
  (cond
    (< a 128)   (max 0 (min (if (#{0 3} j) 127 255) cd))
    (= a 0xEE)  (if (> j 1) (max -8192 (min 8191 cd)) (max 0 (min 255 cd)))
    :else       (max 0 (min (if (and (> j 1) (not (#{0xF6 0x98 0xF7 0xE7} a))) 127 255) cd))))

(defn- replace-rec
  "1行に置換を当てる => [新しいレコード ST のあふれ (256 単位)]"
  [recs i spec rng prog-min]
  (let [r0 (recs i)
        a  (r0 0)
        [_ b c d] (values-for-match recs i spec)
        mv [b c d]
        skip-st? (and (< a 128) (zero? (r0 1))
                      (#{:eq :lt :gt :range} (get-in spec [:conds 1 :f]))
                      (pos? (or (get-in spec [:conds 1 :t]) 0)))]
    (loop [j 0 r r0 hi 0]
      (if (> j 3)
        [r hi]
        (if (and (= j 1) skip-st?)
          (recur (inc j) r hi)
          (let [code (r 0)
                cur  (if (and (= code 0xEE) (> j 1)) (- (+ (r 2) (* 128 (r 3))) 8192) (r j))
                cd   (eval-rep ((:reps spec) j) cur j code mv rng prog-min)
                [cd hi] (if (and (< code 0xF0) (= j 1) (> cd 255)) [(bit-and cd 255) (bit-shift-right cd 8)] [cd hi])
                cd   (clamp-field code j cd)
                r    (if (and (= code 0xEE) (> j 1))
                       (let [v (+ cd 8192)] (assoc r 2 (bit-and v 127) 3 (bit-shift-right v 7)))
                       (assoc r j cd))]
            (recur (inc j) r hi)))))))

;; ------------------------------------------------------------
;; 実行
;; ------------------------------------------------------------
(defn- run-one
  "1つの指定を [from, to) に当てる。mode: :replace / :delete。=> [新しい recs 新しい to 当たった数]"
  [recs from to spec mode rng prog-min]
  (let [hits (set (filter #(matches? recs % spec) (range from (min to (count recs)))))]
    (if (empty? hits)
      [recs to 0]
      (let [out (reduce
                 (fn [out i]
                   (let [r (recs i)]
                     (cond
                       (not (hits i)) (conj out [r])
                       (= mode :delete)
                       (conj out (if (and (:recover? spec) (< (r 0) 0xF0) (pos? (r 1))) [[0 (r 1) 0 0]] []))
                       :else
                       (let [[r' hi] (replace-rec recs i spec rng prog-min)
                             ss  (+ (* hi 256) (r' 1))
                             ;; ST が 255 を超えたら、240 にして残りを休符で足す (replace の dat[i+1])
                             grp (if (pos? hi)
                                   (into [(assoc r' 1 240)]
                                         (loop [ss (- ss 240) acc []] (if (pos? ss) (recur (- ss 240) (conj acc [0 (min 240 ss) 0 0])) acc)))
                                   [r'])]
                         (conj out (if (:add? spec) (into [(assoc r 1 0)] grp) grp))))))
                 [] (range (count recs)))
            new-to (+ from (count (apply concat (subvec out from (min to (count out))))))]
        [(vec (apply concat out)) new-to (count hits)]))))

(defn apply-specs
  "指定全体 (| 区切り) を [from, to) に順に当てる。=> {:recs 新しい recs :hits 当たった数} か {:error}
   opts: {:spc-code f :prog-min 1 :rng java.util.Random}"
  [recs from to text mode opts]
  (let [specs (parse-specs text opts)]
    (if (:error specs)
      specs
      (loop [specs specs recs (vec recs) to to hits 0]
        (if (empty? specs)
          {:recs recs :hits hits}
          (let [[recs to n] (run-one recs from to (first specs) mode (or (:rng opts) (java.util.Random.)) (or (:prog-min opts) 1))]
            (recur (rest specs) recs to (+ hits n))))))))

(defn find-match
  "検索 (F5 / Shift+F5): pos の次 (forward? false なら前) から、指定に合う最初の行。無ければ nil"
  [recs pos text forward? opts]
  (let [specs (parse-specs text opts)]
    (if (:error specs)
      specs
      (let [spec (first specs)
            idxs (if forward? (range (inc pos) (count recs)) (range (dec (min pos (count recs))) -1 -1))]
        (some #(when (matches? recs % spec) %) idxs)))))

;; ------------------------------------------------------------
;; 簡易チェック (マニュアルの指定例。期待値は手で求めた)
;; ------------------------------------------------------------
(defn run-checks []
  (let [spc (fn [sym] (case sym "V" {:code 0xEB :ctl 7} "E" {:code 0xEB :ctl 11} "G" {:code 0xEC :ctl -1}
                        "P" {:code 0xEE :ctl -1} "7" {:code 0xEB :ctl 7} "11" {:code 0xEB :ctl 11} nil))
        opts {:spc-code spc :prog-min 1 :rng (java.util.Random. 1)}
        R (fn [recs text] (:recs (apply-specs recs 0 (count recs) text :replace opts)))
        D (fn [recs text] (:recs (apply-specs recs 0 (count recs) text :delete opts)))
        n3 [[60 48 46 64] [62 48 46 10] [48 24 8 100]]
        checks
        [["C4:D#4" (R n3 "C4:D#4") [[63 48 46 64] [62 48 46 10] [48 24 8 100]]]
         [":+12 (全ノートを1オクターブ上)" (mapv first (R n3 ":+12")) [72 74 60]]
         [",,,:+10 (ベロシティ +10)" (mapv #(% 3) (R n3 ",,,:+10")) [74 20 110]]
         [",,,10:20 (ベロシティ 10 だけ 20 に)" (mapv #(% 3) (R n3 ",,,10:20")) [64 20 100]]
         [",,:/2 (GT を半分)" (mapv #(% 2) (R n3 ",,:/2")) [23 23 4]]
         [",,,:-64*1.2+64 (64 を中心に 1.2 倍)" (mapv #(% 3) (R n3 ",,,:-64*1.2+64")) [64 0 107]]
         ["64>,,,:+20 (キー 64 未満)" (mapv #(% 3) (R n3 "64>,,,:+20")) [84 30 120]]
         [",,,100<:+5 (100 より大きい) は無し" (mapv #(% 3) (R n3 ",,,100<:+5")) [64 10 100]]
         ["C*:+1 (すべての C を半音上)" (mapv first (R n3 "C*:+1")) [61 62 49]]
         ["48_59,,,:100" (mapv #(% 3) (R n3 "48_59,,,:100")) [64 10 100]]
         [",,8_16:12" (mapv #(% 2) (R n3 ",,8_16:12")) [46 46 12]]
         ["_47:0,,,:0 は無し (48 以上だけ)" (R n3 "_47:0,,,:0") n3]
         [",,!46:8 (GT 46 以外を 8)" (mapv #(% 2) (R n3 ",,!46:8")) [46 46 8]]
         [",:*2,:*2 (ST と GT を 2 倍、255 を超えたら休符で足す)"
          (R [[60 200 190 64]] ",:*2,:*2") [[60 240 255 64] [0 160 0 0]]]
         ["%60_71:+12 (元を残して1オクターブ上を足す)"
          (R [[60 48 46 64] [48 48 46 64]] "%60_71:+12") [[60 0 46 64] [72 48 46 64] [48 48 46 64]]]
         ["/G,,10:16 (音色 10 を 16 に、1 始まり)" (R [[0xEC 0 9 0] [0xEC 0 3 0]] "/G,,10:16") [[0xEC 0 15 0] [0xEC 0 3 0]]]
         ["/P,,:*2 (ベンドを 2 倍)" (R [[0xEE 0 0 72]] "/P,,:*2") [[0xEE 0 0 80]]]
         ["/V:/E (ボリュームをエクスプレッションに)" (R [[0xEB 0 7 100] [0xEB 0 10 64]] "/V:/E") [[0xEB 0 11 100] [0xEB 0 10 64]]]
         ["/7:/11 も同じ" (R [[0xEB 0 7 100]] "/7:/11") [[0xEB 0 11 100]]]
         ["/E,,,80_100:120" (R [[0xEB 0 11 90] [0xEB 0 11 50]] "/E,,,80_100:120") [[0xEB 0 11 120] [0xEB 0 11 50]]]
         ["| で2つを順に" (mapv first (R n3 "C4:D4|D4:E4")) [64 64 48]]
         ["削除 @!36: (C2 以外を同じ ST の休符に)"
          (D [[36 48 46 64] [60 24 22 64] [0xEB 0 7 100]] "@!36:") [[36 48 46 64] [0 24 0 0] [0xEB 0 7 100]]]
         ["削除 /S (全コントローラ)" (D [[60 48 46 64] [0xEB 0 7 100] [0xEE 0 0 64]] "/S") [[60 48 46 64]]]
         ["&: タイの音だけ VEL を 1 に" (mapv #(% 3) (R [[60 48 60 64] [60 48 46 64] [62 48 46 64]] ",,&,:1")) [1 64 64]]
         ["和音の途中の音は後ろの ST で照合し、ST は変えない"
          (R [[60 0 46 64] [64 48 46 64]] ",48:24") [[60 0 46 64] [64 24 46 64]]]
         ["検索: 次・前" [(find-match n3 0 "48" true opts) (find-match n3 2 "C4" false opts) (find-match n3 0 "99" true opts)] [2 0 nil]]
         ["未対応・不正はエラー" [(boolean (:error (parse-specs "$C 4" opts))) (boolean (:error (parse-specs "/X" opts)))] [true true]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted-replace: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(run-checks)
