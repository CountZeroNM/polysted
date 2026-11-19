;; src/polysted/notename.clj — 音名入力 "C4 E G" → レコード [key ST GT vel]
;; 仕様: STED2 KEY_SUB.C の ctc(md 4.5)。純粋関数のみ。
;; 使い方:   (polysted.notename/parse-notes "C4 E G" {})
;;           => {:records [[60 48 46 64] [64 48 46 64] [67 48 46 64]]
;;               :last-key 67 :errors []}
(ns polysted.notename
  (:require [clojure.string :as str]))

(def base-semitone {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11})
(def sharp-order [\F \C \G \D \A \E \B])
(def flat-order  [\B \E \A \D \G \C \F])

(def ^:private digit-set #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def ^:private mark-set  #{\# \+ \- \N \n \= \. \< \>})

(defn keysig-offset
  "調号バイト ks での音名 letter の半音補正(+1 / 0 / -1)。
   bit0-2=♯♭の数、bit3=♭(md 4.4)。bit4(短調)は無視。"
  [ks letter]
  (let [n (bit-and ks 7)]
    (cond
      (zero? n)       0
      (bit-test ks 3) (if (some #{letter} (take n flat-order)) -1 0)
      :else           (if (some #{letter} (take n sharp-order)) 1 0))))

(defn- hex-token? [tok]
  (or (str/starts-with? tok "$")
      (boolean (re-find #"[hH]$" tok))))

(defn- parse-hex [tok]
  (let [t (if (str/starts-with? tok "$")
            (subs tok 1)
            (subs tok 0 (dec (count tok))))]
    (when (re-matches #"[0-9A-Fa-f]{1,4}" t)
      (Long/parseLong t 16))))

(defn parse-token
  "1トークン → キー番号(0〜127)。無効なら nil。
   ctx: {:last-key 60 :key-sig 0 :ignore-keysig? false}"
  [tok {:keys [last-key key-sig ignore-keysig?]
        :or   {last-key 60 key-sig 0}}]
  (cond
    (str/blank? tok) nil

    ;; キー番号の直接入力: 10進 (音名の文字が無ければ数値。ctc → str_val の atoi)
    (re-matches #"\d{1,3}" tok)
    (let [k (Long/parseLong tok)]
      (when (<= 0 k 127) k))

    ;; キー番号の直接入力: 末尾 H か先頭 $ の16進
    (hex-token? tok)
    (let [k (parse-hex tok)]
      (when (and k (<= 0 k 127)) k))

    :else
    (let [letter (first (str/upper-case (subs tok 0 1)))
          body   (seq (subs tok 1))]
      (when (contains? base-semitone letter)
        ;; 音名の直後の B は半音下げ
        (let [flat-b (if (and body (#{\B \b} (first body))) 1 0)
              body   (if (pos? flat-b) (rest body) body)]
          (when (every? #(or (digit-set %) (mark-set %)) body)
            (let [up      (count (filter #{\# \+} body))
                  down    (+ flat-b (count (filter #{\-} body)))
                  cancel? (boolean (some #{\N \n \=} body))
                  digits  (filter digit-set body)
                  base-oct (cond
                             (seq digits)      (- (int (last digits)) (int \0))
                             (some #{\.} body) -1
                             :else             (dec (quot last-key 12)))
                  oct     (+ base-oct
                             (count (filter #{\<} body))
                             (- (count (filter #{\>} body))))
                  ks-off  (if ignore-keysig? 0 (keysig-offset key-sig letter))
                  off     (if cancel? 0 (+ ks-off up (- down)))
                  k       (+ (* 12 (inc oct)) (base-semitone letter) off)]
              (when (<= 0 k 127) k))))))))

(defn parse-notes
  "空白かカンマ区切りの音名列 → {:records [...] :last-key n :errors [無効トークン]}
   ctx: {:last-key 60 :st 48 :gt 46 :vel 64 :key-sig 0 :ignore-keysig? false}
   無効なトークンは捨てて :errors に集める(原実装も範囲外は黙って捨てる)。"
  [text ctx]
  (let [{:keys [st gt vel last-key] :or {st 48 gt 46 vel 64 last-key 60}} ctx
        toks (remove str/blank? (str/split (or text "") #"[\s,]+"))]
    (reduce
     (fn [acc tok]
       (if-let [k (parse-token tok (assoc ctx :last-key (:last-key acc)))]
         (-> acc
             (update :records conj [k st gt vel])
             (assoc :last-key k))
         (update acc :errors conj tok)))
     {:records [] :last-key last-key :errors []}
     toks)))

;; ---- 簡易チェック(期待値は手で追って求めた) ----
(defn run-checks []
  (let [pn   (fn [s c] (parse-notes s c))
        keys* (fn [s c] (mapv first (:records (pn s c))))
        checks
        [["C4 E G"            (keys* "C4 E G" {})       [60 64 67]]
         ["レコード形"        (:records (pn "C4 E G" {}))
                              [[60 48 46 64] [64 48 46 64] [67 48 46 64]]]
         ["# b -"             (keys* "C#4 Db4 D-4" {})  [61 61 61]]
         ["B と B#"           (keys* "B4 B#4" {})       [71 72]]
         ["省略=直前キー"     (keys* "C5 E" {})         [72 76]]
         ["初期 last-key 72"  (keys* "C" {:last-key 72}) [72]]
         ["< は上"            (keys* "C<" {})           [72]]
         ["> は下"            (keys* "C>" {})           [48]]
         ["ドット=-1"         (keys* "C." {})           [0]]
         ["最後の数字が有効"  (keys* "C34" {})          [60]]
         ["16進 H と $"       (keys* "3CH $40" {})      [60 64]]
         ["範囲外16進"        (:errors (pn "80H" {}))   ["80H"]]
         ["10進のキー番号"    (keys* "60 0 127" {})     [60 0 127]]
         ["範囲外10進"        (:errors (pn "128" {}))   ["128"]]
         ["無効トークン"      (keys* "C4 X E" {})       [60 64]]
         ["無効の収集"        (:errors (pn "C4 X E" {})) ["X"]]
         ["調号 F#"           (keys* "F4 FN4 C4" {:key-sig 1}) [66 65 60]]
         ["調号 Bb"           (keys* "B4 E4" {:key-sig 9})     [70 64]]
         ["調号を無視"        (keys* "F4" {:key-sig 1 :ignore-keysig? true}) [65]]
         ["ST GT vel 指定"    (:records (pn "C4" {:st 24 :gt 22 :vel 100}))
                              [[60 24 22 100]]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted.notename: OK " (count checks) "件"))
      (doseq [[l a e] bad]
        (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

