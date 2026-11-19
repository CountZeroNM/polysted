;; polysted_edn.clj — 曲データ(レコード形式)の EDN 保存・読み込み
;; 方針: EDN テキストを保存形式の正とする(handoff の decisions「保存形式の正」)。
;; 形式: 曲の map そのもの + :format :polysted-song / :version 1。
;; 読み込みでは、名前を変える前の開発版の印 :sted-song も受け付ける (同じ形式)。
;;       各トラックの :recs は1行1レコード [b0 b1 b2 b3] で書く(git の差分で読めるように)。
;; 読み込み: (load-file "polysted_edn.clj")  ※ロード時に run-checks が走る
;; 使い方:   (polysted-edn/save-song! song "x.edn")  (polysted-edn/load-song "x.edn")
(ns polysted-edn
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def format-tag :polysted-song)
(def ^:private legacy-tags "以前の名前の印 (読み込みだけ受け付ける)" #{:sted-song})
(def format-version 1)

;; ---- 書き出し ----
(defn- kv-str
  "map を {:a 1 :b 2} の中身部分 \":a 1 :b 2\" にする (キー順は ks の順、残りは後ろ)"
  [m ks]
  (let [ks (concat (filter #(contains? m %) ks)
                   (sort-by str (remove (set ks) (keys m))))]
    (str/join " " (map #(str (pr-str %) " " (pr-str (m %))) ks))))

(defn- track-str [t]
  (let [head (kv-str (dissoc t :recs) [:no :ch :mute? :rhythm? :comment])
        recs (:recs t)]
    (str "{" head "\n   :recs\n   ["
         (str/join "\n    " (map pr-str recs))
         "]}")))

(defn song->edn-str
  "曲(:tracks の各要素が :recs を持つ形)→ EDN 文字列"
  [song]
  (let [head (kv-str (-> song (dissoc :tracks :format :version))
                     [:title :timebase :tempo :key :time-sig])]
    (str ";; polysted song (EDN)\n"
         "{:format " (pr-str format-tag) " :version " format-version "\n"
         " " head "\n"
         " :tracks\n"
         " [" (str/join "\n  " (map track-str (:tracks song))) "]}\n")))

;; ---- 読み込み ----
(defn- valid-rec? [r]
  (and (vector? r) (= 4 (count r))
       (every? #(and (integer? %) (<= 0 % 255)) r)))

(defn- fail [msg data] (throw (ex-info (str "EDN 曲データ: " msg) data)))

(defn edn-str->song
  "EDN 文字列 → 曲。形式・レコードを検査し、不正なら ex-info を投げる。"
  [text]
  (let [m (try (edn/read-string text)
               (catch Exception e (fail "EDN として読めない" {:cause (.getMessage e)})))]
    (when-not (map? m) (fail "先頭が map でない" {}))
    (when-not (or (= format-tag (:format m)) (legacy-tags (:format m))) (fail ":format が :polysted-song でない" {:format (:format m)}))
    (when-not (and (integer? (:version m)) (<= 1 (:version m) format-version))
      (fail "対応していない :version" {:version (:version m)}))
    (when-not (and (vector? (:tracks m)) (seq (:tracks m)))
      (fail ":tracks が空か、ベクタでない" {}))
    (doseq [[ti t] (map-indexed vector (:tracks m))]
      (when-not (vector? (:recs t)) (fail ":recs がベクタでない" {:track ti}))
      (doseq [[ri r] (map-indexed vector (:recs t))]
        (when-not (valid-rec? r)
          (fail "不正なレコード (4つの 0〜255 の整数が必要)" {:track ti :index ri :rec r}))))
    (dissoc m :format :version)))

(defn save-song! [song path]
  (spit path (song->edn-str song) :encoding "UTF-8")
  path)

(defn load-song [path]
  (edn-str->song (slurp path :encoding "UTF-8")))

;; ---- 簡易チェック ----
(defn run-checks []
  (let [song {:title "テスト曲" :timebase 48 :tempo 120 :key 9 :time-sig [3 4]
              :tracks [{:no 0 :ch 1 :recs [[60 48 46 64] [0 48 0 0] [0xFE 0 0 0]]
                        :mute? false :rhythm? false :comment "メロ"}
                       {:no 1 :ch nil :recs [] :mute? true :rhythm? true :comment ""
                        :key-shift 0 :st-shift 0}]}
        thrown (fn [text] (try (edn-str->song text) nil
                               (catch clojure.lang.ExceptionInfo e (ex-data e))))
        checks
        [["往復で同じ曲に戻る"        (edn-str->song (song->edn-str song)) song]
         ["1行1レコードで書く"        (boolean (re-find #"\n    \[0 48 0 0\]\n" (song->edn-str song))) true]
         ["空トラックも往復できる"    (-> song song->edn-str edn-str->song :tracks second :recs) []]
         ["format 違いは拒否"         (some? (thrown "{:format :other :version 1 :tracks [{:recs []}]}")) true]
         ["新しい version は拒否"     (:version (thrown "{:format :polysted-song :version 2 :tracks [{:recs []}]}")) 2]
         ["不正なレコードの位置を返す" (thrown "{:format :polysted-song :version 1 :tracks [{:recs [[60 48 46 64] [256 0 0 0]]}]}")
                                      {:track 0 :index 1 :rec [256 0 0 0]}]
         ["3要素のレコードは拒否"     (:index (thrown "{:format :polysted-song :version 1 :tracks [{:recs [[60 48 46]]}]}")) 0]
         ["以前の印 :sted-song も読める" (count (:tracks (edn-str->song "{:format :sted-song :version 1 :tracks [{:recs [[60 48 46 64]]}]}"))) 1]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted-edn: OK " (count checks) "件"))
      (doseq [[l a e] bad]
        (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(run-checks)
