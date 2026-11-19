;; src/polysted/check.clj -- 全部の名前空間の簡易チェック (run-checks) を順に実行する
;;   clojure -M:check          NG が 1 つでもあれば終了コード 1
;; 各名前空間の run-checks は、期待値を手で追って求めた (label 期待値 実際) の照合を OK / NG で表示する。
;; GUI (polysted.gui) は画面が使えない環境では読み込めないので、その場合は注意を出して飛ばす。
(ns polysted.check
  (:require [clojure.string :as str]))

(def check-nss
  "チェックする名前空間 (依存の少ない順)"
  '[polysted.notename polysted.edn polysted.smf polysted.replace polysted.rhythm polysted.ctrl
    polysted.roll polysted.visual polysted.score polysted.record polysted.edit polysted.session
    polysted.tui polysted.gui])

(defn- root-message [^Throwable e]
  (.getMessage ^Throwable (loop [c e] (if-let [n (.getCause ^Throwable c)] (recur n) c))))

(defn run-ns
  "1 つの名前空間のチェックを実行して {:ns :ng :skipped?} を返す (出力はそのまま表示する)"
  [ns-sym]
  (let [loaded (try (require ns-sym) nil (catch Throwable e e))]
    (if (and loaded (= ns-sym 'polysted.gui))
      (do (println "[注意] polysted.gui を読み込めないので飛ばす:" (root-message loaded))
          (println "       画面のある環境で実行する (Claude Code の環境では DISPLAY=:0)")
          {:ns ns-sym :ng 0 :skipped? true})
      (let [out (with-out-str
                  (try (when loaded (throw loaded))
                       ((requiring-resolve (symbol (str ns-sym) "run-checks")))
                       (catch Throwable e (println "NG " ns-sym "の読み込みかチェックで例外:" (root-message e)))))]
        (print out) (flush)
        {:ns ns-sym :ng (count (filter #(str/starts-with? % "NG") (str/split-lines out)))}))))

(defn run-all [] (mapv run-ns check-nss))

(defn -main [& _]
  (let [rs (run-all)
        ng (reduce + (map :ng rs))]
    (println (str "\n==== チェック: NG " ng " 件"
                  (when-let [sk (seq (filter :skipped? rs))] (str " (飛ばした: " (str/join " " (map :ns sk)) ")"))
                  " ===="))
    (shutdown-agents)
    (System/exit (if (zero? ng) 0 1))))
