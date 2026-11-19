;; summarize.clj -- サンプルRCPの中身を要約する診断スクリプト
;; プロジェクトのルート (deps.edn がある場所) から実行。RCP は samples/ に置く (同梱していない):
;;   clojure -M samples/summarize.clj
(require 'polysted.rcp)

(defn summarize [path]
  (let [s   (polysted.rcp/read-rcp path)
        evs (mapcat :events (:tracks s))]
    {:title (:title s)
     :timebase (:timebase s)
     :tempo (:tempo s)
     :tracks (count (:tracks s))
     :types (frequencies (map :type evs))
     :tempo-raw (->> evs (filter #(= :tempo (:type %))) (map :raw) distinct sort)
     :loop-counts (->> evs (filter #(= :loop-end (:type %))) (map :count) distinct sort)
     :raw-codes (->> evs (filter #(= :raw (:type %))) (map :code) distinct sort
                     (map #(format "0x%02X" %)))}))

(doseq [f ["sample_scale" "sample_loop" "sample_stress" "sample_mt32"]]
  (println "==" f)
  (try
    (prn (summarize (str "samples/" f ".rcp")))
    (catch Exception e
      (println "  error:" (.getMessage e)))))
