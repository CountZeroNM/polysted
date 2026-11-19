;; gui.clj — GUI (Swing) を単体で起動するスクリプト
;;   clojure -M gui.clj               空の曲で起動
;;   clojure -M gui.clj song.edn      EDN を開いて起動 (Ctrl-S / w で同じファイルに保存)
;;   clojure -M gui.clj x.rcp         RCP/R36 を開いて起動
;;   clojure -M gui.clj x.mid         SMF を開いて起動
(load-file "polysted.clj") (load-file "polysted_rcp.clj") (load-file "polysted_edit.clj")
(load-file "polysted_notename.clj") (load-file "polysted_edn.clj") (load-file "polysted_smf.clj")
(load-file "polysted_replace.clj") (load-file "polysted_rhythm.clj") (load-file "polysted_ctrl.clj") (load-file "polysted_roll.clj") (load-file "polysted_visual.clj") (load-file "polysted_score.clj") (load-file "polysted_record.clj")
(load-file "polysted_session.clj")
(load-file "polysted_tui_base.clj") (load-file "polysted_tui_screens.clj") (load-file "polysted_tui.clj")
(load-file "polysted_gui.clj")

(let [path (first *command-line-args*)
      lc   (some-> path clojure.string/lower-case)]
  (cond
    (nil? path) nil
    (clojure.string/ends-with? lc ".edn")
    (if (.exists (java.io.File. ^String path))
      (reset! polysted-session/S (assoc (polysted-session/new-session (polysted-edn/load-song path)) :file path))
      (swap! polysted-session/S assoc :file path))
    :else
    (let [{:keys [song warnings]} (polysted-session/song-from-file path)]
      (reset! polysted-session/S (cond-> (polysted-session/new-session song)
                               (seq warnings) (assoc :msg (clojure.string/join " / " warnings))))))
  (polysted-gui/run! {:exit-on-close? true}))
