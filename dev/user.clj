;; dev/user.clj -- REPL の user 名前空間 (clojure -M:repl / clojure -M:dev / bb repl.bb で自動で読まれる)
;;
;; 読み込むと、s が polysted.session の別名になる。チェックは (polysted.check/run-all)。
;; GUI (polysted.gui) は画面が使えない環境では読み込めないので、その場合は注意だけ出して続ける。
(ns ^{:clj-kondo/ignore [:unused-namespace]} user
  (:require [polysted.session :as s]
            [polysted.tui]))

(def gui-loaded?
  (try (require 'polysted.gui) true
       (catch Throwable e
         (println "\n[注意] GUI (polysted.gui) は読み込みませんでした:"
                  (.getMessage ^Throwable (loop [c e] (if-let [n (.getCause ^Throwable c)] (recur n) c))))
         (println "       画面のある端末で起動すると (s/gui) が使えます (Claude Code の環境では DISPLAY=:0)")
         false)))

(println "
==== polysted の REPL ====
  (s/show)                 カーソルのまわりを表示       (s/n \"C4 E G\")  音名で入力
  (s/mml \"l8 cdefg\")       MML で入力                   (s/play!) / (s/stop!)  再生 / 停止
  (s/open! \"x.rcp\")        RCP / R36 / G36 / .mid を開く (s/save! \"x.edn\")  EDN で保存
  (s/tui)                  全画面の TUI (q で REPL に戻る)" (if gui-loaded? "\n  (s/gui)                  GUI のウィンドウ" "") "
  コマンドの一覧は README.md の「REPL コマンドリファレンス」
")
