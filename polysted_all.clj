;; polysted_all.clj — 全部のファイルを正しい順に読み込む (REPL 用)
;;
;; 使い方:
;;   clojure -M:rebel                 ; rebel-readline の REPL を起動してから
;;   (load-file "polysted_all.clj")       ; これだけで全部読み込まれる
;;
;;   clojure -M:polysted                  ; 読み込んでから rebel-readline を起動 (deps.edn のエイリアス)
;;   bb repl.bb                       ; 同じ (babashka から起動)。--plain で普通の REPL
;;   clojure -M -i polysted_all.clj -r    ; rebel を使わない普通の REPL
;;
;; 読み込むと、各ファイルの簡易チェック (OK / NG) が表示され、user 名前空間に s (polysted-session) の別名ができる。
;; GUI (polysted_gui.clj) は画面が使えない環境では読み込みに失敗するので、その場合は警告だけ出して続ける。
(def polysted-files
  "読み込み順 (後のファイルが前の名前空間を使う)"
  ["polysted.clj" "polysted_rcp.clj" "polysted_edit.clj" "polysted_notename.clj" "polysted_edn.clj" "polysted_smf.clj"
   "polysted_replace.clj" "polysted_rhythm.clj" "polysted_ctrl.clj" "polysted_roll.clj" "polysted_visual.clj" "polysted_score.clj"
   "polysted_record.clj" "polysted_session.clj" "polysted_tui_base.clj" "polysted_tui_screens.clj" "polysted_tui.clj"])

(doseq [f polysted-files] (load-file f))

(def polysted-gui-loaded?
  (try (load-file "polysted_gui.clj") true
       (catch Throwable e
         (println "\n[注意] GUI (polysted_gui.clj) は読み込みませんでした:"
                  (.getMessage ^Throwable (loop [c e] (if-let [n (.getCause ^Throwable c)] (recur n) c))))
         (println "       画面のある端末で起動すると (s/gui) が使えます (Claude Code の環境では DISPLAY=:0)")
         false)))

(in-ns 'user)
(alias 's 'polysted-session)

(println "
==== polysted の REPL ====
  (s/show)                 カーソルのまわりを表示       (s/n \"C4 E G\")  音名で入力
  (s/mml \"l8 cdefg\")       MML で入力                   (s/play!) / (s/stop!)  再生 / 停止
  (s/open! \"x.rcp\")        RCP / R36 / G36 / .mid を開く (s/save! \"x.edn\")  EDN で保存
  (s/tui)                  全画面の TUI (q で REPL に戻る)" (if polysted-gui-loaded? "\n  (s/gui)                  GUI のウィンドウ" "") "
  コマンドの一覧は README.md の「REPL コマンドリファレンス」
")
