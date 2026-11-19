#!/usr/bin/env bb
;; repl.bb — polysted の REPL を起動する babashka スクリプト
;;   bb repl.bb            dev/user.clj を読み込んで rebel-readline の REPL (clojure -M:repl)
;;   bb repl.bb --plain    rebel を使わない普通の REPL (clojure -M:dev)
;; どのディレクトリから起動しても、このスクリプトのあるディレクトリで実行する。終了コードは clojure のもの。
(require '[babashka.process :as p]
         '[babashka.fs :as fs])

(let [dir (str (fs/parent (fs/absolutize *file*)))
      plain? (some #{"--plain"} *command-line-args*)
      cmd (if plain?
            ["clojure" "-M:dev"]
            ["clojure" "-M:repl"])]
  (println "起動:" (clojure.string/join " " cmd) " (ディレクトリ" dir ")")
  ;; 端末の入出力はそのまま REPL へつなぐ (shell は入出力を引き継ぐ)
  ;; ChromeOS の Linux では DISPLAY が wayland-0 になっていて、GUI (Swing) が X11 につなげない。
  ;; DISPLAY が X11 の形 (:0 など) でなく、X11 のソケットがあるときは :0 を使う ((s/gui) が使えるように)
  (let [d (System/getenv "DISPLAY")
        env (if (and (not (and d (clojure.string/includes? d ":"))) (fs/exists? "/tmp/.X11-unix/X0")) {"DISPLAY" ":0"} {})]
    (System/exit (:exit (apply p/shell {:dir dir :continue true :extra-env env} cmd))))))
