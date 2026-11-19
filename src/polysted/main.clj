;; src/polysted/main.clj -- 配布用の jar の起動の入口 (java -jar polysted.jar)
;;   java -jar polysted.jar [file]          GUI (ウィンドウ)。ダブルクリックでも同じ
;;   java -jar polysted.jar --tui [file]    TUI (端末)
;;   java -jar polysted.jar --repl          REPL (s が polysted.session の別名)
;;   java -jar polysted.jar --check         全部の簡易チェック
;;   java -jar polysted.jar --version / --help
;; 各 UI の名前空間は requiring-resolve で、使うものだけを読み込む。
(ns polysted.main
  (:require [clojure.java.io :as io]
            [clojure.main]
            [clojure.string :as str])
  (:gen-class))

(defn version
  "版の番号 (jar を作るときに polysted/version.txt に書く。開発中は dev)"
  []
  (or (some-> (io/resource "polysted/version.txt") slurp str/trim) "dev"))

(def usage
  (str "polysted " "— STED2 に強い影響を受けた MIDI ステップエディタ／シーケンサ\n"
       "  java -jar polysted.jar [file]          GUI (ウィンドウ)。file は .edn / .rcp / .r36 / .g36 / .mid\n"
       "  java -jar polysted.jar --tui [file]    TUI (端末)\n"
       "  java -jar polysted.jar --repl          REPL (s/ で操作。(s/sample! 1) でサンプル曲)\n"
       "  java -jar polysted.jar --check         簡易チェック\n"
       "  java -jar polysted.jar --version\n"
       "GUI のメニュー「ファイル → サンプル曲を開く」で、サンプル曲を開けます。\n"
       "https://github.com/CountZeroNM/polysted"))

(defn parse-args
  "コマンドラインの引数 → {:mode :gui|:tui|:repl|:check|:version|:help :file ファイル}"
  [args]
  (let [[a b] args]
    (case a
      ("--tui" "-t")     {:mode :tui :file b}
      ("--repl" "-r")    {:mode :repl}
      "--check"          {:mode :check}
      ("--version" "-v") {:mode :version}
      ("--help" "-h")    {:mode :help}
      (if (and a (str/starts-with? a "-"))
        {:mode :help :error (str "知らないオプションです: " a)}
        {:mode :gui :file a}))))

(defn- repl! []
  (clojure.main/repl
   :init (fn []
           (in-ns 'user)
           (require '[polysted.session :as s])
           (println (str "==== polysted " (version) " の REPL ====\n"
                         "  (s/sample! 1)  サンプル曲を開く   (s/show)  表示   (s/play!) / (s/stop!)  再生 / 停止\n"
                         "  (s/n \"C4 E G\")  音名で入力         (s/mml \"l8 cdefg\")  MML で入力   (s/gui) ウィンドウ  (s/tui) 端末\n"
                         "  終わるときは Ctrl-D")))))

(defn -main [& args]
  (let [{:keys [mode file error]} (parse-args args)]
    (case mode
      :version (println "polysted" (version))
      :help    (do (when error (println error)) (println usage))
      :tui     ((requiring-resolve 'polysted.tui/-main) file)
      :repl    (do (repl!) (shutdown-agents) (System/exit 0))
      :check   ((requiring-resolve 'polysted.check/-main))
      :gui     (if (java.awt.GraphicsEnvironment/isHeadless)
                 (do (println "画面 (ウィンドウ) が使えない環境です。端末で使うときは --tui を付けてください。")
                     (println usage)
                     (System/exit 1))
                 ((requiring-resolve 'polysted.gui/-main) file)))))

(defn run-checks []
  (let [cases [["引数なしは GUI"             (parse-args [])                  {:mode :gui :file nil}]
               ["ファイルだけなら GUI で開く" (parse-args ["a.edn"])           {:mode :gui :file "a.edn"}]
               ["--tui とファイル"            (parse-args ["--tui" "a.rcp"])   {:mode :tui :file "a.rcp"}]
               ["--repl / --check"            (mapv :mode [(parse-args ["-r"]) (parse-args ["--check"])]) [:repl :check]]
               ["知らないオプションは使い方"  (parse-args ["--foo"])           {:mode :help :error "知らないオプションです: --foo"}]]
        bad (remove (fn [[_ a e]] (= a e)) cases)]
    (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a))
    (when (empty? bad) (println (str "polysted.main: OK " (count cases) "件")))))
