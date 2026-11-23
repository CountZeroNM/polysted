;; build.clj -- 配布用の jar を作る (tools.build)
;;   clojure -T:build uber     target/polysted-<版>.jar (java -jar で GUI が開く)
;;   clojure -T:build clean
;; 前提: 仮想 MIDI キーボード VMidiKey の機器の jar (../vmidikey/target/vmidikey-device.jar) があること
;;      (vmidikey で bb device)。deps.edn の :vmidikey で取り込み、jar の中に同梱する。
(ns build
  (:require [clojure.tools.build.api :as b]))

(def version "0.1.2")
(def class-dir "target/classes")
(def uber-file (format "target/polysted-%s.jar" version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (let [basis (b/create-basis {:project "deps.edn" :aliases [:vmidikey]})]
    (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
    (b/copy-dir {:src-dirs ["examples"] :target-dir (str class-dir "/polysted/examples") :include "*.edn"})
    (b/copy-dir {:src-dirs ["doc/original"] :target-dir (str class-dir "/polysted/doc/original")})
    (b/copy-file {:src "LICENSE" :target (str class-dir "/LICENSE")})
    (b/write-file {:path (str class-dir "/polysted/version.txt") :string version})
    ;; 事前にコンパイルして起動を速くする (ウィンドウは作らないので headless で)
    (b/compile-clj {:basis basis :class-dir class-dir
                    :ns-compile '[polysted.main polysted.gui polysted.tui polysted.check]
                    :java-opts ["-Djava.awt.headless=true"]})
    ;; 内蔵音源の遅れを指定するため、Java の内部の API を使えるようにする (java -jar のとき manifest で効く)
    (b/uber {:class-dir class-dir :uber-file uber-file :basis basis :main 'polysted.main
             :manifest {"Add-Exports" "java.desktop/com.sun.media.sound"}})
    (println "作成:" uber-file)))
