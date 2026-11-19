;; src/polysted/tui_base.clj — TUI の土台 (純粋): 端末の入力バイトの解読、UI の状態、画面で共通の小さな関数
;;
;; src/polysted/tui.clj から分けたもの (2026-11-19)。src/polysted/tui_screens.clj と src/polysted/tui.clj が :refer で使う。
(ns polysted.tui-base
  (:require [clojure.string :as str]
            [polysted.session :as ss]))

;; ---- 入力バイト列 → キー ----
(def csi-final
  {65 :up 66 :down 67 :right 68 :left 72 :home 70 :end
   80 :f1 81 :f2 82 :f3 83 :f4})                         ; ESC O P〜S / ESC [1;2P など
(def csi-tilde
  {"1" :home "7" :home "4" :end "8" :end "3" :delete "5" :pgup "6" :pgdn "2" :insert
   "11" :f1 "12" :f2 "13" :f3 "14" :f4 "15" :f5
   "17" :f6 "18" :f7 "19" :f8 "20" :f9 "21" :f10})
(def modifiers {"2" "shift-" "5" "ctrl-"})

(defn decode-keys
  "端末から読んだ文字コード列 → キーのベクタ。
   キーは :up :down :left :right :home :end :pgup :pgdn :delete :insert
   :ctrl-up :ctrl-down :ctrl-left :ctrl-right (Ctrl+矢印)
   :f1〜:f10 と :shift-f1〜:shift-f10 (Alt+1〜Alt+0 も :f1〜:f10 として扱う)
   :enter :backspace :tab :esc :ctrl-a〜:ctrl-z、または文字 (char)。"
  [codes]
  (let [v (vec codes) n (count v)]
    (loop [i 0 out []]
      (if (>= i n)
        out
        (let [c (v i)]
          (cond
            (and (= c 27) (< (inc i) n) (#{91 79} (v (inc i))))   ; ESC [ / ESC O
            (let [j (+ i 2)
                  ;; 引数部 (数字と ;) を読み、終端文字で決める
                  k (loop [k j] (if (and (< k n) (<= 48 (v k) 63)) (recur (inc k)) k))]
              (cond
                (>= k n) (recur n out)                            ; 途中で切れた → 捨てる

                (and (= (v k) 91) (< (inc k) n))                  ; Linux コンソールの F1〜F5: ESC [ [ A〜E
                (let [f ({65 :f1 66 :f2 67 :f3 68 :f4 69 :f5} (v (inc k)))]
                  (recur (+ k 2) (if f (conj out f) out)))

                :else
                (let [fin   (v k)
                      parts (str/split (apply str (map char (subvec v j k))) #";")
                      base  (if (= fin 126) (csi-tilde (first parts)) (csi-final fin))
                      mod   (modifiers (second parts))
                      key   (if (and base mod) (keyword (str mod (name base))) base)]
                  (recur (inc k) (if key (conj out key) out)))))

            (and (= c 27) (< (inc i) n) (<= 48 (v (inc i)) 57))  ; Alt+数字 → F キーの代わり
            (let [d (- (v (inc i)) 48)]
              (recur (+ i 2) (conj out (keyword (str "f" (if (zero? d) 10 d))))))

            (= c 27)              (recur (inc i) (conj out :esc))
            (or (= c 13) (= c 10)) (recur (inc i) (conj out :enter))
            (or (= c 127) (= c 8)) (recur (inc i) (conj out :backspace))
            (= c 9)               (recur (inc i) (conj out :tab))
            (<= 1 c 26)           (recur (inc i) (conj out (keyword (str "ctrl-" (char (+ c 96))))))
            (= c 30)              (recur (inc i) (conj out :ctrl-caret))    ; Ctrl+^ (Ctrl+6)
            (< c 32)              (recur (inc i) out)
            :else                 (recur (inc i) (conj out (char c)))))))))

;; ---- UI 状態 ----
(defn new-ui
  "s: セッション。saved: 保存済みとみなす曲 (未保存の変更の判定に使う)
   :direct? 直接入力モード  :cx 欄カーソル (0=K# 1=ST 2=GT 3=VEL)
   :edit    欄の入力中の状態 (nil なら入力中でない)
   :mark    範囲の開始行 (F6 / v。nil なら範囲なし)"
  ([s] (new-ui s (:song s)))
  ([s saved]
   {:s s :saved saved :mode :normal :kind nil :buf ""
    :direct? false :cx 0 :edit nil :mark nil
    :screen :edit :ts-row 2 :ts-col 0
    :rows 24 :cols 80 :effects [] :quit? false :confirm-quit? false :help? false}))

(defn body-height [ui] (max 1 (- (:rows ui) 4)))   ; 見出し・列見出し・状態行・ヘルプ行を除く
(defn page [ui] (max 1 (dec (body-height ui))))
(defn dirty? [ui] (not= (:song (:s ui)) (:saved ui)))
(defn set-msg [ui m] (assoc-in ui [:s :msg] m))
(defn recs [ui] (ss/cur-recs (:s ui)))
(defn cur-pos [ui] (:pos (:s ui)))

(def prompt-labels {:notes "音名" :mml "MML" :save "保存先 (.edn / .rcp / .r36 / .mid)"
                    :quantize "クオンタイズ 解像度(2-48) レート(50-100%)"
                    :note-ctrl "ノート操作 L/C/R/O/T 下限 上限 | U/D | G/S ずらすST"
                    :replace "置換 (例 C4:D#4  ,,,:+10  /V:/E)"
                    :delete-match "指定削除 (例 /S  @!36)"
                    :search "検索 (例 C4  ,,,100<  /P)"
                    :command "コマンド (same / same all / extract / repeat / repeat all / rev / mix / mplay / rec / uex n データ / excload ファイル / excsave ファイル)"
                    :rc-ch "録音チャンネル (1〜16)" :rc-meas "録音する小節 (開始 終わり)"
                    :ot-val "値 (ST・GT 0〜255、VEL 0〜127)" :ux-name "名前 (24 文字まで)" :ux-val "挿入する値 (16進 00〜7F か gt ve ch cs ss)"
                    :ux-val-over "値 (16進 00〜7F か gt ve ch cs ss)" :ux-copy "写す元の番号 (0〜7)"
                    :ux-load "読み込む .REX ファイル (8 個まとめて)" :ux-load-res "読み込む .RES ファイル (この番号へ)"
                    :ux-save "書き出す .REX ファイル (8 個まとめて)" :ux-save-res "書き出す .RES ファイル (この番号)"
                    :exclusive "トラックエクスクルーシブ (16進、gt ve ch cs ss。例 41 10 42 12 cs 40 00 7F 00 ss)"
                    :replicate "レプリケート トラック 開始小節 終了小節 回数 キーシフト"
                    :replicate-all "全トラックのレプリケート 開始小節 終了小節 回数 キーシフト"
                    :replicate-range "範囲を トラック 小節 回数 キーシフト へ"
                    :replicate-range-all "範囲を全トラックの 小節 回数 キーシフト へ"})
(defn run-cmd [ui cmd args]
  (try
    (update ui :s #(apply ss/dispatch % cmd args))
    (catch Exception e
      (set-msg ui (str "エラー: " (or (.getMessage e) (class e)))))))

(defn char-width
  "端末での表示幅。全角 (CJK・全角記号・ハングル) は 2、それ以外は 1。
   ↑ ♪ などの「幅が曖昧な文字」は 1 とみなす (多くの端末の既定)。"
  [c]
  (let [c (int c)]
    (if (or (<= 0x1100 c 0x115F) (<= 0x2E80 c 0x303E) (<= 0x3041 c 0x33FF)
            (<= 0x3400 c 0x4DBF) (<= 0x4E00 c 0x9FFF) (<= 0xA000 c 0xA4CF)
            (<= 0xAC00 c 0xD7A3) (<= 0xF900 c 0xFAFF) (<= 0xFE30 c 0xFE4F)
            (<= 0xFF00 c 0xFF60) (<= 0xFFE0 c 0xFFE6))
      2 1)))

(defn cut-width
  "表示幅が cols を超えないように末尾を切る"
  [s cols]
  (loop [i 0 w 0]
    (if (>= i (count s))
      s
      (let [w' (+ w (char-width (.charAt ^String s i)))]
        (if (> w' cols) (subs s 0 i) (recur (inc i) w'))))))

(defn pad-l [s w] (let [s (str s)] (str (str/join (repeat (- w (count s)) \space)) s)))
(defn pad-r [s w] (let [s (str s)] (str s (str/join (repeat (- w (count s)) \space)))))
