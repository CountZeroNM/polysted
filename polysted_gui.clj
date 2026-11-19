;; polysted_gui.clj — GUI 層 (Swing。追加のライブラリなし)
;;
;; 構成 (md 3.5 の方針: UI は描画と入力の変換だけを持ち、コアを共有する):
;;   - 画面の内容は TUI と同じ polysted-tui/render-lines、キー操作は polysted-tui/handle-key をそのまま使う。
;;     編集画面・直接入力・トラック設定・リズム画面のすべての機能が GUI でも使える。
;;   - GUI 独自: メニュー (ファイル選択ダイアログ付きの開く・保存)、マウス (クリックで行を選ぶ、
;;     ホイールでスクロール)、ウィンドウの大きさに合わせた表示行数、等幅フォントでの描画。
;;   - 純粋部分: pressed->key / typed->key (Swing のキーイベント → TUI のキー)、
;;     paint-screen! (render-lines の結果を Graphics2D に描く。画像にも描けるのでテストできる)。
;;   - ピアノロール (原作には無い。GUI ならではの表示【設計判断】): 編集画面の右側に今のトラックの音を描く。
;;     計算は polysted_roll.clj (純粋)。カーソルの音を強調し、ほかのトラックの音は薄く重ねる。
;;     クリックで音の行へ移動、Ctrl+ホイールで横の拡大・縮小、F11 で表示の切り替え。
;;     編集: ドラッグで音の移動 (高さ・時刻)、右端のドラッグで長さ (GT)、空き所のダブルクリックで追加、
;;     右クリックで削除。時刻と長さはグリッドに合わせる。レコードの操作は polysted-session/roll-edit (undo 1回分)。
;; セッションは REPL・TUI と同じ polysted-session/S。
;;
;; 起動:
;;   REPL から: (load-file "polysted_tui.clj") (load-file "polysted_gui.clj") (s/gui)
;;   単体:      clojure -M gui.clj [song.edn | x.rcp | x.mid]
(ns polysted-gui
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [polysted-session :as ss]
            [polysted-tui :as tui]
            [polysted-tui-base :as tb]
            [polysted-tui-screens]
            [polysted-roll :as roll]
            [polysted-visual]
            [polysted-score :as score]
            [polysted-ctrl]
            [polysted-edn])
  (:import (javax.swing JFrame JDialog JPanel JMenuBar JMenu JMenuItem JFileChooser JOptionPane SwingUtilities
                        KeyStroke WindowConstants)
           (javax.swing.filechooser FileNameExtensionFilter)
           (java.awt Font Color Graphics2D RenderingHints Dimension Cursor)
           (java.awt.event KeyEvent KeyAdapter InputEvent ActionListener MouseAdapter MouseEvent
                           MouseWheelEvent WindowAdapter ComponentAdapter)
           (java.awt.image BufferedImage)))

;; ============================================================
;; 1. キーイベント → TUI のキー (純粋)
;; ============================================================
(def ^:private vk-keys
  {KeyEvent/VK_UP :up KeyEvent/VK_DOWN :down KeyEvent/VK_LEFT :left KeyEvent/VK_RIGHT :right
   KeyEvent/VK_HOME :home KeyEvent/VK_END :end KeyEvent/VK_PAGE_UP :pgup KeyEvent/VK_PAGE_DOWN :pgdn
   KeyEvent/VK_DELETE :delete KeyEvent/VK_INSERT :insert KeyEvent/VK_ENTER :enter
   KeyEvent/VK_BACK_SPACE :backspace KeyEvent/VK_ESCAPE :esc KeyEvent/VK_TAB :tab})

(defn pressed->key
  "keyPressed のキーコードと修飾キー → TUI のキー。文字キー (keyTyped で扱う) は nil。
   F1〜F10 (Shift で :shift-f)、Ctrl+矢印、Ctrl+英字、Ctrl+6 (Ctrl+^)、Alt+数字 (F キーの代わり) も扱う"
  [code ctrl? shift? alt?]
  (cond
    (<= KeyEvent/VK_F1 code KeyEvent/VK_F10)
    (keyword (str (if shift? "shift-" "") "f" (inc (- code KeyEvent/VK_F1))))

    (and ctrl? (#{KeyEvent/VK_LEFT KeyEvent/VK_RIGHT KeyEvent/VK_UP KeyEvent/VK_DOWN} code))
    (keyword (str "ctrl-" (name (vk-keys code))))

    (vk-keys code) (vk-keys code)

    (and ctrl? (= code KeyEvent/VK_6)) :ctrl-caret
    (and ctrl? (<= KeyEvent/VK_A code KeyEvent/VK_Z))
    (keyword (str "ctrl-" (char (+ (int \a) (- code KeyEvent/VK_A)))))

    (and alt? (<= KeyEvent/VK_0 code KeyEvent/VK_9))
    (let [d (- code KeyEvent/VK_0)] (keyword (str "f" (if (zero? d) 10 d))))

    :else nil))

(defn typed->key
  "keyTyped の文字 → TUI のキー (印字できる文字だけ。Ctrl・Alt 付きは pressed->key が扱う)"
  [ch ctrl? alt?]
  (when (and (not ctrl?) (not alt?) (>= (int ch) 32) (not= (int ch) 127)
             (not= ch KeyEvent/CHAR_UNDEFINED))
    ch))

;; ============================================================
;; 2. 描画 (render-lines の結果を描く)
;; ============================================================
(def bg      (Color. 0x1E 0x22 0x28))
(def fg      (Color. 0xDC 0xDF 0xE4))
(def dim     (Color. 0x8A 0x90 0x99))
(def hl-bg   (Color. 0x3A 0x6E 0xA5))
(def head-bg (Color. 0x2E 0x34 0x40))
(def range-bg (Color. 0x33 0x3B 0x2E))

(defn mono-font
  "日本語も等幅で出せるフォント (無ければ Java の Monospaced)"
  [size]
  (let [names (set (.getAvailableFontFamilyNames (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)))
        nm (or (some names ["Noto Sans Mono CJK JP" "Noto Sans Mono CJK SC" "Noto Sans Mono"]) "Monospaced")]
    (Font. nm Font/PLAIN (int size))))

(defn cell-size
  "1桁の幅と1行の高さ [w h]"
  [^Graphics2D g ^Font font]
  (let [fm (.getFontMetrics g font)]
    [(.charWidth fm \M) (+ (.getHeight fm) 2)]))

(defn- col->x "文字の位置 i (行の先頭から) → 桁数 (全角は 2 桁)" [^String l i]
  (reduce + (map tb/char-width (subs l 0 (min i (count l))))))

(defn paint-screen!
  "render-lines の結果を描く。cw ch は1桁の幅と1行の高さ"
  [^Graphics2D g {:keys [lines cursor-line cursor-span range-lines]} cw ch w h]
  (.setRenderingHint g RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
  (.setColor g bg) (.fillRect g 0 0 w h)
  (let [fm (.getFontMetrics g) asc (.getAscent fm) last-i (dec (count lines))]
    (doseq [[i ^String l] (map-indexed vector lines)]
      (let [y (* i ch)
            row-bg (cond (= i 0) head-bg
                         (and (= i cursor-line) (nil? cursor-span)) hl-bg
                         (contains? range-lines i) range-bg)]
        (when row-bg (.setColor g row-bg) (.fillRect g 0 y w ch))
        (when (and (= i cursor-line) cursor-span)
          (let [[a b] cursor-span]
            (.setColor g hl-bg)
            (.fillRect g (* cw (col->x l a)) y (* cw (- (col->x l b) (col->x l a))) ch)))
        (.setColor g (if (or (= i 1) (= i last-i)) dim fg))
        ;; 1文字ずつ桁に合わせて置く (全角は 2 桁)
        (loop [k 0 x 0]
          (when (< k (count l))
            (let [c (.charAt l k)]
              (.drawString g (str c) (int (* cw x)) (int (+ y asc 1)))
              (recur (inc k) (+ x (tb/char-width c))))))))))

;; ============================================================
;; 2.5 ピアノロール
;; ============================================================
(defonce roll-opts (atom {:on? true :view :roll :beat-px 32 :score-px 2.0 :off 0 :grid-div 16}))   ; :beat-px は1拍の幅 (ピクセル)、:grid-div はグリッド (16 = 16分音符、0 = なし)
(defonce ^:private drag (atom nil))   ; ドラッグ中の音 {:mode :move|:resize :note 音 :tick0 :key0 :preview {:tick :key :len}}

(defn cycle-view "F11: ピアノロール → 楽譜 → グラフ → 楽譜とグラフ → エクスクルーシブ → 表示なし → ピアノロール" [o]
  (if-not (:on? o)
    (assoc o :on? true :view :roll)
    (case (:view o)
      :roll (assoc o :view :score)
      :score (assoc o :view :graph)
      :graph (assoc o :view :both)
      :both (assoc o :view :exc)
      (assoc o :on? false))))

(defn zoom-view "横の拡大・縮小 (今の表示のほう)" [o k]
  (if (#{:score :graph :both} (:view o))
    (update o :score-px #(-> (* % k) (max 0.4) (min 8.0)))
    (update o :beat-px #(-> (* % k) (max 4) (min 256)))))

(defn grid-ticks "グリッドの tick 数 (なしなら 1)" [tb div]
  (if (pos? div) (max 1 (quot (* tb 4) div)) 1))

(defn- snap [t grid] (* grid (Math/round (/ (double t) grid))))

(defn drag-preview
  "ドラッグの今の位置 (tick key) → 音の新しい {:tick :key :len}。
   動かした量がグリッドの半分に満たなければ時刻・長さは元のまま (グリッドから外れた音を勝手に動かさない)"
  [{:keys [mode note tick0 key0]} tick key grid]
  (let [dt (- tick tick0) small? (< (Math/abs (double dt)) (/ grid 2.0))]
    (case mode
      :move   {:tick (if small? (:tick note) (max 0 (snap (+ (:tick note) dt) grid)))
               :key (-> (+ (:key note) (- key key0)) (max 0) (min 127))
               :len (:len note)}
      :resize {:tick (:tick note) :key (:key note)
               :len (if small? (:len note) (-> (- (snap (+ (:tick note) (:len note) dt) grid) (:tick note)) (max 1) (min 255)))})))

(defn drag-op
  "ドラッグの結果 → roll-edit の [op args] (変わらなければ nil)"
  [{:keys [note]} {:keys [tick key len]}]
  (cond
    (not= len (:len note))                        [:gt [(:idx note) len]]
    (not= tick (:tick note))                      [:move [(:idx note) tick key]]
    (not= key (:key note))                        [:key [(:idx note) key]]
    :else nil))
(def roll-text-cols "ピアノロールを出すとき、文字の画面に残す桁数" 50)
(def ^:private kbd-w 34)

(defn roll-rect
  "ピアノロールを描く領域 [x y w h]。表示しないとき (切り替えで消した・編集画面でない・ヘルプ・狭い) は nil"
  [ui cw ch w]
  (when (and (:on? @roll-opts) (= :edit (:screen ui)) (not (:help? ui)))
    (let [x (* cw roll-text-cols) rw (- w x)]
      (when (> rw 200) [x ch rw (* ch (- (:rows ui) 3))]))))

(defonce ^:private roll-cache (atom {}))
(defn- cached-roll "トラック tr のロールの計算 (レコード列が同じなら前の結果を使う)" [tr recs]
  (let [[r d] (get @roll-cache tr)]
    (if (identical? r recs)
      d
      (let [d (roll/roll-data recs)] (swap! roll-cache assoc tr [recs d]) d))))

(def ^:private black-keys #{1 3 6 8 10})

(defn- vel-color "ベロシティ → 色 (弱いと青、強いと赤)" [v same?]
  (Color/getHSBColor (float (* 0.62 (- 1.0 (/ (min v 127) 127.0)))) (float (if same? 0.25 0.65)) (float (if same? 0.6 0.95))))

(defn paint-roll!
  "ピアノロールを描き、マウス用の位置情報を roll-opts の :geom に残す"
  [^Graphics2D g ui [x y w h]]
  (let [s (:s ui) tr (:tr s)
        recs (ss/cur-recs s)
        data (cached-roll tr recs)
        tb (or (get-in s [:song :timebase]) 48)
        [_ den] (or (get-in s [:song :time-sig]) [4 4])
        beat (max 1 (quot (* tb 4) (max 1 den)))
        px (/ (double (:beat-px @roll-opts)) beat)
        x0 (+ x kbd-w)
        ;; 1段の高さ: 音域 + 上下 4 段 (最低 2 オクターブ) が収まるように、6〜18 ピクセル
        ks (map :key (:notes data))
        span (if (seq ks) (+ 9 (- (apply max ks) (apply min ks))) 24)
        key-h (-> (quot h (max 24 span)) (max 6) (min 18))
        nrows (max 1 (quot h key-h))
        pos (:pos s)
        cur (get recs pos)
        ct (roll/cursor-tick data pos)
        lo (roll/key-window data (when (and cur (< (cur 0) 0x80) (pos? (cur 2))) (cur 0)) nrows)
        off (roll/scroll-offset (:off @roll-opts) ct (/ (- w kbd-w) px))
        bottom (+ y (* nrows key-h))
        tx (fn [t] (int (+ x0 (* px (- t off)))))
        ky (fn [k] (- bottom (* key-h (inc (- k lo)))))
        t-end (+ off (/ (- w kbd-w) px))
        clip (.getClip g)]
    (swap! roll-opts assoc :off off :geom {:x0 x0 :px px :off off :lo lo :bottom bottom :key-h key-h :rect [x y w h]})
    (.setClip g (int x) (int y) (int w) (int h))
    ;; 背景: 黒鍵の段を暗く、C の段の下に線
    (.setColor g (Color. 0x16 0x19 0x1E)) (.fillRect g x y w h)
    (doseq [k (range lo (min 128 (+ lo nrows)))]
      (when (black-keys (mod k 12))
        (.setColor g (Color. 0x12 0x14 0x18)) (.fillRect g x0 (ky k) w key-h))
      (when (zero? (mod k 12))
        (.setColor g (Color. 0x2A 0x2F 0x38)) (.drawLine g x0 (+ (ky k) key-h -1) (+ x w) (+ (ky k) key-h -1))))
    ;; 拍と小節の線
    (let [bars (:bars data)]
      (doseq [[b nb] (map vector bars (concat (rest bars) [(max t-end (:end data))]))
              t (range b nb beat)
              :when (<= off t t-end)]
        (.setColor g (if (= t b) (Color. 0x55 0x5E 0x6C) (Color. 0x26 0x2B 0x33)))
        (.drawLine g (tx t) y (tx t) bottom)))
    ;; ほかのトラックの音 (薄く)
    (.setColor g (Color. 0x3A 0x40 0x4A))
    (doseq [[i t] (map-indexed vector (get-in s [:song :tracks]))
            :when (and (not= i tr) (not (:mute? t)) (seq (:recs t)))
            {:keys [tick len key]} (:notes (cached-roll i (:recs t)))
            :when (and (< tick t-end) (> (+ tick len) off) (<= lo key (+ lo nrows -1)))]
      (.fillRect g (tx tick) (+ (ky key) 2) (max 2 (int (* px len))) (- key-h 4)))
    ;; 今のトラックの音
    (doseq [{:keys [idx tick len key vel same?]} (:notes data)
            :when (and (< tick t-end) (> (+ tick len) off) (<= lo key (+ lo nrows -1)))]
      (let [cx (tx tick) cw (max 3 (int (* px len))) cy (ky key)]
        (.setColor g (if (= idx pos) (Color. 0xFF 0xB0 0x40) (vel-color vel same?)))
        (.fillRect g cx (inc cy) cw (- key-h 1))
        (when (= idx pos)
          (.setColor g Color/WHITE) (.drawRect g cx cy (dec cw) key-h))))
    ;; ドラッグ中の音の行き先 (枠だけ)
    (when-let [{:keys [tick key len]} (:preview @drag)]
      (.setColor g Color/WHITE)
      (.drawRect g (tx tick) (ky key) (max 3 (int (* px len))) key-h))
    ;; カーソルの位置
    (.setColor g (Color. 0xE8 0xD0 0x50))
    (.drawLine g (tx ct) y (tx ct) bottom)
    ;; 鍵盤
    (doseq [k (range lo (min 128 (+ lo nrows)))]
      (.setColor g (if (black-keys (mod k 12)) (Color. 0x30 0x30 0x34) (Color. 0xD8 0xD8 0xD8)))
      (.fillRect g x (ky k) (- kbd-w 1) (dec key-h))
      (when (zero? (mod k 12))
        (.setColor g (Color. 0x30 0x30 0x34))
        (.setFont g (.deriveFont (.getFont g) (float (min 11 (max 8 (dec key-h))))))
        (.drawString g (str "C" (dec (quot k 12))) (int (+ x 2)) (int (+ (ky k) key-h -1)))))
    (.setClip g clip)))

(defn roll-point
  "ピアノロールの点 (mx my) → {:tick :key}。clamp? なら領域の外も端に寄せて返す (ドラッグ用)。領域外は nil"
  ([mx my] (roll-point mx my false))
  ([mx my clamp?]
   (when-let [{:keys [x0 px off lo bottom key-h rect]} (:geom @roll-opts)]
     (let [[x y w _] rect]
       (when (or clamp? (and (<= x0 mx (+ x w)) (<= y my bottom)))
         {:tick (long (Math/floor (+ off (/ (- mx x0) px))))
          :key (-> (+ lo (Math/floorDiv (int (- bottom my)) (int key-h))) (max 0) (min 127))})))))

(defn- cur-roll [ui] (cached-roll (:tr (:s ui)) (ss/cur-recs (:s ui))))

(defn roll-hit
  "ピアノロールの点 (mx my) → 移動先の行番号 (音の上ならその行、何も無い所ならその tick の行)。領域外は nil"
  [ui mx my]
  (when-let [{:keys [tick key]} (roll-point mx my)]
    (let [data (cur-roll ui)]
      (or (roll/note-at data tick key) (roll/idx-at-tick data tick)))))

(defn- near-end?
  "点 mx が音の右端の近く (4 ピクセル以内、短い音は右 1/3) か"
  [note mx]
  (let [{:keys [x0 px off]} (:geom @roll-opts)
        end-x (+ x0 (* px (- (+ (:tick note) (:len note)) off)))
        w (* px (:len note))]
    (<= (- end-x (min 4 (/ w 3))) mx (+ end-x 1))))

;; ============================================================
;; 2.7 楽譜表示 (原作 SCORE.C gra_gakufu。計算は polysted_score.clj)
;; ============================================================
;; 記号のフォントが無くても描けるよう、音符・休符・記号は図形で描く (クレフは G と F の文字)【設計判断】
(defonce ^:private score-cache (atom {}))
(defn- cached-score [tr recs opts]
  (let [[r o d] (get @score-cache tr)]
    (if (and (identical? r recs) (= o opts))
      d
      (let [d (score/score-items recs opts)] (swap! score-cache assoc tr [recs opts d]) d))))

(defn- cur-score [ui]
  (let [s (:s ui) song (:song s)]
    (cached-score (:tr s) (ss/cur-recs s)
                  {:timebase (or (:timebase song) 48) :key (or (:key song) 0) :rhythm? (boolean (:rhythm? (ss/cur-track s)))})))

(def ^:private flag-count [0 0 0 0 0 1 1 2 2 3 3 4])
(def ^:private ink (Color. 0xE6 0xE6 0xE6))
(def ^:private staff-ink (Color. 0x8A 0x90 0x99))

(defn- glyph "臨時記号の文字 (フォントに無ければ # b n)" [^Graphics2D g acc]
  (let [[u a] (case acc :sharp ["♯" "#"] :flat ["♭" "b"] ["♮" "n"])]
    (if (.canDisplay (.getFont g) (int (.charAt ^String u 0))) u a)))

(defonce ^:private graph-cache (atom {}))
(defn- cur-graph [ui]
  (let [s (:s ui) tr (:tr s) recs (ss/cur-recs s) tb (or (get-in s [:song :timebase]) 48)
        [r t d] (get @graph-cache tr)]
    (if (and (identical? r recs) (= t tb))
      d
      (let [d (score/ctrl-graph recs {:timebase tb})] (swap! graph-cache assoc tr [recs tb d]) d))))

;; グラフの色と線 (原作は 6 つの値を色分けし、データエントリとパンは点線)
(def ^:private graph-styles
  {:pitch [(Color. 0xE0 0x6C 0xD0) false "PITCH"] :vol [(Color. 0xE8 0xD0 0x50) false "VOL"]
   :expr [(Color. 0x60 0xC8 0x60) false "EXPR"] :modu [(Color. 0x50 0xB0 0xE0) false "MOD"]
   :entry [(Color. 0xB0 0xB0 0xB0) true "ENTRY"] :pan [(Color. 0xE0 0x8A 0x50) true "PAN"]})

(defn- draw-graph!
  "コントローラのグラフ (原作 SCORE.C gra_special)。sx は楽譜の x → 画面の x"
  [^Graphics2D g ui sx head-x [x y w h]]
  (let [{:keys [changes vels bars]} (cur-graph ui)
        top (+ y 18) bot (+ y h -6)
        vy (fn [v] (int (- bot (/ (* (- bot top) (min 127 (max 0 v))) 127.0))))
        dashed (java.awt.BasicStroke. 1.0 java.awt.BasicStroke/CAP_BUTT java.awt.BasicStroke/JOIN_MITER 1.0 (float-array [4 3]) 0.0)
        solid (.getStroke g)
        right (+ x w)]
    ;; 見出しと色の説明、目盛り
    (.setColor g dim)
    (.drawString g "CONTROLLER" (int (+ x 6)) (int (+ y 14)))
    (doseq [[i [k [col _ label]]] (map-indexed vector (concat (map (fn [[k]] [k (graph-styles k)]) score/graph-ctls)
                                                             [[:vel [ink false "VEL"]]]))]
      (.setColor g col) (.drawString g (str label) (int (+ head-x (* i 56))) (int (+ y 14))) k)
    (.setColor g staff-ink)
    (doseq [v [0 64 127]] (.drawString g (str v) (int (+ x 6)) (int (+ (vy v) 4))))
    (.setColor g (Color. 0x2A 0x2F 0x38))
    (doseq [v [0 32 64 96 127]] (.drawLine g head-x (vy v) right (vy v)))
    (.setClip g (int (- head-x 4)) (int y) (int (- right head-x -4)) (int h))
    ;; 小節線
    (.setColor g (Color. 0x55 0x5E 0x6C))
    (doseq [b bars] (let [bx (int (sx (inc b)))] (.drawLine g bx top bx bot)))
    ;; ベロシティの棒 (高さが VEL、長さが GT。GT が ST より長い音は下まで落とさない)
    (.setColor g ink)
    (doseq [{:keys [x len vel legato?]} vels
            :let [x0 (int (sx x)) x1 (int (sx (+ x len)))]
            :when (and (< x0 (+ right 4)) (> x1 (- head-x 4)))]
      (.drawLine g x0 bot x0 (vy vel)) (.drawLine g x0 (vy vel) x1 (vy vel))
      (when-not legato? (.drawLine g x1 (vy vel) x1 bot)))
    ;; 6 つの値 (段の形)
    (doseq [[k dflt] score/graph-ctls
            :let [[col dash?] (graph-styles k)
                  pts (concat [[-1000000 dflt]] (map (juxt :x :v) (filter #(= k (:ctl %)) changes)))]]
      (.setColor g col)
      (.setStroke g (if dash? dashed solid))
      (doseq [[[x0 v0] [x1]] (map vector pts (concat (rest pts) [[Integer/MAX_VALUE]]))
              :let [a (if (neg? x0) (- head-x 10) (int (sx x0)))
                    b (if (= x1 Integer/MAX_VALUE) right (int (sx x1)))]
              :when (and (< a right) (> b (- head-x 10)))]
        (.drawLine g a (vy v0) (max a (min b right)) (vy v0))
        (when (< x1 Integer/MAX_VALUE)
          (let [v1 (some #(when (= (:x %) x1) (:v %)) (filter #(= k (:ctl %)) changes))]
            (when v1 (.drawLine g b (vy v0) b (vy v1)))))))
    (.setStroke g solid)
    (.setClip g nil)))

(defn paint-score!
  "楽譜とコントローラのグラフ (表示の :view が :score / :graph / :both) を描き、マウス用の位置情報を roll-opts の :sgeom に残す"
  [^Graphics2D g ui [x y w h]]
  (let [view (:view @roll-opts)
        h0 h
        sh (case view :score h :both (int (* h 0.58)) nil)       ; 楽譜の高さ (グラフだけなら nil)
        h (or sh h)
        s (:s ui)
        rhythm? (boolean (:rhythm? (ss/cur-track s)))
        base-key (if rhythm? 0 (or (get-in s [:song :key]) 0))
        px (double (:score-px @roll-opts))
        h2 (-> (quot h 46) (max 4) (min 10))                      ; 段の間隔 (線と線の半分)
        ;; 横の位置: 要素の幅は「符頭・旗・臨時記号が収まる最小の幅」と「長さ × px」の大きいほう
        lay (score/layout (cur-score ui) {:px px :head-w (* 2.6 h2) :note-w (* 6.0 h2) :acc-w (* 2.4 h2)
                                          :rest-w (* 4.0 h2) :bar-w (* 2.0 h2) :key-w (* 1.6 h2)})
        items (:items lay)
        bar-w (* 2 h2)
        head-x (+ x 34 (* 9 h2))                                  ; クレフと調号の後
        vw (- (+ x w) head-x 12)
        ct (score/cursor-dx lay (:pos s))
        off (roll/scroll-offset (:soff @roll-opts) ct vw)
        ftop 52 fbot 18
        y0 (+ y (quot (- h (* (- ftop fbot) h2)) 2))
        fy (fn [f] (int (+ y0 (* (- ftop f) h2))))
        tx (fn [dx] (int (+ head-x (- dx off))))
        cur (score/item-at items (:pos s))
        font (.getFont g)
        clip (.getClip g)
        sig! (fn [skey sx]                                        ; 調号 (ト音記号と、14 段下のヘ音記号)
               (let [[kind n] (score/key-sig skey) steps (if (= kind :sharp) score/sharp-steps score/flat-steps)]
                 (.setFont g (.deriveFont font (float (* 2.6 h2))))
                 (doseq [i (range n) d [0 14]]
                   (.drawString g (str (glyph g kind)) (int (+ sx (* i (int (* 1.6 h2))))) (int (+ (fy (- (steps i) d)) h2))))
                 (.setFont g font)))
        ;; 表示の始まりの調 (小節線で曲の調に戻り、0xF5 で変わる)
        start-key (reduce (fn [k it] (case (:kind it) :bar base-key :key (:skey it) k)) base-key
                          (take-while #(< (:dx %) off) items))]
    (swap! roll-opts assoc :soff off :sgeom {:head-x head-x :off off :lay lay :rect [x y w h]})
    (.setClip g (int x) (int y) (int w) (int h))
    (.setColor g (Color. 0x16 0x19 0x1E)) (.fillRect g x y w h)
    (when sh
    ;; 譜線 (ト音記号 E4〜F5、ヘ音記号 G2〜A3)、左のつなぎの線、クレフ、調号
    (.setColor g staff-ink)
    (doseq [f [37 39 41 43 45 25 27 29 31 33]] (.drawLine g (+ x 6) (fy f) (+ x w) (fy f)))
    (.drawLine g (+ x 6) (fy 45) (+ x 6) (fy 25))
    (.setColor g ink)
    (.setFont g (Font. "Serif" Font/BOLD (int (* 5 h2))))
    (.drawString g "G" (int (+ x 10)) (int (+ (fy 39) (* 2 h2))))
    (.setFont g (Font. "Serif" Font/BOLD (int (* 4 h2))))
    (.drawString g "F" (int (+ x 10)) (int (+ (fy 31) (* 1.6 h2))))
    (.setFont g font)
    (sig! start-key (+ x 14 (* 4 h2)))
    (.setClip g (int (- head-x 4)) (int y) (int (- (+ x w) head-x -4)) (int h))
    ;; 要素
    (doseq [{:keys [kind notes type] :as it} items
            :let [ix (tx (:dx it))]
            :when (< (- x 60) ix (+ x w 20))]
      (let [hi? (= it cur)
            col (if hi? (Color. 0xFF 0xB0 0x40) ink)]
        (.setColor g col)
        (case kind
          :bar (do (.setColor g staff-ink) (.drawLine g (+ ix (quot bar-w 2)) (fy 45) (+ ix (quot bar-w 2)) (fy 25))
                   (when (:same? it) (.setColor g col) (.drawString g "SAME" (int (+ ix 3)) (int (- (fy 45) 4)))))
          :rep-start (doseq [[a b] [[45 37] [33 25]]]
                       (.drawLine g (+ ix 3) (fy a) (+ ix 3) (fy b)) (.drawLine g (+ ix 3) (fy a) (+ ix 6) (fy a)) (.drawLine g (+ ix 3) (fy b) (+ ix 6) (fy b)))
          :rep-end (doseq [[a b] [[45 37] [33 25]]]
                     (.drawLine g ix (fy a) ix (fy b)) (.drawLine g (- ix 3) (fy a) ix (fy a)) (.drawLine g (- ix 3) (fy b) ix (fy b)))
          :key (sig! (:skey it) (+ ix (quot bar-w 2)))
          :rest (let [cx (tx (:hx it))]
                  (doseq [mid [41 29]]                            ; ト音記号とヘ音記号の両方 (原作どおり)
                    (cond
                      (= type 0) (.fillRect g (- cx h2) (fy (+ mid 2)) (* 2 h2) h2)
                      (<= type 2) (.fillRect g (- cx h2) (- (fy mid) h2) (* 2 h2) h2)
                      (<= type 4) (.drawPolyline g (int-array [(- cx 2) (+ cx 2) (- cx 2) (+ cx 2) (- cx 1)])
                                                 (int-array (map #(+ (fy mid) (* % h2)) [-3 -1 0 1 3])) 5)
                      :else (let [n (flag-count type)]
                              (.drawLine g (+ cx 3) (- (fy mid) (* 2 h2)) (- cx 1) (+ (fy mid) (* 2 h2)))
                              (dotimes [k n] (.fillOval g (- cx 3 (- k)) (+ (- (fy mid) (* 2 h2)) (* k 2 h2) -1) 4 4))))
                    (when (and (odd? type) (< type 11)) (.fillOval g (+ cx (* 2 h2)) (- (fy mid) 2) 3 3))))
          :chord (let [cx (tx (:hx it))
                       hw (int (* 2.6 h2)) hh (int (* 1.8 h2))
                       fs (map :f notes)]
                   (doseq [{:keys [f acc]} notes]
                     (let [ny (fy f)]
                       ;; 加線
                       (.setColor g staff-ink)
                       (doseq [lf (concat (when (>= f 47) (range 47 (inc f) 2)) (when (= f 35) [35]) (when (<= f 23) (range 23 (dec f) -2)))]
                         (.drawLine g (- cx (int (* 1.8 h2))) (fy lf) (+ cx (int (* 1.8 h2))) (fy lf)))
                       (.setColor g col)
                       (if (<= type 2)
                         (.drawOval g (- cx (quot hw 2)) (- ny (quot hh 2)) hw hh)
                         (.fillOval g (- cx (quot hw 2)) (- ny (quot hh 2)) hw hh))
                       (when acc
                         (.setFont g (.deriveFont font (float (* 2.6 h2))))
                         (.drawString g (str (glyph g acc)) (int (- cx hw (* 1.4 h2))) (int (+ ny h2)))
                         (.setFont g font))
                       (when (and (odd? type) (< type 11))
                         (.fillOval g (+ cx (quot hw 2) 3) (- ny 2) 3 3))))
                   (when (and (seq fs) (pos? type))           ; 符尾 (上向き、原作どおり) と旗
                     (let [sx (+ cx (quot hw 2) -1) top (- (fy (apply max fs)) (* 7 h2)) bot (fy (apply min fs))]
                       (.drawLine g sx bot sx top)
                       (dotimes [k (flag-count type)]
                         (.drawLine g sx (+ top (* k 2 h2)) (+ sx (* 2 h2)) (+ top (* k 2 h2) (* 3 h2)))))))
          nil))))
    ;; コントローラのグラフ (楽譜の下、またはグラフだけ)
    (when (#{:graph :both} view)
      (let [[gy gh] (if sh [(+ y sh) (- h0 sh)] [y h0])
            xd (score/x->dx lay px)]
        (.setClip g (int x) (int gy) (int w) (int gh))
        (.setColor g (Color. 0x16 0x19 0x1E)) (.fillRect g x gy w gh)
        (when sh (.setColor g staff-ink) (.drawLine g x gy (+ x w) gy))
        (draw-graph! g ui #(tx (xd %)) head-x [x gy w gh])))
    ;; カーソルの位置
    (.setClip g (int x) (int y) (int w) (int h0))
    (.setColor g (Color. 0xE8 0xD0 0x50))
    (.drawLine g (tx ct) y (tx ct) (+ y h0))
    (.setClip g clip)))

(defn score-hit
  "楽譜の点 mx → その位置の要素の最初の行 (最後より後なら END)。表示していなければ nil"
  [mx]
  (when-let [{:keys [head-x off lay rect]} (:sgeom @roll-opts)]
    (let [[x _ w _] rect]
      (when (<= x mx (+ x w))
        (score/hit-dx lay (+ off (- mx head-x)))))))

(defn paint-exc!
  "エクスクルーシブの表示 (原作 EXCLU.C exc_view)。カーソルの行が Tr.Exclu か U0〜U7 のとき、データを 16 バイトずつ。
   SC-55 のドットのデータは 16×16 の点で描く"
  [^Graphics2D g ui [x y w h]]
  (let [s (:s ui)
        v (polysted-ctrl/exc-view (ss/cur-recs s) (:pos s) (ss/uex-table s))
        fm (.getFontMetrics g) lh (+ 2 (.getHeight fm)) cw (.charWidth fm \M)
        clip (.getClip g)]
    (.setClip g (int x) (int y) (int w) (int h))
    (.setColor g (Color. 0x16 0x19 0x1E)) (.fillRect g x y w h)
    (if-not v
      (do (.setColor g dim)
          (.drawString g "カーソルをエクスクルーシブ (Tr.Exclu か U0〜U7) の行に置くと、データを表示します" (int (+ x 10)) (int (+ y lh))))
      (let [lines (polysted-ctrl/exc-view-lines v false)
            dots (:dots (polysted-ctrl/sc55-display (:data v)))]
        (doseq [[i l] (map-indexed vector lines)]
          (.setColor g (cond (zero? i) (Color. 0xE8 0xD0 0x50) (= i 2) dim :else fg))
          (loop [k 0 cx 0]                                        ; 桁に合わせて 1 文字ずつ
            (when (< k (count l))
              (let [c (.charAt ^String l k)]
                (.drawString g (str c) (int (+ x 10 (* cw cx))) (int (+ y (* (inc i) lh))))
                (recur (inc k) (+ cx (tb/char-width c)))))))
        (when dots                                                ; SC-55 の液晶のドット
          (let [top (+ y (* (+ 1 (count lines)) lh)) d (max 6 (min 14 (quot (- (+ y h) top 10) 16)))]
            (.setColor g (Color. 0x9C 0xB0 0x48)) (.fillRect g (+ x 10) top (+ (* 16 d) 8) (+ (* 16 d) 8))
            (.setColor g (Color. 0x20 0x28 0x10))
            (doseq [[r row] (map-indexed vector dots) [c on?] (map-indexed vector row) :when on?]
              (.fillRect g (+ x 14 (* c d)) (+ top 4 (* r d)) (dec d) (dec d)))))))
    (.setClip g clip)))

(defn paint-all!
  "文字の画面と、出すなら右側のピアノロールか楽譜を描く"
  [^Graphics2D g ui cw ch w h]
  (paint-screen! g (tui/render-lines ui) cw ch w h)
  (if-let [r (roll-rect ui cw ch w)]
    (case (:view @roll-opts)
      (:score :graph :both) (do (swap! roll-opts dissoc :geom) (paint-score! g ui r))
      :exc (do (swap! roll-opts dissoc :geom :sgeom) (paint-exc! g ui r))
      (do (swap! roll-opts dissoc :sgeom) (paint-roll! g ui r)))
    (swap! roll-opts dissoc :geom :sgeom)))   ; 出していないときはマウスの位置を解釈しない

;; ============================================================
;; 2.6 コントローラのビジュアルエディット (原作 VISUAL.C。計算は polysted_visual.clj)
;; ============================================================
;; 編集の状態 ve: {:vmo :vmo2 :para :step :cx :ccx (SUB、nil なら無し) :from :len :ost :changed? :maxst :msg}
(def ^:private wave-keys
  {:shift-f1 0 :shift-f2 1 :f3 2 :shift-f3 2 :f4 3 :shift-f4 3 :f5 4 :shift-f5 4 :f6 5 :shift-f6 5
   :f7 6 :shift-f7 6 :f8 7 :shift-f8 7 :f9 8 :shift-f9 8 :f10 9 :shift-f10 9})

(defn- total-st [ve] (reduce + (:step ve)))

(defn vis-prompt
  "キー k が数の入力を要るなら [見出し 既定値]。要らなければ nil (原作の vinput の位置と既定値)"
  [ve k]
  (let [cx (:cx ve)]
    (case k
      (\x \X) ["STEP=" 2]
      (\z \Z) ["STEP=" (total-st ve)]
      \i (when (< (count (:para ve)) 768) ["Insert STEP=" 0])
      \I ["Change STEP=" ((:step ve) cx)]
      \D ["Delete STEP=" ((:step ve) cx)]
      (:shift-f8 :shift-f9 :shift-f10) ["wave count=" 1]
      nil)))

(defn vis-key
  "ビジュアルエディットの1キー (純粋)。n は vis-prompt で聞いた数 (要らないキーは nil)。
   => 新しい ve (Enter・Esc は GUI 側で扱う)"
  [ve k n]
  (let [{:keys [para step cx ccx vmo maxst]} ve
        co (count para) st (total-st ve)
        pitch? (polysted-visual/pitch? vmo)
        ys (if pitch? 16 1) yss (if pitch? 682 10)
        xss (cond (> co 384) 24 (> co 192) 12 :else 6)
        setv (fn [ve v] (assoc-in ve [:para cx] (polysted-visual/clamp-value vmo v)))
        ve (dissoc ve :msg)
        changed (fn [ve pts] (-> ve (merge pts) (assoc :changed? true) (update :cx #(min % (dec (count (:para pts))))) (assoc :ccx nil)))]
    (cond
      (= k :left)       (assoc ve :cx (mod (dec cx) co))
      (= k :right)      (assoc ve :cx (mod (inc cx) co))
      (= k :ctrl-left)  (assoc ve :cx (mod (- cx xss) co))
      (= k :ctrl-right) (assoc ve :cx (mod (+ cx xss) co))
      (#{:f1 :home} k)  (assoc ve :cx 0)
      (#{:f2 :end} k)   (assoc ve :cx (dec co))
      (= k :up)         (setv ve (+ (para cx) ys))
      (= k :down)       (setv ve (- (para cx) ys))
      (= k :pgup)       (setv ve (+ (para cx) yss))
      (= k :pgdn)       (setv ve (- (para cx) yss))
      (and (char? k) (<= (int \0) (int k) (int \9)) (not pitch?)) (setv ve (* 14 (- (int k) (int \0))))
      (= k \space)      (assoc ve :ccx (if ccx nil cx))
      (wave-keys k)
      (let [shift? (str/starts-with? (name k) "shift-")
            ln (if ccx (inc (Math/abs (long (- cx ccx)))) co)
            xc (if (and shift? (>= (wave-keys k) 7)) (* 2 (or n 0)) 2)]
        (if (or (< xc 2) (> xc ln))
          (assoc ve :msg (str "波の数は 1〜" (quot ln 2) " です"))
          (cond-> (merge ve (polysted-visual/wave {:para para :step step} (wave-keys k)
                                              {:cx cx :ccx ccx :shift? shift? :xc xc :st st :vmo vmo}))
            (nil? ccx) (assoc :changed? (or (:changed? ve) (not= step (polysted-visual/bunkatsu st co)))))))
      (#{\x \X} k) (if (and n (pos? n)) (changed ve (polysted-visual/cut-points ve n)) ve)
      (#{\z \Z} k) (cond (not (and n (pos? n))) ve
                         (> n maxst) (assoc ve :msg "データが多すぎます。")
                         :else (changed ve {:para para :step (polysted-visual/zoom-steps step n)}))
      (= k \i)     (cond (not (and n (pos? n))) ve
                         (> (+ st n) maxst) (assoc ve :msg "データが多すぎます。")
                         :else (changed ve (polysted-visual/insert-step ve cx n)))
      (= k \I)     (cond (not (and n (pos? n))) ve
                         (> (+ (- st (step cx)) n) maxst) (assoc ve :msg "データが多すぎます。")
                         :else (changed ve {:para para :step (assoc step cx n)}))
      (= k \d)     (if (and (> co 1) (> st (step cx))) (changed ve (polysted-visual/delete-point ve cx)) ve)
      (= k \D)     (if (and n (pos? n) (> co 1) (< n st)) (changed ve (polysted-visual/delete-st ve cx n)) ve)
      (#{\c \C} k) (-> (changed ve (polysted-visual/link-points ve)) (assoc :cx 0))
      ;; Y: 縦の拡大 (原作の yzm: ピッチは 1→2→3→6→1 倍で 0 を中心に、ほかは 1↔2 倍。
      ;;    原作の 2 倍は半分の高さの表示を全体に広げるもので、ここは初めから全体なので、カーソルの値のまわり 64 段を出す【設計判断】)
      (#{\y \Y} k) (let [z (or (:yzm ve) 1)
                           z (if (< z 3) (inc z) (let [z2 (* 2 z)] (if (> z2 6) 1 z2)))
                           z (if (and (not pitch?) (> z 2)) 1 z)
                           c (-> (para cx) (max 32) (min 95))]
                       (assoc ve :yzm z :yr (cond pitch? [(- (quot 8192 z)) (quot 8192 z)]
                                                  (= z 2) [(- c 32) (+ c 32)]
                                                  :else nil)))
      :else ve)))

(defn vis-geom
  "グラフの位置: {:gx :gy :gw :gh :xs [各点の左端の x と最後の右端]}"
  [ve w h]
  (let [gx 56 gy 64 gw (max 50 (- w gx 16)) gh (max 40 (- h gy 40))
        st (max 1 (total-st ve))
        cums (reductions + 0 (:step ve))
        [vlo vhi] (or (:yr ve) (if (polysted-visual/pitch? (:vmo ve)) [-8192 8192] [0 127]))]
    {:gx gx :gy gy :gw gw :gh gh :vlo vlo :vhi vhi :xs (mapv #(+ gx (quot (* % gw) st)) cums)}))

(defn vis-v->y "値 → y (表示範囲 :vlo〜:vhi をグラフの高さに)" [_vmo {:keys [gy gh vlo vhi]} v]
  (- (+ gy gh) (quot (* (- v vlo) gh) (max 1 (- vhi vlo)))))

(defn vis-y->v "y → 値 (表示範囲の中に収める)" [vmo {:keys [gy gh vlo vhi]} y]
  (polysted-visual/clamp-value vmo (-> (+ vlo (Math/round (/ (* (- (+ gy gh) y) (- vhi vlo)) (double gh))))
                                   (max vlo) (min vhi))))

(defn vis-hit "x → 点の番号 (グラフの外なら端の点)" [{:keys [xs]} x]
  (let [n (dec (count xs))]
    (-> (or (first (filter #(< x (xs (inc %))) (range n))) (dec n)) (max 0))))

(defn vis-draw
  "マウスで点 i の値を y に。直前に描いた点 prev があれば間を直線でつなぐ (原作のマウス)"
  [ve geom i y prev]
  (let [vmo (:vmo ve)
        ve (-> ve (assoc-in [:para i] (vis-y->v vmo geom y)) (assoc :cx i))]
    (if (and prev (not= prev i))
      (merge ve (select-keys (polysted-visual/line-between ve prev i vmo) [:para]))
      ve)))

(defn paint-visual!
  "ビジュアルエディットの画面を描く"
  [^Graphics2D g ve w h barlen]
  (let [{:keys [para cx ccx vmo]} ve
        {:keys [gx gy gw gh xs] :as geom} (vis-geom ve w h)
        st (total-st ve) co (count para)
        y (fn [v] (vis-v->y vmo geom v))
        {:keys [vlo vhi]} geom
        base (-> (if (polysted-visual/pitch? vmo) (y 0) (+ gy gh)) (max gy) (min (+ gy gh)))
        fm (.getFontMetrics g)]
    (.setRenderingHint g RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    (.setColor g bg) (.fillRect g 0 0 w h)
    ;; 状態 (原作の STEP= SUB= STEP TIME= PARA= TOTAL ST=)
    (.setColor g fg)
    (.drawString g (str "CONTROLLER VISUAL EDIT  " (polysted-visual/kind-name vmo)
                        (when (#{0xDE 0xED} vmo) (str " (" (:vmo2 ve) ")"))
                        "   STEP=" (inc cx) "/" co (when ccx (str "  SUB=" (inc ccx)))
                        "   STEP TIME=" ((:step ve) cx) "   PARA=" (para cx) "   TOTAL ST=" st
                        "   Y-ZOOM=" (or (:yzm ve) 1))
                 (int 8) (int (+ 4 (.getAscent fm))))
    (.setColor g (if (:msg ve) (Color. 0xFF 0xB0 0x40) dim))
    (doseq [[i l] (map-indexed vector
                               (if (:msg ve)
                                 [(:msg ve)]
                                 ["ドラッグで描く (上の余白で SUB、下の余白でカーソル)  ←→ 点 (Ctrl ×6)  ↑↓ ±1  PgUp/Dn ±10  0-9 ×14  Space SUB  Y 縦の拡大  M 試し再生 (Shift 混ぜて)"
                                  "F1/F2 端  S-F1 反転  S-F2 逆  F3 直線 (S-F3 乱数)  F5-F7 曲線・山  F8 sin  F9 三角  F10 矩形  X Z I D C ステップ  Enter 確定 (Shift 混ぜる)  Esc 取消"]))]
      (.drawString g (str l) (int 8) (int (+ 4 (.getAscent fm) (* (inc i) (.getHeight fm))))))
    ;; 背景・目盛り
    (.setColor g (Color. 0x16 0x19 0x1E)) (.fillRect g gx gy gw gh)
    (let [vals (cond (polysted-visual/pitch? vmo) (for [i (range -4 5)] (quot (* i vhi) 4))   ; 0 を中心に 8 等分
                     (:yr ve) (range vlo (inc vhi) 8)
                     :else (range 0 128 16))]
      (doseq [v vals]
        (.setColor g (Color. 0x2A 0x2F 0x38)) (.drawLine g gx (y v) (+ gx gw) (y v))
        (.setColor g dim) (.drawString g (str v) (int 4) (int (+ (y v) 4)))))
    (doseq [[t bar?] (concat (map vector (range 0 (inc st) (max 1 (quot barlen 4))) (repeat false))
                             (map vector (range 0 (inc st) (max 1 barlen)) (repeat true)))]
      (let [x (+ gx (quot (* t gw) (max 1 st)))]
        (.setColor g (if bar? (Color. 0x55 0x5E 0x6C) (Color. 0x2A 0x2F 0x38)))
        (.drawLine g x gy x (+ gy gh))
        (when bar? (.setColor g dim) (.drawString g (str t) (int (- x 8)) (int (+ gy gh 16))))))
    ;; SUB とカーソルの間、カーソルの点
    (when ccx
      (.setColor g (Color. 0x33 0x3B 0x2E))
      (let [[a b] (sort [cx ccx])] (.fillRect g (xs a) gy (max 1 (- (xs (inc b)) (xs a))) gh)))
    (.setColor g (Color. 0x2A 0x48 0x6E))
    (.fillRect g (xs cx) gy (max 2 (- (xs (inc cx)) (xs cx))) gh)
    ;; 値 (段の形。拡大で外に出た分は描かない)
    (.setClip g (int gx) (int gy) (int gw) (int (inc gh)))
    (doseq [i (range co)]
      (let [x0 (xs i) x1 (xs (inc i)) yv (y (para i))]
        (.setColor g (Color. 0x3A 0x6E 0xA5 0x90))
        (.fillRect g x0 (min yv base) (max 1 (- x1 x0)) (Math/abs (long (- base yv))))
        (.setColor g (if (= i cx) (Color. 0xFF 0xB0 0x40) (Color. 0xE8 0xD0 0x50)))
        (.fillRect g x0 (dec yv) (max 2 (- x1 x0)) 3)
        (when (pos? i)
          (.setColor g (Color. 0xE8 0xD0 0x50))
          (.drawLine g x0 (y (para (dec i))) x0 yv))))
    (.setClip g nil)))

(defn visual-begin
  "V を押したときの準備 (純粋)。mark は範囲の開始行 (nil なら範囲なし)、mode-text は EDIT MODE の入力。
   => {:error 理由} / {:ve 範囲の点} / {:new {:vmo :vmo2 :from :st-default | :fixed-st}} (新規。TOTAL ST と COUNT を聞く)"
  [s mark mode-text]
  (let [recs (ss/cur-recs s) pos (:pos s) cur (get recs pos)
        maxst (* 8 (ss/bar-length s))
        m (polysted-visual/parse-mode mode-text cur)
        m (cond-> m (= 0x98 (:vmo m)) (assoc :exc (polysted-visual/exc-tail recs pos)))]
    (cond
      (:error m) m
      mark
      (let [lo (min mark pos) hi (max mark pos)
            {:keys [para step matched st]} (polysted-visual/read-points recs lo (inc hi) (:vmo m))
            sum (reduce + step)]
        (cond
          (and (zero? matched) (> st 1)) {:new (assoc m :from lo :fixed-st (min st maxst))}
          (or (< (count para) 2) (< sum 2)) {:error "データが少なすぎます。"}
          (or (> (count para) 768) (> sum maxst)) {:error "データが多すぎます。"}
          :else {:ve (merge m {:para para :step step :cx 0 :ccx nil :from lo :len (inc (- hi lo))
                               :ost sum :changed? false :maxst maxst})}))
      :else {:new (assoc m :from pos :st-default (polysted-visual/default-total-st cur (ss/bar-length s)))})))

(defn visual-new
  "新規の点を作る (純粋)。st-text・co-text は TOTAL ST と COUNT の入力。=> {:ve} か {:error}"
  [s {:keys [vmo vmo2 exc from]} st-text co-text]
  (let [maxst (* 8 (ss/bar-length s))
        st (parse-long (str/trim (str st-text)))
        co (when st (polysted-visual/parse-count co-text st))]
    (cond
      (not (and st (<= 2 st maxst))) {:error (str "TOTAL ST は 2〜" maxst " です")}
      (not (polysted-visual/count-ok? co st)) {:error (str "COUNT は 2〜" (min st 768) " です")}
      :else {:ve (merge {:vmo vmo :vmo2 vmo2 :exc exc :cx 0 :ccx nil :from from :len 0 :ost st :changed? false :maxst maxst}
                        (polysted-visual/new-points (ss/cur-recs s) from vmo st co))})))

(defn render-image
  "画面を画像に描く (テスト・確認用。画面の無い環境でも動く)"
  ^BufferedImage [ui font-size]
  (let [probe (.createGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_RGB))
        font (mono-font font-size)
        [cw ch] (cell-size probe font)
        w (* cw (:cols ui)) h (* ch (:rows ui))
        img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        g (.createGraphics img)]
    (.setFont g font)
    (paint-all! g ui cw ch w h)
    (.dispose g)
    img))

;; ============================================================
;; 3. ウィンドウ (副作用)
;; ============================================================
(defonce ^:private ui-state (atom nil))

(defn- apply-key!
  "キーを1つ処理して、副作用を実行し、セッション S を更新する"
  [k]
  (swap! ui-state
         (fn [ui]
           (let [ui (tui/handle-key ui k)
                 effs (:effects ui)
                 ui (reduce tui/perform (assoc ui :effects []) effs)]
             (reset! ss/S (:s ui))
             ui))))

(defn- run-effect!
  "副作用を1つ実行する (メニューの保存など、キーを経ない操作)"
  [eff]
  (swap! ui-state (fn [ui] (let [ui (tui/perform (assoc ui :effects []) eff)] (reset! ss/S (:s ui)) ui))))

(defn- choose-file
  "ファイル選択ダイアログ。save? なら保存用。選ばれたパス (取り消しなら nil)"
  [parent save?]
  (let [fc (JFileChooser. (System/getProperty "user.dir"))]
    (.setFileFilter fc (FileNameExtensionFilter. "曲 (.edn .rcp .r36 .g36 .mid)" (into-array String ["edn" "rcp" "r36" "g36" "mid" "midi"])))
    (when (= JFileChooser/APPROVE_OPTION (if save? (.showSaveDialog fc parent) (.showOpenDialog fc parent)))
      (.getPath (.getSelectedFile fc)))))

(defn- open-file! [path]
  (let [song-msg (if (str/ends-with? (str/lower-case path) ".edn")
                   {:song (polysted-edn/load-song path) :warnings []}
                   (ss/song-from-file path))
        s (cond-> (ss/new-session (:song song-msg))
            (str/ends-with? (str/lower-case path) ".edn") (assoc :file path)
            (seq (:warnings song-msg)) (assoc :msg (str/join " / " (:warnings song-msg))))]
    (reset! ss/S s)
    (swap! ui-state (fn [ui] (merge (tb/new-ui s) (select-keys ui [:rows :cols]))))))

(defn- menu-item [^JMenu m label ks f]
  (let [it (JMenuItem. ^String label)]
    (when ks (.setAccelerator it (KeyStroke/getKeyStroke ^String ks)))
    (.addActionListener it (reify ActionListener (actionPerformed [_ _] (f))))
    (.add m it)
    it))

(defn- set-msg! [text]
  (swap! ui-state (fn [ui] (let [ui (assoc-in ui [:s :msg] text)] (reset! ss/S (:s ui)) ui))))

(defn open-visual!
  "ビジュアルエディットのダイアログを開く。on-done は (fn [ve mix?]) (確定のとき)。
   opts: {:modal? false} (テスト用。既定はモーダル)"
  ([frame ve barlen on-done] (open-visual! frame ve barlen on-done {}))
  ([frame ve barlen on-done {:keys [modal? on-preview] :or {modal? true}}]
   (let [state (atom ve) prev (atom nil)
         dlg (JDialog. ^JFrame frame (str "コントローラのビジュアルエディット — " (polysted-visual/kind-name (:vmo ve))) (boolean modal?))
         font (mono-font 13)
         panel (proxy [JPanel] []
                 (paintComponent [^Graphics2D g]
                   (proxy-super paintComponent g)
                   (.setFont g font)
                   (paint-visual! g @state (.getWidth this) (.getHeight this) barlen)))
         close! (fn [] (.dispose dlg))
         handle! (fn [k]
                   (let [[label default] (vis-prompt @state k)
                         n (when label
                             (some-> (JOptionPane/showInputDialog dlg label (str default)) str/trim parse-long))]
                     (when (or (nil? label) n)
                       (swap! state vis-key k n))
                     (.repaint panel)))
         mouse! (fn [^MouseEvent e drag?]
                  (let [geom (vis-geom @state (.getWidth panel) (.getHeight panel))
                        {:keys [gy gh]} geom
                        i (vis-hit geom (.getX e)) y (.getY e)]
                    (cond
                      (and (not drag?) (< y (- gy 4))) (swap! state #(assoc % :ccx (if (:ccx %) nil i)))     ; 上: SUB
                      (and (not drag?) (> y (+ gy gh 4))) (swap! state assoc :cx i)                           ; 下: カーソル
                      :else (do (swap! state vis-draw geom i (-> y (max gy) (min (+ gy gh))) @prev)
                                (reset! prev i)))
                    (.repaint panel)))
         ma (proxy [MouseAdapter] []
              (mousePressed [e] (reset! prev nil) (mouse! e false))
              (mouseDragged [e] (mouse! e true))
              (mouseReleased [_] (reset! prev nil)))]
     (.setPreferredSize panel (Dimension. 1080 440))
     (.setFocusable panel true)
     (.setFocusTraversalKeysEnabled panel false)
     (.addMouseListener panel ma)
     (.addMouseMotionListener panel ma)
     (.addKeyListener panel
                      (proxy [KeyAdapter] []
                        (keyPressed [^KeyEvent e]
                          (let [m (.getModifiersEx e)
                                ctrl? (pos? (bit-and m InputEvent/CTRL_DOWN_MASK))
                                shift? (pos? (bit-and m InputEvent/SHIFT_DOWN_MASK))
                                code (.getKeyCode e)]
                            (cond
                              (= code KeyEvent/VK_ENTER) (do (close!) (on-done @state shift?))
                              (= code KeyEvent/VK_ESCAPE) (close!)
                              :else (when-let [k (pressed->key code ctrl? shift? (pos? (bit-and m InputEvent/ALT_DOWN_MASK)))]
                                      (.consume e) (handle! k)))))
                        (keyTyped [^KeyEvent e]
                          (let [m (.getModifiersEx e)]
                            (when-let [k (typed->key (.getKeyChar e) (pos? (bit-and m InputEvent/CTRL_DOWN_MASK))
                                                     (pos? (bit-and m InputEvent/ALT_DOWN_MASK)))]
                              (if (#{\m \M} k)
                                (when on-preview (on-preview @state (= k \M)))   ; M: 試し再生 (Shift で混ぜて)
                                (handle! k)))))))
     (.add (.getContentPane dlg) panel)
     (.pack dlg)
     (.setLocationRelativeTo dlg frame)
     (.setVisible dlg true)
     (.requestFocusInWindow panel)
     {:dialog dlg :panel panel :state state})))

(defn- visual-ok? "V でビジュアルエディットを始められる状態か" [ui]
  (and (= :edit (:screen ui)) (nil? (:edit ui)) (not= :input (:mode ui)) (not (:chord ui)) (not (:help? ui))))

(defn- start-visual!
  "V: 種類 (EDIT MODE)、新規なら TOTAL ST と COUNT を聞いて、ビジュアルエディットを開く"
  [frame refresh!]
  (let [ui @ui-state s (:s ui)
        ask (fn [label default] (JOptionPane/showInputDialog frame label (str default)))
        done (fn [ve mix?]
               (swap! ui-state (fn [ui] (let [ui (update ui :s ss/visual-commit ve mix?)] (reset! ss/S (:s ui)) ui)))
               (swap! ui-state assoc :mark nil)
               (refresh!))
        preview (fn [ve mix?] (ss/visual-preview! s ve mix?))]
    (when-let [mode (ask "EDIT MODE (P/V/E/M/N/C/K/T、1〜127、U0〜U7、R0〜R127、/記号、G)" "V")]
      (let [b (visual-begin s (:mark ui) mode)]
        (cond
          (:error b) (set-msg! (:error b))
          (:ve b) (open-visual! frame (:ve b) (ss/bar-length s) done {:on-preview preview})
          :else
          (let [nw (:new b)
                kind (polysted-visual/kind-name (:vmo nw))
                st-text (or (some-> (:fixed-st nw) str) (ask (str kind "  TOTAL ST=") (:st-default nw)))
                st (some-> st-text str/trim parse-long)
                co-text (when st-text (ask (str kind "  TOTAL ST=" st-text "  COUNT= (/n で ST÷n、%n で ST×n÷100)")
                                           (if st (min st 768) "")))]
            (when co-text
              (let [r (visual-new s nw st-text co-text)]
                (if (:error r)
                  (set-msg! (:error r))
                  (open-visual! frame (:ve r) (ss/bar-length s) done {:on-preview preview}))))))))
    (refresh!)))

(defn run!
  "GUI を開く。opts: {:exit-on-close? true} (単体起動のとき、閉じたら JVM を終える)"
  ([] (run! {}))
  ([{:keys [exit-on-close?]}]
   (reset! ui-state (assoc (tb/new-ui (dissoc @ss/S :msg)) :rows 30 :cols 100))
   (SwingUtilities/invokeLater
    (fn []
      (let [frame (JFrame. "polysted")
            font  (mono-font 15)
            dims  (atom [9 18])
            panel (proxy [JPanel] []
                    (paintComponent [^Graphics2D g]
                      (proxy-super paintComponent g)
                      (.setFont g font)
                      (let [[cw ch] (cell-size g font)
                            ui (swap! ui-state (fn [ui] (if (identical? (:s ui) @ss/S) ui (assoc ui :s @ss/S))))]
                        (reset! dims [cw ch])
                        (paint-all! g ui cw ch (.getWidth this) (.getHeight this)))))
            refresh! (fn [] (.repaint panel)
                       (let [ui @ui-state]
                         (when (:quit? ui)
                           (.dispose frame)
                           (ss/stop!)
                           (when exit-on-close? (shutdown-agents) (System/exit 0)))))
            key! (fn [k] (apply-key! k) (refresh!))
            roll-edit! (fn [[op args]]
                         (swap! ui-state (fn [ui] (let [ui (update ui :s ss/roll-edit op args)] (reset! ss/S (:s ui)) ui)))
                         (refresh!))
            quit! (fn [] (key! \q) (when-not (:quit? @ui-state)
                                     ;; 未保存の確認 (TUI と同じく、もう一度 q で終了)
                                     (when (= JOptionPane/YES_OPTION
                                              (JOptionPane/showConfirmDialog frame "保存していない変更があります。終了しますか？"
                                                                             "終了" JOptionPane/YES_NO_OPTION))
                                       (key! \q))))
            mb (JMenuBar.)
            mfile (JMenu. "ファイル") medit (JMenu. "編集") mview (JMenu. "表示") mplay (JMenu. "再生") mhelp (JMenu. "ヘルプ")]
        (.setBackground panel bg)
        (.setFocusable panel true)
        (.setFocusTraversalKeysEnabled panel false)          ; Tab をキーとして受け取る
        (.setPreferredSize panel (Dimension. 960 600))
        ;; ---- メニュー ----
        (menu-item mfile "新規" "control shift N" (fn [] (reset! ss/S (ss/new-session)) (swap! ui-state #(merge (tb/new-ui @ss/S) (select-keys % [:rows :cols]))) (refresh!)))
        (menu-item mfile "開く…" "control O" (fn [] (when-let [p (choose-file frame false)]
                                                     (try (open-file! p) (catch Exception e (JOptionPane/showMessageDialog frame (str "開けません: " (.getMessage e)))))
                                                     (refresh!))))
        (menu-item mfile "保存" "control S" (fn [] (when-let [p (or (:file (:s @ui-state)) (choose-file frame true))]
                                                    (run-effect! [:save p]) (refresh!))))
        (menu-item mfile "名前を付けて保存… (.edn .rcp .r36 .mid)" "control shift S"
                   (fn [] (when-let [p (choose-file frame true)] (run-effect! [:save p]) (refresh!))))
        (.addSeparator mfile)
        (menu-item mfile "終了" "control Q" quit!)
        (menu-item medit "元に戻す" "control Z" (fn [] (key! \u)))
        (menu-item medit "やり直し" "control shift Z" (fn [] (key! :ctrl-r)))
        (menu-item medit "直接入力モードの切り替え" nil (fn [] (key! \:)))
        (menu-item medit "コントローラのビジュアルエディット (V)" nil (fn [] (when (visual-ok? @ui-state) (start-visual! frame refresh!))))
        (menu-item medit "ワンタッチ表 ST/GT ASSIGN (})" nil (fn [] (when (visual-ok? @ui-state) (key! \}))))
        (menu-item medit "ユーザーエクスクルーシブの表 USER EXC (Ctrl-U)" nil (fn [] (when (visual-ok? @ui-state) (key! :ctrl-u))))
        (menu-item mview "編集画面 / トラック設定" nil (fn [] (key! :tab)))
        (menu-item mview "リズムパターン編集" nil (fn [] (key! :ctrl-g)))
        (.addSeparator mview)
        (menu-item mview "右側の表示を切り替え (ピアノロール → 楽譜 → グラフ → 両方 → エクスクルーシブ → なし)" "F11" (fn [] (swap! roll-opts cycle-view) (refresh!)))
        (menu-item mview "ピアノロール" nil (fn [] (swap! roll-opts assoc :on? true :view :roll) (refresh!)))
        (menu-item mview "楽譜" nil (fn [] (swap! roll-opts assoc :on? true :view :score) (refresh!)))
        (menu-item mview "コントローラのグラフ" nil (fn [] (swap! roll-opts assoc :on? true :view :graph) (refresh!)))
        (menu-item mview "楽譜とコントローラのグラフ" nil (fn [] (swap! roll-opts assoc :on? true :view :both) (refresh!)))
        (menu-item mview "エクスクルーシブの表示" nil (fn [] (swap! roll-opts assoc :on? true :view :exc) (refresh!)))
        (menu-item mview "横に拡大" "control EQUALS" (fn [] (swap! roll-opts zoom-view 1.25) (refresh!)))
        (menu-item mview "横に縮小" "control MINUS" (fn [] (swap! roll-opts zoom-view 0.8) (refresh!)))
        (let [mg (JMenu. "ピアノロールのグリッド")]
          (doseq [[label div] [["4分音符" 4] ["8分音符" 8] ["16分音符" 16] ["32分音符" 32] ["3連8分" 12] ["なし" 0]]]
            (menu-item mg label nil (fn [] (swap! roll-opts assoc :grid-div div) (refresh!))))
          (.add mview mg))
        (menu-item mplay "再生 / 停止" "F12" (fn [] (key! \p)))
        (menu-item mplay "カーソルの小節から再生" nil (fn [] (key! \P)))
        (menu-item mplay "停止" nil (fn [] (ss/stop!) (refresh!)))
        (menu-item mplay "レコーディング (Ctrl-X rec)" nil (fn [] (swap! ui-state polysted-tui-screens/open-record) (refresh!)))
        (menu-item mhelp "キー操作の一覧" nil (fn [] (key! \?)))
        (doseq [m [mfile medit mview mplay mhelp]] (.add mb m))
        (.setJMenuBar frame mb)
        ;; ---- キーボード ----
        (.addKeyListener panel
                         (proxy [KeyAdapter] []
                           (keyPressed [^KeyEvent e]
                             (let [m (.getModifiersEx e)
                                   ctrl? (pos? (bit-and m InputEvent/CTRL_DOWN_MASK))
                                   k (pressed->key (.getKeyCode e) ctrl?
                                                   (pos? (bit-and m InputEvent/SHIFT_DOWN_MASK))
                                                   (pos? (bit-and m InputEvent/ALT_DOWN_MASK)))]
                               (when (and k (not (and ctrl? (#{:ctrl-o :ctrl-s :ctrl-q :ctrl-z} k))))  ; メニューのショートカット
                                 (.consume e) (key! k))))
                           (keyTyped [^KeyEvent e]
                             (let [m (.getModifiersEx e)]
                               (when-let [k (typed->key (.getKeyChar e)
                                                        (pos? (bit-and m InputEvent/CTRL_DOWN_MASK))
                                                        (pos? (bit-and m InputEvent/ALT_DOWN_MASK)))]
                                 (if (and (= k \V) (visual-ok? @ui-state))
                                   (start-visual! frame refresh!)          ; V: コントローラのビジュアルエディット
                                   (key! k)))))))
        ;; ---- マウス: クリックで行を選ぶ、ホイールでスクロール ----
        (let [ma (proxy [MouseAdapter] []
                   (mousePressed [^MouseEvent e]
                     (.requestFocusInWindow panel)
                     (let [[cw ch] @dims line (quot (.getY e) ch)
                           ui @ui-state
                           mx (.getX e) my (.getY e)
                           free? (and (= :edit (:screen ui)) (nil? (:edit ui)) (not= :input (:mode ui)))
                           in-roll? (and (roll-rect ui cw ch (.getWidth panel)) (>= mx (* cw roll-text-cols)))
                           pt (when in-roll? (roll-point mx my))
                           note (when pt (roll/note-under (cur-roll ui) (:tick pt) (:key pt)))
                           note (when-not (:same? note) note)       ; SAME の音は選ぶだけ
                           idx (cond (and in-roll? (:sgeom @roll-opts)) (score-hit mx)
                                     in-roll? (roll-hit ui mx my)
                                     :else (get (:row-idxs (tui/render-lines ui)) line))]
                       (when free?
                         (cond
                           ;; 右クリック: 音を消す
                           (and note (SwingUtilities/isRightMouseButton e))
                           (roll-edit! [:delete [(:idx note)]])
                           ;; 空き所のダブルクリック: グリッドの升目の頭に音を置く (長さ 1 升、VEL はカーソルの音か 100)
                           (and pt (not note) (= 2 (.getClickCount e)) (SwingUtilities/isLeftMouseButton e))
                           (let [s (:s ui) grid (grid-ticks (or (get-in s [:song :timebase]) 48) (:grid-div @roll-opts))
                                 grid (if (> grid 1) grid (or (get-in s [:song :timebase]) 48))
                                 cur (get (ss/cur-recs s) (:pos s))
                                 vel (if (roll/note-rec? cur) (cur 3) 100)]
                             (roll-edit! [:add [(* grid (quot (:tick pt) grid)) (:key pt) grid vel]]))
                           :else
                           (when idx
                             (swap! ui-state update :s ss/goto-pos idx)
                             (reset! ss/S (:s @ui-state))
                             (when (and note (SwingUtilities/isLeftMouseButton e))
                               (reset! drag {:mode (if (near-end? note mx) :resize :move) :note note
                                             :tick0 (:tick pt) :key0 (:key pt)}))
                             (refresh!))))))
                   (mouseDragged [^MouseEvent e]
                     (when-let [d @drag]
                       (when-let [{:keys [tick key]} (roll-point (.getX e) (.getY e) true)]
                         (let [tb (or (get-in @ui-state [:s :song :timebase]) 48)]
                           (swap! drag assoc :preview (drag-preview d tick key (grid-ticks tb (:grid-div @roll-opts)))))
                         (.repaint panel))))
                   (mouseReleased [^MouseEvent _]
                     (when-let [d @drag]
                       (reset! drag nil)
                       (if-let [op (and (:preview d) (drag-op d (:preview d)))]
                         (roll-edit! op)
                         (refresh!))))
                   (mouseMoved [^MouseEvent e]
                     ;; 音の右端ではカーソルを左右の矢印に
                     (let [ui @ui-state
                           pt (when (:geom @roll-opts) (roll-point (.getX e) (.getY e)))
                           note (when pt (roll/note-under (cur-roll ui) (:tick pt) (:key pt)))]
                       (.setCursor panel (Cursor/getPredefinedCursor
                                          (if (and note (not (:same? note)) (near-end? note (.getX e)))
                                            Cursor/E_RESIZE_CURSOR Cursor/DEFAULT_CURSOR))))))]
          (.addMouseListener panel ma)
          (.addMouseMotionListener panel ma))
        (.addMouseWheelListener panel
                                (proxy [MouseAdapter] []
                                  (mouseWheelMoved [^MouseWheelEvent e]
                                    (let [n (.getWheelRotation e)]
                                      (if (.isControlDown e)        ; Ctrl+ホイール: ピアノロールの横の拡大・縮小
                                        (swap! roll-opts zoom-view (Math/pow 1.25 (- n)))
                                        (dotimes [_ (* 3 (Math/abs n))] (apply-key! (if (pos? n) :down :up))))
                                      (refresh!)))))
        ;; ---- 大きさ: ウィンドウに合わせて行数・桁数を変える ----
        (.addComponentListener panel
                               (proxy [ComponentAdapter] []
                                 (componentResized [_]
                                   (let [[cw ch] @dims]
                                     (swap! ui-state assoc
                                            :rows (max 8 (quot (.getHeight panel) ch))
                                            :cols (max 40 (quot (.getWidth panel) cw)))
                                     (refresh!)))))
        (.setDefaultCloseOperation frame WindowConstants/DO_NOTHING_ON_CLOSE)
        (.addWindowListener frame (proxy [WindowAdapter] [] (windowClosing [_] (quit!))))
        (.add (.getContentPane frame) panel)
        (.pack frame)
        (.setLocationRelativeTo frame nil)
        (.setVisible frame true)
        (.requestFocusInWindow panel))))
   :opened))

;; ============================================================
;; 簡易チェック (純粋部分と画像への描画。画面は開かない)
;; ============================================================
(defn run-checks []
  (let [img (render-image (-> (tb/new-ui (-> (ss/new-session) (ss/insert-notes "C4 E G")))
                              (assoc :rows 12 :cols 60))
                          14)
        rimg (render-image (-> (tb/new-ui (-> (ss/new-session) (ss/insert-notes "C4 E G C5") (ss/goto-pos 1)))
                               (assoc :rows 20 :cols 110))
                           14)
        checks
        [["キー: 矢印・F キー・Shift+F・Ctrl+矢印・Ctrl+英字・Ctrl+6・Alt+数字"
          [(pressed->key KeyEvent/VK_UP false false false) (pressed->key KeyEvent/VK_F5 false false false)
           (pressed->key KeyEvent/VK_F9 false true false) (pressed->key KeyEvent/VK_RIGHT true false false)
           (pressed->key KeyEvent/VK_G true false false) (pressed->key KeyEvent/VK_6 true false false)
           (pressed->key KeyEvent/VK_3 false false true) (pressed->key KeyEvent/VK_A false false false)]
          [:up :f5 :shift-f9 :ctrl-right :ctrl-g :ctrl-caret :f3 nil]]
         ["文字: 印字できる文字だけ (Ctrl・Alt 付きと制御文字は除く)"
          [(typed->key \a false false) (typed->key \a true false) (typed->key (char 8) false false) (typed->key \# false false)]
          [\a nil nil \#]]
         ["画像に描ける (60 桁 × 12 行)"
          [(pos? (.getWidth img)) (pos? (.getHeight img))
           ;; 背景色でない画素がある (文字が描かれている)
           (boolean (some (fn [[x y]] (not= (.getRGB img x y) (.getRGB img 0 (dec (.getHeight img)))))
                          (for [x (range 0 (.getWidth img) 3) y (range 0 (.getHeight img) 3)] [x y])))]
          [true true true]]
         ["ビジュアルエディットのキー: ↑ PgUp ← (端で回る) 数字 Space、反転 (全体)、X Z、多すぎ、波の数"
          (let [ve {:vmo 7 :vmo2 0 :para [0 40 80 120] :step [12 12 12 12] :cx 1 :ccx nil :maxst 1536}
                k (fn [key n] (vis-key ve key n))]
            [(get-in (k :up nil) [:para 1]) (get-in (k :pgup nil) [:para 1]) (:cx (k :left nil))
             (:cx (vis-key (assoc ve :cx 0) :left nil)) (get-in (k \5 nil) [:para 1]) (:ccx (k \space nil))
             ((juxt :para :changed?) (k :shift-f1 nil)) (count (:para (k \x 5))) ((juxt :step :changed?) (k \z 24))
             (:msg (k \z 2000)) (:msg (k :shift-f8 3))])
          [41 50 0 3 70 1 [[127 87 47 7] false] 12 [[6 6 6 6] true] "データが多すぎます。" "波の数は 1〜2 です"]]
         ["ビジュアルエディットの Y: ピッチは 1→2→3→6→1 倍、ほかは 1↔2 倍 (カーソルの値のまわり 64 段)、拡大した範囲の上端"
          (let [pv {:vmo 0 :para [0 4096] :step [12 12] :cx 1}
                ys (take 5 (iterate #(vis-key % \Y nil) pv))
                cv (vis-key {:vmo 7 :para [0 40] :step [12 12] :cx 1} \y nil)]
            [(mapv (juxt :yzm :yr) (rest ys)) ((juxt :yzm :yr) cv) (:yzm (vis-key cv \y nil))
             (vis-v->y 0 (vis-geom (nth ys 1) 1000 440) 4096)])
          [[[2 [-4096 4096]] [3 [-2730 2730]] [6 [-1365 1365]] [1 [-8192 8192]]] [2 [8 72]] 1 64]]
         ["ビジュアルエディットの座標とマウス: x → 点、y ↔ 値、描いた点の間を直線に"
          (let [ve {:vmo 7 :vmo2 0 :para [0 40 80 120] :step [12 12 12 12] :cx 1}
                geom (vis-geom ve 1000 440)]
            [(:xs geom) (vis-hit geom 300) (vis-y->v 7 geom 400) (vis-y->v 7 geom 44) (vis-v->y 7 geom 127)
             ((juxt :para :cx) (vis-draw ve geom 3 44 1))])
          [[56 288 520 752 984] 1 0 127 64 [[0 40 83 127] 3]]]
         ["ビジュアルエディットの開始: 新規 (TOTAL ST の既定)、範囲の読み込み、範囲に無い種類は新規、TOTAL ST と COUNT の検査"
          (let [s (-> (ss/new-session) (ss/insert-recs [[0xEB 0 7 90] [60 48 46 64] [0xEB 24 7 80] [62 48 46 64]]) (ss/goto-pos 0))
                s3 (ss/goto-pos s 3)]
            [(visual-begin s nil "v")
             (select-keys (:ve (visual-begin s3 0 "v")) [:para :step :from :len :ost])
             (visual-begin s3 0 "e")
             (let [v (:ve (visual-new s {:vmo 7 :vmo2 0 :from 0} "48" "/4"))] [(count (:para v)) (first (:para v)) (:step v)])
             (visual-new s {:vmo 7 :vmo2 0 :from 0} "1" "2") (visual-new s {:vmo 7 :vmo2 0 :from 0} "48" "100")])
          [{:new {:vmo 7 :vmo2 0 :from 0 :st-default 48}}
           {:para [90 80] :step [48 72] :from 0 :len 4 :ost 120}
           {:new {:vmo 11 :vmo2 0 :from 0 :fixed-st 120}}
           [12 100 [4 4 4 4 4 4 4 4 4 4 4 4]]
           {:error "TOTAL ST は 2〜1536 です"} {:error "COUNT は 2〜48 です"}]]
         ["F11 の切り替え (ピアノロール → 楽譜 → なし → ピアノロール)、拡大・縮小は今の表示のほう"
          (let [o {:on? true :view :roll :beat-px 32 :score-px 1.6}]
            [(mapv (juxt :on? :view) (take 7 (iterate cycle-view o)))
             (:beat-px (zoom-view o 2)) (:score-px (zoom-view (assoc o :view :score) 2)) (:score-px (zoom-view (assoc o :view :score) 100))])
          [[[true :roll] [true :score] [true :graph] [true :both] [true :exc] [false :exc] [true :roll]] 64 3.2 8.0]]
         ["ピアノロールのドラッグ: 少し動かしただけなら時刻はそのまま、グリッドに合わせて移動・長さ、変化なしは nil"
          (let [n {:idx 5 :tick 108 :len 30 :key 67}
                mv {:mode :move :note n :tick0 110 :key0 67}
                rs {:mode :resize :note n :tick0 138 :key0 67}
                op (fn [d t k] (drag-op d (drag-preview d t k 12)))]
            [(grid-ticks 48 16) (grid-ticks 48 0) (grid-ticks 48 12)
             (op mv 112 69) (op mv 150 67) (op rs 150 67) (op mv 110 67) (op rs 140 67)])
          [12 1 16 [:key [5 69]] [:move [5 144 67]] [:gt [5 48]] nil nil]]
         ["ピアノロール: 右側に描かれ、カーソルの音の色 (橙) がある、クリック位置 → 行"
          (let [{:keys [rect]} (:geom @roll-opts)
                [x y w h] rect]
            [(boolean rect)
             (boolean (some (fn [[px py]] (= (.getRGB rimg px py) (.getRGB (Color. 0xFF 0xB0 0x40))))
                            (for [px (range x (+ x w) 2) py (range y (+ y h) 2)] [px py])))])
          [true true]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted-gui: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(run-checks)
