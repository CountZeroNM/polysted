;; src/polysted/tui_screens.clj — TUI の補助の画面 (純粋): トラック設定・リズムパターン編集・ワンタッチ表・
;; ユーザーエクスクルーシブの表・エクスクルーシブの表示のキー処理と描画
;;
;; src/polysted/tui.clj から分けたもの (2026-11-19)。編集画面と共通のキー (保存・終了・再生など) は、
;; 呼び出し側 (src/polysted/tui.clj の handle-key) が渡す関数 common で処理する (名前空間の循環を避けるため)。
(ns polysted.tui-screens
  (:require [clojure.string :as str]
            [polysted.session :as ss]
            [polysted.edit]
            [polysted.rhythm]
            [polysted.ctrl :as sc]
            [polysted.tui-base :refer [cur-pos cut-width dirty? pad-l pad-r prompt-labels recs
                                run-cmd set-msg]]))

(declare ts-labels ts-field ts-set)
(declare ux-submit ot-col ot-key ux-key ev-key)

;; ---- トラック設定画面 (原作のメイン画面 TRK.SET) ----
;; 行 0 = 曲名、行 1 = テンポ・タイムベース・拍子・調・PLAYBIAS、行 2〜 = トラック
(def ts-song-cols  [:tempo :timebase :beat :key :pbias])
(def ts-track-cols [:tr :mode :ch :st-shift :key-shift :rhy :comment])
(def ts-labels {:title "曲名" :tempo "テンポ" :timebase "タイムベース" :beat "拍子 (例 3/4)" :key "調 (例 G / Am)"
                :pbias "PLAYBIAS" :ch "MIDI CH (A1-B16, 0=OFF)" :st-shift "ST+ (-99〜99)"
                :key-shift "K#+ (-64〜63, OFF)" :comment "トラックメモ"})

(defn ts-cols [_ui row] (case (int row) 0 [:title] 1 ts-song-cols ts-track-cols))
(defn ts-field [ui] (get (ts-cols ui (:ts-row ui)) (:ts-col ui)))
(defn ts-track [ui] (- (:ts-row ui) 2))

(defn ts-value-text
  "入力欄の初期値 (今の値)"
  [ui]
  (let [song (:song (:s ui)) f (ts-field ui)]
    (if (< (:ts-row ui) 2)
      (case f
        :title (:title song "") :tempo (str (:tempo song 120)) :timebase (str (:timebase song 48))
        :beat (let [[a b] (:time-sig song [4 4])] (str a "/" b)) :key (str (:key song 0)) :pbias (str (:pbias song 0)))
      (let [t (get-in song [:tracks (ts-track ui)])]
        (case f
          :ch (sc/ch-str (or (:ch t) 0)) :st-shift (str (sc/st-shift-val (:st-shift t)))
          :key-shift (sc/key-shift-str (:key-shift t)) :comment (:comment t "") "")))))

(defn ts-set [ui text]
  (let [f (ts-field ui)]
    (if (< (:ts-row ui) 2)
      (run-cmd ui :set-song-field [f text])
      (run-cmd ui :set-track-field [(ts-track ui) f text]))))

(defn ts-step
  "+ / - で数値を 1 ずつ変える"
  [ui d]
  (let [f (ts-field ui) song (:song (:s ui))]
    (if (< (:ts-row ui) 2)
      (if-let [v (case f :tempo (:tempo song 120) :timebase (:timebase song 48) :pbias (:pbias song 0) :key (:key song 0) nil)]
        (ts-set ui (str (if (= f :key) (mod (+ v d) 32) (+ v d))))
        ui)
      (let [t (get-in song [:tracks (ts-track ui)])]
        (case f
          :ch (ts-set ui (str (mod (+ (or (:ch t) 0) d) 33)))
          :st-shift (ts-set ui (str (+ (sc/st-shift-val (:st-shift t)) d)))
          :key-shift (let [k (or (:key-shift t) 0) v (cond (>= k 128) 0 (> k 63) (- k 128) :else k)] (ts-set ui (str (+ v d))))
          ui)))))

(defn ts-key
  "トラック設定画面のキー処理。common は編集画面と共通のキー (保存・終了・再生など) の処理"
  [ui key common]
  (let [nt   (count (get-in ui [:s :song :tracks]))
        rows (+ 2 nt)
        mv   (fn [ui dr dc]
               (let [r (-> (+ (:ts-row ui) dr) (max 0) (min (dec rows)))
                     c (-> (+ (:ts-col ui) dc) (max 0) (min (dec (count (ts-cols ui r)))))]
                 (assoc ui :ts-row r :ts-col c)))
        f    (ts-field ui)]
    (case key
      :up (mv ui -1 0) :down (mv ui 1 0) :left (mv ui 0 -1) :right (mv ui 0 1)
      :home (assoc ui :ts-row 0 :ts-col 0) :end (assoc ui :ts-row (dec rows))
      (:tab :esc) (assoc ui :screen :edit :mark nil)
      (:enter \return)
      (cond
        (= f :tr)    (-> ui (update :s #(ss/select-track % (ts-track ui))) (assoc :screen :edit))
        (= f :mode)  (run-cmd ui :set-track-field [(ts-track ui) :mode-next nil])   ; Play → Mute → Mix → Rec
        (= f :rhy)   (run-cmd ui :set-track-field [(ts-track ui) :rhy nil])
        :else (assoc ui :mode :input :kind :ts-edit :buf (ts-value-text ui)))
      (\+ \>) (ts-step ui 1)
      (\- \<) (ts-step ui -1)
      \space (case f (:ch :st-shift :key-shift) (ts-set ui "0") ui)
      \= (case f
           :mode (run-cmd ui :set-track-field [(ts-track ui) :mute? nil])
           :rhy  (run-cmd ui :set-track-field [(ts-track ui) :rhy nil])
           :ch   (let [c (or (get-in ui [:s :song :tracks (ts-track ui) :ch]) 0)]
                   (if (pos? c) (ts-set ui (str (if (<= c 16) (+ c 16) (- c 16)))) ui))
           ui)
      \a (-> (run-cmd ui :add-track [(let [n (inc nt)] (when (<= n 16) n))])
             (assoc :ts-row (+ 2 nt) :ts-col 0))
      ;; 共通のキー (保存・終了・再生・ヘルプ・undo・コマンド欄)
      (if (#{\q \w \p \? \u :ctrl-r :ctrl-x} key) (common ui key) ui))))

;; ---- リズムパターン編集画面 (原作の REDIT.C) ----
(defn rh-pattern [ui] (ss/rhythm-pattern (:s ui) (get-in ui [:rh :meas])))

(defn rh-update [ui f] (run-cmd ui :rhythm-update [(get-in ui [:rh :meas]) f]))

(defn rh-level-step
  "強さを d (+1 / -1) ずつ順に変える (- 1 2 … 9 を繰り返す)"
  [ui d]
  (let [{:keys [cx cy page]} (:rh ui) idx (+ page cy)
        as (ss/rhythm-assign (:s ui))
        v  (or (first (get-in (rh-pattern ui) [:steps cx :hits idx])) 0)
        l  (mod (+ (polysted.rhythm/vel->level v) d) 10)]
    (rh-update ui #(ss/rhythm-set-hit % cx idx (polysted.rhythm/rhy-vel l) (nth (as idx) 2)))))

(defn rh-key
  "リズム画面のキー処理。common は編集画面と共通のキーの処理"
  [ui key common]
  (let [{:keys [cx cy page meas]} (:rh ui)
        p    (rh-pattern ui)
        n    (count (:steps p))
        idx  (+ page cy)
        as   (ss/rhythm-assign (:s ui))
        st?  (= cy 16)
        set-cx (fn [ui c] (assoc-in ui [:rh :cx] (if (pos? n) (mod c n) 0)))
        go-meas (fn [ui m] (assoc ui :rh (assoc (:rh ui) :meas m :cx 0 :x0 0)))
        total (ss/measure-count (:s ui))
        st-add (fn [ui d] (rh-update ui (fn [p] (update-in p [:steps cx :st] #(max 0 (min 255 (+ (or % 0) d)))))))]
    (cond
      (#{:esc :ctrl-g} key)
      (-> ui (assoc :screen :edit)
          (update :s #(ss/goto-pos % (or (polysted.edit/meas-add (ss/cur-recs %) meas) (count (ss/cur-recs %))))))
      (= key :left)  (set-cx ui (dec cx))
      (= key :right) (set-cx ui (inc cx))
      (= key :up)    (assoc-in ui [:rh :cy] (mod (dec cy) 17))
      (= key :down)  (assoc-in ui [:rh :cy] (mod (inc cy) 17))
      (= key :pgdn)  (if (< meas total) (go-meas ui (inc meas)) ui)
      (= key :pgup)  (if (> meas 1) (go-meas ui (dec meas)) ui)
      (#{:home :f1} key) (go-meas ui 1)
      (#{:end :f2} key)  (go-meas ui total)
      ;; 小節の切り取り・複写・貼り付け (原作の F7 / F8 / F9。編集画面と同じ x y Ctrl-V でも)
      (#{:f7 \x} key) (let [u (run-cmd ui :rhythm-cut-measure [meas])]
                        (go-meas u (min meas (ss/measure-count (:s u)))))
      (#{:f8 \y} key) (run-cmd ui :rhythm-copy-measure [meas])
      (#{:f9 :ctrl-v} key) (run-cmd ui :rhythm-paste-measure [meas])
      (= key \j) (assoc ui :mode :input :kind :rh-jump :buf "")
      (= key :tab)   (assoc-in ui [:rh :page] (- 16 page))
      (and (char? key) (Character/isDigit (char key)))
      (if st?
        (assoc ui :mode :input :kind :rh-st :buf (str key))
        (rh-update ui #(ss/rhythm-set-hit % cx idx (polysted.rhythm/rhy-vel (- (int key) (int \0))) (nth (as idx) 2))))
      (#{\space \+ \.} key) (if st? (st-add ui 1) (rh-level-step ui 1))
      (= key \>) (if st? (st-add ui 10) (rh-level-step ui 1))
      (#{\- \,} key) (if st? (st-add ui -1) (rh-level-step ui -1))
      (= key \<) (if st? (st-add ui -10) (rh-level-step ui -1))
      (= key :enter) (assoc ui :mode :input :kind (if st? :rh-st :rh-vel)
                            :buf (str (if st? (get-in p [:steps cx :st] 0) (or (first (get-in p [:steps cx :hits idx])) 0))))
      ;; ステップの削除 (消したステップは覚えておき、Ctrl-L / Insert でカーソル位置に戻す。原作の rlcbuf)
      (#{:delete :ctrl-y} key)
      (if-let [st (get (:steps p) cx)]
        (-> (rh-update ui (fn [p] (update p :steps #(vec (concat (subvec % 0 cx) (subvec % (min (count %) (inc cx))))))))
            (assoc-in [:rh :lbuf] st))
        ui)
      (#{:ctrl-l :insert} key)
      (if-let [st (get-in ui [:rh :lbuf])]
        (rh-update ui (fn [p] (update p :steps #(vec (concat (subvec % 0 (min cx (count %))) [st] (subvec % (min cx (count %))))))))
        (set-msg ui "削除したステップがありません"))
      (#{:ctrl-d :f10} key) (rh-update ui (fn [p] (update p :steps #(if (< cx (count %)) (vec (concat (subvec % 0 (inc cx)) (subvec % cx))) %))))
      (= key \n) (let [u (run-cmd ui :rhythm-new-measure [])] (if-let [m (:rhy-meas (:s u))] (go-meas u m) u))
      (= key \N) (assoc ui :mode :input :kind :rh-step :buf (str (or (:rhy-step (:s ui)) 12)))
      (and (= key \a) (not st?))
      (let [[nm k g] (as idx)] (assoc ui :mode :input :kind :rh-assign :buf (str k " " g " " nm)))
      (= key \m) (-> ui (update :s #(ss/goto-pos % (or (polysted.edit/meas-add (ss/cur-recs %) meas) 0))) (update :effects conj [:play-measure]))
      (= key \P) (-> ui (update :s #(ss/goto-pos % (or (polysted.edit/meas-add (ss/cur-recs %) meas) 0))) (update :effects conj [:play-here]))
      :else (if (#{\q \w \p \? \u :ctrl-r} key) (common ui key) ui))))

(defn rh-submit [ui]
  (let [{:keys [kind buf]} ui
        n (when (re-matches #"\d{1,3}" (str/trim buf)) (Long/parseLong (str/trim buf)))
        ui (assoc ui :mode :normal :kind nil :buf "")
        {:keys [cx cy page]} (:rh ui)
        as (ss/rhythm-assign (:s ui))]
    (cond
      (nil? n) (set-msg ui "数値を入力してください")
      (= kind :rh-st)  (if (<= n 255) (rh-update ui (fn [p] (assoc-in p [:steps cx :st] n))) (set-msg ui "ST は 0〜255 です"))
      (= kind :rh-vel) (if (<= n 127) (rh-update ui #(ss/rhythm-set-hit % cx (+ page cy) n (nth (as (+ page cy)) 2))) (set-msg ui "ベロシティは 0〜127 です"))
      (= kind :rh-jump) (if (<= 1 n (ss/measure-count (:s ui)))
                          (assoc ui :rh (assoc (:rh ui) :meas n :cx 0 :x0 0))
                          (set-msg ui (str "小節は 1〜" (ss/measure-count (:s ui)) " です")))
      (= kind :rh-step) (let [u (run-cmd ui :rhythm-new-measure [n])]
                          (if-let [m (:rhy-meas (:s u))] (assoc u :rh (assoc (:rh u) :meas m :cx 0 :x0 0)) u)))))

;; ---- ワンタッチ表の画面 (原作の } = ST/GT ASSIGN、ASSIGN.C stgt_as) ----
(def ot-cols [:st :gt :vel])
(defn ot-col [ui] (ot-cols (get-in ui [:ot :x])))
(defn ot-val [ui] (get-in (ss/onetouch-table (:s ui)) [(ot-col ui) (get-in ui [:ot :y])]))

(defn ot-step "+ - で値を変える (ST・GT は 8 ビット、VEL は 7 ビットで回す)" [ui d]
  (run-cmd ui :set-onetouch [(ot-col ui) (get-in ui [:ot :y]) (mod (+ (ot-val ui) d) (if (= :vel (ot-col ui)) 128 256))]))

(defn ot-key [ui key]
  (let [{:keys [x y]} (:ot ui)]
    (cond
      (= key :up)    (assoc-in ui [:ot :y] (mod (dec y) 10))
      (= key :down)  (assoc-in ui [:ot :y] (mod (inc y) 10))
      (= key :left)  (assoc-in ui [:ot :x] (mod (dec x) 3))
      (= key :right) (assoc-in ui [:ot :x] (mod (inc x) 3))
      (= key :enter) (assoc ui :mode :input :kind :ot-val :buf (str (ot-val ui)))
      (and (char? key) (Character/isDigit ^char key)) (assoc ui :mode :input :kind :ot-val :buf (str key))
      (#{\+ \.} key) (ot-step ui 1)
      (= key \>)     (ot-step ui 12)
      (#{\- \,} key) (ot-step ui -1)
      (= key \<)     (ot-step ui -12)
      (= key \space) (run-cmd ui :reset-onetouch [(ot-col ui) y])
      (= key :ctrl-l) (-> (run-cmd ui :reset-onetouch []) (set-msg "ワンタッチ表を既定に戻しました"))
      (#{:esc :ctrl-k \}} key) (assoc ui :screen :edit)
      :else ui)))

;; ---- ユーザーエクスクルーシブの画面 (原作の USER EXC、EXCLU.C user_exc_as) ----
;; 行 y: 偶数 = 表 y/2 の名前、奇数 = そのデータ。x = データの位置 (0〜23)
(defn ux-n [ui] (quot (get-in ui [:ux :y]) 2))
(defn ux-op [ui op v] (run-cmd ui :uex-op [(ux-n ui) op (get-in ui [:ux :x]) v]))
(defn ux-next [ui] (update-in ui [:ux :x] #(min 23 (inc %))))

(defn ux-key [ui key]
  (let [{:keys [x y]} (:ux ui) data? (odd? y) n (quot y 2)
        prompt (fn [kind buf] (assoc ui :mode :input :kind kind :buf buf))]
    (cond
      (= key :up)    (assoc-in ui [:ux :y] (mod (dec y) 16))
      (= key :down)  (assoc-in ui [:ux :y] (mod (inc y) 16))
      (= key :left)  (assoc-in ui [:ux :x] (mod (dec x) 24))
      (= key :right) (assoc-in ui [:ux :x] (mod (inc x) 24))
      (#{:esc :ctrl-u} key) (assoc ui :screen :edit)
      (= key :enter) (if data? (prompt :ux-val "") (prompt :ux-name (first ((ss/uex-table (:s ui)) n))))
      (= key \space) (run-cmd ui :uex-reset [n])
      (= key :ctrl-l) (-> (run-cmd ui :uex-reset [nil]) (set-msg "ユーザーエクスクルーシブを全部空にしました (u で戻せます)"))
      (#{\u \U} key) (prompt :ux-copy "")
      (= key :f9)        (prompt :ux-load "user.rex")
      (= key :shift-f9)  (prompt :ux-load-res (str "user" n ".res"))
      (= key :f10)       (prompt :ux-save "user.rex")
      (= key :shift-f10) (prompt :ux-save-res (str "user" n ".res"))
      (not data?) ui
      (and (char? key) (re-matches #"[0-9a-fA-F]" (str key))) (prompt :ux-val-over (str/upper-case (str key)))
      (#{\+ \.} key) (ux-op ui :step 1)
      (= key \>)     (ux-op ui :step 16)
      (#{\- \,} key) (ux-op ui :step -1)
      (= key \<)     (ux-op ui :step -16)
      (#{:delete :ctrl-g} key) (ux-op ui :del nil)
      (= key :backspace) (if (pos? x) (-> ui (assoc-in [:ux :x] (dec x)) (ux-op :del nil)) ui)
      (#{:f1 :f2 :f3 :f4 :f5} key) (-> (ux-op ui :ins (+ 0x80 (dec (Long/parseLong (subs (name key) 1))))) ux-next)
      (= key :f6)  (ux-op ui :end nil)
      (= key :f8)  (if (> x 5) (ux-op ui :sum nil) (set-msg ui "チェックサムは 7 バイト目より後に入れます"))
      (= key :tab) (ux-op ui :dup nil)
      :else ui)))

(defn ev-key
  "エクスクルーシブの表示の画面: ↑↓ で前後のエクスクルーシブの行へ、Esc・E で戻る"
  [ui key]
  (let [rs (recs ui) p (cur-pos ui)
        exc? (fn [i] (when-let [r (get rs i)] (<= 0x90 (r 0) 0x98)))
        find (fn [is] (first (filter exc? is)))]
    (cond
      (= key :up)   (if-let [i (find (range (dec p) -1 -1))] (update ui :s ss/goto-pos i) (set-msg ui "前にエクスクルーシブはありません"))
      (= key :down) (if-let [i (find (range (inc p) (count rs)))] (update ui :s ss/goto-pos i) (set-msg ui "後ろにエクスクルーシブはありません"))
      (#{:esc \E \q :enter} key) (assoc ui :screen :edit)
      :else ui)))

(defn ux-submit
  "ユーザーエクスクルーシブの画面の入力欄の確定"
  [ui kind buf]
  (let [n (ux-n ui) x (get-in ui [:ux :x])
        num (fn [] (parse-long (str/trim buf)))
        byte1 (fn [] (let [bs (sc/parse-exc-data buf)] (when (and bs (not= 0xF7 (first bs))) (first bs))))]
    (case kind
      :ux-name (run-cmd ui :uex-name [n buf])
      (:ux-val :ux-val-over)
      (if-let [v (byte1)]
        (let [d (second ((ss/uex-table (:s ui)) n))
              op (if (or (= kind :ux-val) (= 0xF7 (d x))) :ins :set)]
          (-> (ux-op ui op v) ux-next))
        (set-msg ui "値は 16進 00〜7F か gt ve ch cs ss です"))
      :ux-copy (if-let [v (num)] (run-cmd ui :uex-copy [n v]) (set-msg ui "番号は 0〜7 です"))
      :ux-load     (update ui :effects conj [:rex-load [(str/trim buf) nil]])
      :ux-load-res (update ui :effects conj [:rex-load [(str/trim buf) n]])
      :ux-save     (update ui :effects conj [:rex-save [(str/trim buf) nil]])
      :ux-save-res (update ui :effects conj [:rex-save [(str/trim buf) n]]))))

(defn render-trkset
  "トラック設定画面の行と強調する位置"
  [ui]
  (let [{:keys [rows cols s]} ui
        song (:song s)
        cut #(cut-width % cols)
        seg-line (fn [segs] ; segs: [[文字列 欄番号または nil] ...] → [行 {欄番号 [開始 終了]}]
                   (reduce (fn [[l spans] [txt c]]
                             [(str l txt) (if c (assoc spans c [(count l) (+ (count l) (count txt))]) spans)])
                           ["" {}] segs))
        [l0 sp0] (seg-line [[" TITLE  " nil] [(pad-r (:title song "") 64) 0]])
        [l1 sp1] (seg-line [[" TEMPO " nil] [(pad-l (:tempo song 120) 3) 0] ["  TIMEBASE " nil] [(pad-l (:timebase song 48) 4) 1]
                            ["  BEAT " nil] [(let [[a b] (:time-sig song [4 4])] (pad-r (str a "/" b) 5)) 2]
                            ["  KEY " nil] [(pad-r (sc/key-str (:key song 0)) 16) 3] ["  PLAYBIAS " nil] [(pad-l (:pbias song 0) 3) 4]])
        thead " TR  MODE CH.  ST+ K#+  RHY  TRACK MEMO                            STEP TOTAL ST"
        trs  (vec (get-in song [:tracks]))
        body-h (max 1 (- rows 7))
        top  (max 0 (min (- (:ts-row ui) 2 (quot body-h 2)) (- (count trs) body-h)))
        trows (for [i (range top (min (count trs) (+ top body-h)))]
                (let [t (trs i) {:keys [steps total-st]} (ss/track-stats t)
                      cur? (= i (:tr s))]
                  (seg-line [[(if cur? ">" " ") nil] [(pad-l (inc i) 2) 0] ["  " nil] [(pad-r (sc/mode-str t) 4) 1] [" " nil]
                             [(pad-r (sc/ch-str (or (:ch t) 0)) 3) 2] ["  " nil] [(pad-l (sc/st-shift-val (:st-shift t)) 3) 3] [" " nil]
                             [(pad-l (sc/key-shift-str (:key-shift t)) 3) 4] ["  " nil]
                             [(pad-r (sc/rhy-str (or (:rhy-byte t) (if (:rhythm? t) 0x80 0))) 3) 5] ["  " nil]
                             [(pad-r (:comment t "") 36) 6] [(pad-l steps 6) nil] [(pad-l total-st 9) nil]])))
        lines (vec (concat [(str "TRACK SET  " (if (:file s) (:file s) "") (if (dirty? ui) " *" ""))
                            l0 l1 "" thead]
                           (map first trows)
                           (repeat (- body-h (count trows)) "")
                           [(cond (= :input (:mode ui)) (str (ts-labels (ts-field ui)) "> " (:buf ui) "_")
                                  (:msg s) (str "! " (:msg s))
                                  :else "")
                            "Tab 編集画面  ↑↓←→ 移動  Enter 設定 (TR で編集へ)  + - 増減  Space OFF  = 反転  a 追加  q 終了"]))
        r (:ts-row ui)
        [line span] (cond (= r 0) [1 (sp0 (:ts-col ui))]
                          (= r 1) [2 (sp1 (:ts-col ui))]
                          :else (let [k (- r 2 top)]
                                  (when (< -1 k (count trows)) [(+ 5 k) ((second (nth trows k)) (:ts-col ui))])))]
    {:lines (mapv cut lines) :cursor-line line :cursor-span span :range-lines #{}}))

(defn render-rhythm
  "リズムパターン編集画面の行と強調する位置"
  [ui]
  (let [{:keys [rows cols s]} ui
        {:keys [meas cx cy page]} (:rh ui)
        p     (ss/rhythm-pattern s meas)
        steps (:steps p)
        n     (count steps)
        as    (ss/rhythm-assign s)
        t     (ss/cur-track s)
        vis   (max 1 (quot (- cols 14) 3))
        x0    (max 0 (min (- cx (quot vis 2)) (- n vis)))
        xs    (range x0 (min n (+ x0 vis)))
        cell  (fn [txt] (pad-l txt 3))
        head  (str "RHYTHM  TR" (format "%02d" (inc (:tr s))) " ch=" (or (:ch t) "OFF") "  MEAS " meas "/" (ss/measure-count s)
                   "  STEP ST " (or (:rhy-step s) 12) "  楽器 " (if (zero? page) "1-16" "17-32")
                   (if (dirty? ui) " *" ""))
        nums  (str (pad-r "" 13) (apply str (map #(cell (inc %)) xs)))
        inst  (for [r (range 16) :let [i (+ page r)]]
                (str (pad-l (inc i) 2) " " (pad-r (subs (str (first (as i)) "            ") 0 10) 10)
                     (apply str (map (fn [x] (let [st (steps x)]
                                               (cell (cond (= :raw (:kind st)) "*"
                                                           :else (let [v (first (get-in st [:hits i]))]
                                                                   (if (and v (pos? v)) (str (polysted.rhythm/vel->level v)) "-"))))))
                                     xs))))
        strow (str (pad-r " ST" 13) (apply str (map #(cell (:st (steps %))) xs)))
        info  (cond
                (:same? p) "=== SAME MEAS (パターン編集できません) ==="
                (:too-many? p) "この小節はステップが多すぎます"
                (= :input (:mode ui)) (str ({:rh-st "ST" :rh-vel "ベロシティ (0-127)" :rh-step "ステップの ST (6-96, 192)" :rh-jump "移動する小節"
                                             :rh-assign "割り当て キー GT 名前"} (:kind ui)) "> " (:buf ui) "_")
                (:msg s) (str "! " (:msg s))
                (zero? n) ""
                (= cy 16) (str "ステップ " (inc cx) "  ST " (:st (steps cx)))
                :else (let [i (+ page cy) [v g] (get-in steps [cx :hits i])]
                        (str (first (as i)) " (キー " (second (as i)) ")  ステップ " (inc cx)
                             "  VEL " (or v 0) "  GT " (or g (nth (as i) 2)))))
        helps ["0-9 強さ  Space + - 変える  Enter 値  Tab 楽器  a 割り当て  n 新小節  m 再生"
               "PgUp PgDn F1 F2 j 小節の移動   x y ^V 小節の切取・複写・前に貼付 (F7 F8 F9)"
               "Del ステップの削除  ^L/Ins 削除したステップを戻す  ^D 二重化  Esc 戻る"]
        lines (vec (concat [head nums] inst [strow info] helps))
        cut   #(cut-width % cols)
        ci    (.indexOf ^java.util.List (vec xs) cx)
        span  (when (and (>= ci 0) (not (:same? p))) (let [a (+ 13 (* 3 ci))] [a (+ a 3)]))]
    {:lines (mapv cut (take rows lines))
     :cursor-line (when span (+ 2 cy))
     :cursor-span span
     :range-lines #{}}))

(defn screen-status
  "状態行: 入力中なら入力欄、メッセージがあればそれ"
  [ui]
  (cond (= :input (:mode ui)) (str (prompt-labels (:kind ui)) "> " (:buf ui) "_")
        (:msg (:s ui)) (str "! " (:msg (:s ui)))
        :else ""))

(defn render-onetouch
  "ワンタッチ表の画面 (原作の ST/GT ASSIGN)"
  [ui]
  (let [{:keys [rows cols s]} ui {:keys [x y]} (:ot ui)
        t (ss/onetouch-table s)
        lines (vec (concat ["ST/GT ASSIGN  ワンタッチ入力の表 (直接入力の F1〜F10 で使う。曲には保存しない)" ""
                            "  NO.     ST   GT  VEL"]
                           (for [i (range 10)]
                             (str "  F" (pad-l (inc i) 2) ":  " (pad-l ((:st t) i) 4) " " (pad-l ((:gt t) i) 4) " " (pad-l ((:vel t) i) 4)))
                           (repeat (max 0 (- rows 15)) "")
                           [(screen-status ui)
                            "↑↓←→ 移動  Enter 数字 入力  + - ±1  > < ±12  Space 既定値  Ctrl-L 全部  Esc 戻る"]))
        a (+ 8 (* 5 x))]
    {:lines (mapv #(cut-width % cols) (take rows lines)) :cursor-line (+ 3 y) :cursor-span [a (+ a 4)] :range-lines #{}}))

(defn render-excview
  "エクスクルーシブの表示の画面 (原作の Exclusive View)"
  [ui]
  (let [{:keys [rows cols s]} ui
        v (sc/exc-view (ss/cur-recs s) (:pos s) (ss/uex-table s))
        body (if v (sc/exc-view-lines v true) [" (エクスクルーシブの行ではありません)"])
        lines (vec (concat [(str "EXCLUSIVE VIEW  TR" (format "%02d" (inc (:tr s))) "  行 " (inc (:pos s)))]
                           (take (- rows 3) body)
                           (repeat (max 0 (- rows 3 (count body))) "")
                           [(screen-status ui) "↑↓ 前後のエクスクルーシブの行  Esc 戻る"]))]
    {:lines (mapv #(cut-width % cols) (take rows lines)) :cursor-line nil :cursor-span nil :range-lines #{}}))

(defn render-uexc
  "ユーザーエクスクルーシブの画面 (原作の USER DEFINE EXCLUSIVE)"
  [ui]
  (let [{:keys [rows cols s]} ui {:keys [x y]} (:ux ui)
        ue (ss/uex-table s) n (quot y 2)
        vis (max 1 (quot (- rows 6) 2))
        top (-> (- n (quot vis 2)) (max 0) (min (- 8 vis)) (max 0))
        body (mapcat (fn [i] (let [[nm d] (ue i)]
                               [(str " " i "  MEMO: " nm)
                                (str "     F0 " (str/join " " (sc/uex-cells d)))]))
                     (range top (min 8 (+ top vis))))
        d (second (ue n)) v (d x)
        info (if (odd? y)
               (str "番号 " n "  位置 " (inc x) "  値 " (if (< v 128) (str v " (" (format "%02XH" v) ")") "---"))
               (str "番号 " n "  名前 (Enter で入力)"))
        lines (vec (concat [(str "USER DEFINE EXCLUSIVE  ユーザーエクスクルーシブの表 (/ の U0〜U7 で使う)" (if (dirty? ui) " *" ""))]
                           body
                           (repeat (max 0 (- rows 5 (count body))) "")
                           [info (screen-status ui)
                            "↑↓←→ 移動  Enter 入力  0-F 上書き  + - > < 増減  Del BS 削除  Tab 複写  U 写す"
                            "F1-5 gt ve ch cs ss  F6 終わり  F8 チェックサム  Space 空  F9 F10 .REX  Esc 戻る"]))
        line (+ 1 (* 2 (- n top)) (if (odd? y) 1 0))
        span (if (odd? y) (let [a (+ 8 (* 3 x))] [a (+ a 2)]) [10 (+ 10 (max 1 (count (first (ue n)))))])]
    {:lines (mapv #(cut-width % cols) (take rows lines)) :cursor-line line :cursor-span span :range-lines #{}}))

;; ---- レコーディングの画面 (原作 RECORD.C real_record の REALTIME RECORDING) ----
(defn open-record "レコーディングの画面を開く (録音チャンネルの既定は今のトラックのチャンネル)" [ui]
  (assoc ui :screen :record :mark nil
         :rc (or (:rc ui) {:ch (inc (bit-and (dec (or (:ch (ss/cur-track (:s ui))) 1)) 15)) :thru? true :metronome? true :top 1 :btm 999})))

(defn rc-key
  "レコーディングの画面のキー (原作の C 録音チャンネル、O 入力をそのまま出す、M メトロノーム、S 録音する小節、
   Enter・Space で開始、録音中は Esc・Enter・Space で止めて取り込む)"
  [ui key]
  (if (ss/recording?)
    (if (#{:esc :enter \space} key) (update ui :effects conj [:record-stop]) ui)
    (case key
      (\c \C) (assoc ui :mode :input :kind :rc-ch :buf (str (get-in ui [:rc :ch])))
      (\o \O) (update-in ui [:rc :thru?] not)
      (\m \M) (update-in ui [:rc :metronome?] not)
      (\s \S) (assoc ui :mode :input :kind :rc-meas :buf (str (get-in ui [:rc :top]) " " (get-in ui [:rc :btm])))
      (:enter \space) (update ui :effects conj [:record-start (:rc ui)])
      :esc (assoc ui :screen :edit)
      ui)))

(defn rc-submit "レコーディングの画面の入力欄の確定" [ui]
  (let [{:keys [kind buf]} ui
        ns (mapv #(Long/parseLong %) (re-seq #"\d+" buf))
        ui (assoc ui :mode :normal :kind nil :buf "")]
    (case kind
      :rc-ch (let [[c] ns] (if (and c (<= 1 c 16)) (assoc-in ui [:rc :ch] c) (set-msg ui "録音チャンネルは 1〜16 です")))
      :rc-meas (let [[a b] ns]
                 (if (and a b (<= 1 a b 999)) (update ui :rc assoc :top a :btm b) (set-msg ui "録音する小節は「開始 終わり」(1〜999) で入力してください"))))))

(defn render-record
  "レコーディングの画面"
  [ui]
  (let [{:keys [rows cols s]} ui
        {:keys [ch thru? metronome? top btm]} (:rc ui)
        song (:song s)
        trs (ss/rec-tracks s)
        ins (ss/midi-inputs)
        rec? (ss/recording?)
        lines (vec (concat
                    [" REALTIME RECORDING" ""
                     (str " RECORDING CH.: " ch "   OUT MODE: " (if thru? "THRU" "OFF") "   METORO: " (if metronome? "ON" "OFF"))
                     (str " RECORD MEAS: " top " - " btm "   TEMPO: " (:tempo song 120) "   BEAT: " (let [[a b] (:time-sig song [4 4])] (str a "/" b)))
                     (str " MIDI IN: " (if (seq ins) (:name (first ins)) "(見つかりません)"))
                     (str " 録音するトラック: " (if (seq trs)
                                                 (str/join " " (map #(str "TR" (inc %) "(" (sc/mode-str (get-in song [:tracks %])) ")") trs))
                                                 "なし (Tab のトラック設定で MODE を Mix か Rec に)"))
                     (if (> (count trs) 1) "   (トラックが複数のときは、それぞれのトラックのチャンネルの入力を入れます)" "")
                     ""
                     (cond rec? " ● 録音中   Esc・Enter・Space で止めて取り込みます"
                           :else (str " 待機中   Enter・Space で開始" (if (= 1 top) " (2 小節のカウントの後から)" "")))]
                    (repeat (max 0 (- rows 11)) "")
                    [(screen-status ui)
                     "C 録音チャンネル  O 入力をそのまま出す  M メトロノーム  S 録音する小節  Enter 開始  Esc 戻る"]))]
    {:lines (mapv #(cut-width % cols) (take rows lines)) :cursor-line nil :cursor-span nil :range-lines #{}}))
