;; src/polysted/tui.clj — TUI 層 (ターミナル上の全画面エディタ)
;;
;; 構成 (md 3.5 の方針どおり、コアには手を入れない):
;;   1. 純粋: decode-keys (端末の入力バイト → キー) / handle-key (ui, キー → ui)
;;            render-lines (ui → 画面の行)。キー割り当ては normal-keys / direct-keys の表(データ)。
;;   2. 副作用: run! — stty で raw モードにし、ANSI エスケープで描画するループ。
;;      セッションは REPL と同じ polysted.session/S を使うので、REPL と行き来できる。
;; 依存ライブラリなし。Linux/macOS のターミナル (stty と /dev/tty) が必要。
;;
;; モード:
;;   通常モード   1キーのコマンド (n 音名入力, m MML, x 削除 ...)
;;   直接入力モード (: で切り替え) STED2 のエディット画面の入力 (EDIT.C の ret() と数値上書き)
;;     Enter で行を挿入し K# → ST → GT → VEL の順に入力。A〜G・0〜9 で既存の欄を上書き。
;;
;; 起動:
;;   REPL から: (s/tui)
;; 分割: 入力の解読・UI の状態・共通の関数は src/polysted/tui_base.clj、補助の画面は src/polysted/tui_screens.clj
;;   単体:      clojure -M:tui [song.edn | song.rcp]   (-main)
(ns polysted.tui
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [polysted.session :as ss]
            [polysted.edit]
            [polysted.rhythm]
            [polysted.ctrl :as sc]
            [polysted.notename]
            [polysted.mml]
            [polysted.rcp]
            [polysted.edn]
            ;; 土台と補助の画面は別のファイル (2026-11-19 に分割)
            [polysted.tui-base :refer [body-height cur-pos cut-width decode-keys dirty? new-ui pad-l pad-r
                                page prompt-labels recs run-cmd set-msg]]
            [polysted.tui-screens :refer [ev-key ot-col ot-key render-excview render-onetouch render-rhythm render-trkset
                                render-uexc rh-key rh-submit ts-field ts-key ts-labels ts-set ux-key
                                ux-submit open-record rc-key rc-submit render-record]]))

;; ============================================================
;; 1. 純粋部分
;; ============================================================

;; 通常モードのキー割り当て。値は
;;   [:cmd コマンド名 引数...]  → polysted.session/dispatch に渡す
;;   [:ui 動作 ...]             → TUI 自身の動作
;; TUI/GUI で共通に使えるよう、データとして持つ。
(def normal-keys
  {:up     [:cmd :move -1]   \k [:cmd :move -1]
   :down   [:cmd :move 1]    \j [:cmd :move 1]
   :pgup   [:ui :page -1]    :pgdn [:ui :page 1]
   :home   [:cmd :goto 0]    \g [:cmd :goto 0]
   :end    [:cmd :goto Integer/MAX_VALUE] \G [:cmd :goto Integer/MAX_VALUE]
   :left   [:ui :track -1]   :right [:ui :track 1]
   :ctrl-left [:ui :track -1] :ctrl-right [:ui :track 1]
   :delete [:ui :cut-or-delete] \x [:ui :cut-or-delete]
   ;; 範囲・コピー・貼り付け (原作の F6〜F10。直接入力モードでは F キーはワンタッチなので文字キーを使う)
   :f6     [:ui :mark]       \v [:ui :mark]
   :f7     [:ui :cut-or-delete]
   :f8     [:ui :copy]       \y [:ui :copy]
   :f9     [:ui :paste]      :ctrl-v [:ui :paste]
   :shift-f9 [:ui :paste :rev]                           ; 反転貼付 (Ctrl-X rev でも)
   \r      [:ui :prompt :replicate]                        ; レプリケート (原作の R)
   \R      [:ui :prompt :replicate-all]                    ; 全トラックのレプリケート (原作の Shift+R)
   \t      [:ui :prompt :replicate-range]                  ; 範囲を指定トラック・小節へ (原作の T)
   \T      [:ui :prompt :replicate-range-all]              ; 範囲を全トラックの小節へ (原作の Shift+T)
   :shift-f8 [:ui :paste :mix] \M [:ui :paste :mix]     ; ミックス貼付
   :f10    [:cmd :duplicate] :ctrl-d [:cmd :duplicate]
   \u      [:cmd :undo]      :ctrl-r [:cmd :redo]
   \o      [:cmd :optimize]
   \n      [:ui :prompt :notes] :enter [:ui :prompt :notes]
   \m      [:ui :prompt :mml]
   \w      [:ui :prompt :save]
   \p      [:ui :effect [:toggle-play]] \space [:ui :effect [:toggle-play]]
   \P      [:ui :effect [:play-here]]                      ; カーソルのある小節から演奏 (原作のポイントプレイ)
   \:      [:ui :direct]
   :tab    [:ui :screen]                                   ; トラック設定画面へ
   \*      [:cmd :insert-measure]
   \[      [:cmd :insert-repeat-start]
   \]      [:ui :insert-cmd :repeat-end]
   \^      [:ui :insert-cmd :same]
   \/      [:ui :insert-ctrl]
   \_      [:ui :exclusive]                                ; トラックエクスクルーシブ (原作の _)
   \X      [:ui :chord]                                    ; ワンタッチコード入力 (原作の X)
   \Q      [:ui :prompt :quantize]                        ; クオンタイズ (原作の Q)
   :shift-f1 [:ui :prompt :note-ctrl] :ctrl-e [:ui :prompt :note-ctrl]   ; ノート操作 (原作の Shift+F1)
   :f3 [:ui :prompt :replace]  :ctrl-t [:ui :prompt :replace]            ; 置換
   :ctrl-caret [:ui :same-extract]                                     ; SAME の展開 (原作の Ctrl+^)
   :ctrl-x [:ui :prompt :command]                                      ; コマンド欄 (same / extract ...)
   :ctrl-g [:ui :rhythm]                                               ; リズムパターン編集 (原作の [記号])
   :shift-f3 [:ui :prompt :delete-match] :ctrl-k [:ui :prompt :delete-match] ; 指定削除
   :f4 [:ui :prompt :search]   :ctrl-f [:ui :prompt :search]             ; 検索の指定 (と前へ検索)
   :f5 [:cmd :find-next true]  :ctrl-n [:cmd :find-next true]            ; 次を検索
   :shift-f5 [:cmd :find-next false] :ctrl-p [:cmd :find-next false]     ; 前を検索
   :backspace [:ui :bs-delete] :ctrl-y [:cmd :delete 1]   ; 原作の BS (上の行を削除) と Ctrl-Y
   :ctrl-l [:cmd :restore-line] :insert [:cmd :restore-line] ; 削除行の復活 (原作の Ctrl-L / UNDO)
   \}      [:ui :onetouch]                                 ; ワンタッチ表 (原作の } = ST/GT ASSIGN)
   :ctrl-u [:ui :uexc]                                     ; ユーザーエクスクルーシブの表 (原作の ? = USER EXC)
   \E      [:ui :excview]                                  ; エクスクルーシブの表示 (原作の Exclusive View)
   \?      [:ui :help]
   \q      [:ui :quit]})

;; 直接入力モードで通常モードより優先するキー。
;; A〜G・0〜9 は direct-char が扱う (g/G の先頭・末尾移動は Home/End のみになる)。
;; ここに無いキー (x u o n m w p q ? Ctrl-R など) は normal-keys がそのまま効く。
(def direct-keys
  {:left  [:ui :cx :left]  :right [:ui :cx :right]  \space [:ui :cx :right]
   :enter [:ui :insert-line]
   ;; 値の増減と反転 (原作の + . > / - , < / =)
   \+ [:ui :val :inc] \. [:ui :val :inc] \> [:ui :val :inc-big]
   \- [:ui :val :dec] \, [:ui :val :dec] \< [:ui :val :dec-big] \= [:ui :val :rev]
   \:     [:ui :direct]    :esc [:ui :direct]})

(def field-names ["K#" "ST" "GT" "VEL"])
(def kind-labels {:repeat-end "回数" :same "小節番号" :ctrl-sym "コントローラ"})
(def kind-texts  {:repeat-end "] REPEAT " :same "======== SAME MEAS " :ctrl-sym "/ "})
(def ctrl-sym-help
  "V 音量 E 表情 H ホールド D モジュ N パン P ベンド G 音色 T テンポ M CH C アフター 数字 CC番号")

(defn- edit-label [edit]
  (cond
    (= :ctrl (:kind edit)) (sc/ctrl-field-labels (:ftype edit))
    (kind-labels (:kind edit)) (kind-labels (:kind edit))
    :else (field-names (:cx edit))))

;; ---- 直接入力: 欄の値 ----
(defn- key-ctx
  "K# 欄の音名を読む文脈。オクターブ省略時の基準は、その行のキー (ctc の da)。
   その行が休符ならカーソル直前の発音ノート (なければ 60)【設計判断】"
  [s row]
  (let [rs (ss/cur-recs s)
        r  (get rs row)
        lk (if (and r (< (r 0) 0x80) (pos? (r 2)) (pos? (r 3)))
             (r 0)
             (or (ss/last-note-key rs row) 60))]
    {:last-key lk
     :key-sig (ss/key-at s row)}))

(defn parse-field
  "欄の入力文字列 → 値。空なら :keep、不正なら nil。
   K#: 音名かキー番号 (polysted.notename)。ST/GT: 0〜255。VEL: 0 以上 (127 で頭打ち、原作どおり)"
  [s row cx text]
  (let [t (str/trim text)]
    (cond
      (str/blank? t) :keep
      (zero? cx)     (polysted.notename/parse-token t (key-ctx s row))
      :else          (when (re-matches #"\d{1,3}" t)
                       (let [v (Long/parseLong t)]
                         (cond (= cx 3)   (min 127 v)
                               (<= v 255) v))))))

(defn- parse-int [text]
  (let [t (str/trim text)] (when (re-matches #"\d{1,4}" t) (Long/parseLong t))))

(defn- edit-result
  "入力中の欄の値を確定したレコード => [:ok レコード] / [:err メッセージ]"
  [ui]
  (let [{:keys [row cx buf default kind]} (:edit ui)
        rs  (recs ui)
        rec (get rs row)]
    (case (or kind :note)
      :note       (if-let [v (parse-field (:s ui) row cx buf)]
                    [:ok (assoc rec cx (if (= v :keep) default v))]
                    [:err (str (field-names cx) " の値が不正です: " buf)])
      :repeat-end (let [n (parse-int buf)]
                    (if-let [err (ss/repeat-count-error n)]
                      [:err err]
                      [:ok (assoc rec 1 n)]))
      :same       (let [j (parse-int buf)]
                    (if-let [err (ss/same-error rs row j)]
                      [:err err]
                      [:ok (ss/set-same-ref rec j)]))
      :ctrl-sym   (if-let [sc (sc/spc-code buf)]
                    (if (= 0x98 (:code sc))
                      [:exclusive nil]
                    (if (sc/supported-ctrls (:code sc))
                      [:ok (ss/ctrl-init-rec (:s ui) row sc)]
                      [:err (str "未対応のコントローラです: " buf)]))
                    [:err (str "コントローラの記号が不正です: " buf "   (" ctrl-sym-help ")")])
      :ctrl       (if-let [r (sc/set-ctrl-field rec cx (:ftype (:edit ui)) buf)]
                    [:ok r]
                    [:err (str (sc/ctrl-field-labels (:ftype (:edit ui))) " の値が不正です: " buf)]))))

(defn- start-insert
  "カーソル行に1行挿入して K# の入力を始める (EDIT.C ret())。
   初期値は上にある一番近いノート (休符を含む) のコピー、なければ 60 48 46 64。"
  [ui first-ch]
  (let [s0   (:s ui)
        rs   (ss/cur-recs s0)
        p    (:pos s0)
        prev (some #(when (< (% 0) 0x80) %) (rseq (subvec rs 0 p)))
        init (or prev [60 48 46 64])]
    (assoc ui
           :s (-> s0 (ss/insert-line init) (ss/move -1))  ; SAME の前なら参照もずらす (line_ins_aft)
           :cx 0
           :edit {:type :insert :row p :cx 0 :buf (if first-ch (str first-ch) "")
                  :default (init 0) :snap s0 :filled #{}})))

(defn- start-over
  "既存の行の欄 cx を、打った文字 ch から上書き入力する。kind: :note / :repeat-end / :same"
  ([ui cx ch] (start-over ui cx ch :note))
  ([ui cx ch kind]
   (let [p (cur-pos ui)]
     (assoc ui :cx cx
            :edit {:type :over :kind kind :row p :cx cx :buf (str ch)
                   :default ((get (recs ui) p) cx) :snap (:s ui)}))))

(defn- start-ctrl-insert
  "コントローラの行を挿入し、記号の入力を始める (原作の '/'、EDIT.C spcon)"
  [ui]
  (let [s0 (:s ui)
        p  (:pos s0)]
    (assoc ui
           :s (-> s0 (ss/insert-line [0 0 0 0]) (ss/move -1))
           :cx 0
           :edit {:type :insert :kind :ctrl-sym :row p :cx 0 :buf "" :default nil
                  :snap s0 :filled #{}})))

(defn- to-ctrl-field
  "コントローラの挿入中に fi 番目の欄へ"
  [ui fi]
  (let [[cx t] (get-in ui [:edit :fields fi])]
    (-> ui (assoc :cx cx)
        (update :edit assoc :kind :ctrl :fi fi :cx cx :ftype t :buf ""))))

(defn- start-cmd-insert
  "リピート終了 ']' / SAME '^' の行を挿入し、回数・小節番号の入力を始める"
  [ui kind]
  (let [s0 (:s ui)
        p  (:pos s0)]
    (assoc ui
           :s (-> s0 (ss/insert-line ({:repeat-end [0xF8 0 0 0] :same [0xFC 0 0 0]} kind))
                  (ss/move -1))
           :cx 1
           :edit {:type :insert :kind kind :row p :cx 1 :buf "" :default nil
                  :snap s0 :filled #{}})))

(defn- to-field
  "挿入中の行で欄 cx に移る。default を省略すると今の値"
  ([ui cx] (to-field ui cx nil))
  ([ui cx default]
   (let [r (get (recs ui) (:row (:edit ui)))]
     (-> ui (assoc :cx cx)
         (update :edit assoc :cx cx :buf ""
                 :default (if (some? default) default (r cx))
                 :st0 (r 1))))))

(defn- finish-insert [ui]
  (-> ui (assoc :edit nil :cx 0) (update :s ss/move 1)))

(defn- direct-move
  "直接入力モードのカーソル移動。← は左端で前の行の右端へ、→ は右端で次の行の左端へ"
  [ui key]
  (let [cx (:cx ui) p (cur-pos ui) n (count (recs ui))]
    (case key
      :left  (cond (pos? cx) (assoc ui :cx (dec cx))
                   (pos? p)  (-> ui (assoc :cx 3) (update :s ss/move -1))
                   :else     ui)
      :right (cond (< cx 3)  (assoc ui :cx (inc cx))
                   (< p n)   (-> ui (assoc :cx 0) (update :s ss/move 1))
                   :else     ui)
      :up    (update ui :s ss/move -1)
      :down  (update ui :s ss/move 1)
      ui)))

(defn- insert-transition
  "挿入中に欄を確定したあとの移動 (EDIT.C ret() の ecode による分岐)
   K#:  Enter・← で確定、それ以外 (→ ↓ ↑) は ST へ
   ST:  ← で K#、→ で GT (ST を変えたら GT の初期値は ST−1)、Enter・↑・↓ で確定
        (確定時の GT・VEL は上の行のコピーのまま。原作どおり)
   GT:  ← で ST、→ で VEL、Enter・↑・↓ で確定
   VEL: ← で GT、それ以外で確定"
  [ui key]
  (let [{:keys [cx st0 row kind]} (:edit ui)
        r (get (recs ui) row)]
    (cond
      (= kind :ctrl-sym)                               ; 記号が決まったら ST と各パラメータへ
      (-> ui (assoc-in [:edit :fields] (sc/ctrl-insert-fields (sc/spc-code (:buf (:edit ui)))))
          (to-ctrl-field 0))
      (= kind :ctrl)
      (let [fi (:fi (:edit ui)) n (count (:fields (:edit ui)))]
        (cond (and (= key :left) (pos? fi)) (to-ctrl-field ui (dec fi))
              (< (inc fi) n)                 (to-ctrl-field ui (inc fi))
              :else                          (finish-insert ui)))
      (and kind (not= kind :note))
      (finish-insert ui)                               ; ']' '^' は1欄だけ
      :else
    (case (int cx)
      0 (if (#{:enter :left} key) (finish-insert ui) (to-field ui 1))
      1 (case key
          :left  (to-field ui 0)
          :right (to-field ui 2 (if (= (r 1) st0) (r 2) (max 0 (dec (r 1)))))
          (finish-insert ui))
      2 (case key
          :left  (to-field ui 1)
          :right (to-field ui 3)
          (finish-insert ui))
      3 (if (= key :left) (to-field ui 2) (finish-insert ui))))))

(defn fkey
  "F キーなら [表の番号 0〜9, Shift?]、それ以外は nil"
  [key]
  (when (keyword? key)
    (when-let [[_ sh d] (re-matches #"(shift-)?f(\d+)" (name key))]
      (let [d (Long/parseLong d)]
        (when (<= 1 d 10) [(dec d) (some? sh)])))))

(defn- edit-fkey
  "入力中の F キー (EDIT.C ret() の fnc_dis(2)/(8) の割り当て)
   ST 欄: ST と GT を表の値にし、GT 欄を開いたまま確定を待つ (Shift なら GT=0 で休符)
   GT 欄: GT を表の値に (Shift なら 0)【設計判断】
   VEL 欄: VEL を表の値にして行を確定
   上書き中: 入力を捨ててワンタッチ (既存行の F キーと同じ)【設計判断】"
  [ui n shift?]
  (let [{:keys [type cx row kind]} (:edit ui)
        {:keys [st gt vel]} (ss/onetouch-table (:s ui))
        rec (get (recs ui) row)
        gtv (str (if shift? 0 (gt n)))]
    (cond
      (and kind (not= kind :note)) ui
      (not= :insert type) (run-cmd (assoc ui :edit nil) :one-touch [cx n shift?])
      (= cx 1) (-> ui (update :s ss/set-rec row (assoc rec 1 (st n)))
                   (update-in [:edit :filled] conj 1)
                   (to-field 2)
                   (assoc-in [:edit :buf] gtv))
      (= cx 2) (assoc-in ui [:edit :buf] gtv)
      (= cx 3) (-> ui (update :s ss/set-rec row (assoc rec 3 (vel n))) finish-insert)
      :else ui)))

(defn- edit-key
  "欄の入力中のキー処理"
  [ui key]
  (let [e   (:edit ui)
        key (if (= key \space) :right key)]          ; 入力中の Space は → と同じ
    (cond
      (fkey key) (let [[n sh] (fkey key)] (edit-fkey ui n sh))

      (#{:esc :ctrl-c} key)
      (if (= :insert (:type e))
        (assoc ui :s (:snap e) :edit nil :cx 0)      ; 挿入を取り消す
        (assoc ui :edit nil))

      (= key :backspace) (update-in ui [:edit :buf] #(subs % 0 (max 0 (dec (count %)))))
      (= key :ctrl-u)    (assoc-in ui [:edit :buf] "")

      (#{:enter :left :right :up :down} key)
      (let [[res r] (edit-result ui)]
        (cond
          (= res :err) (set-msg ui r)
          (= res :exclusive)        ; / X: 挿入をやめてトラックエクスクルーシブの入力欄へ
          (assoc ui :s (:snap e) :edit nil :mode :input :kind :exclusive :buf "")
          (= :insert (:type e))
          (-> ui (update :s ss/set-rec (:row e) r)
              (update-in [:edit :filled] conj (:cx e))
              (insert-transition key))
          :else
          (-> ui (assoc :s (ss/replace-rec (:snap e) (:row e) r) :edit nil)
              (direct-move (if (= key :enter) :down key)))))

      (char? key) (update-in ui [:edit :buf] str key)
      :else ui)))

(defn- direct-char
  "直接入力モードで文字を打ったとき。対象外の文字なら nil (通常モードの割り当てへ)"
  [ui ch]
  (let [letter? (contains? #{\a \b \c \d \e \f \g \A \B \C \D \E \F \G} ch)
        digit?  (Character/isDigit (char ch))
        r       (get (recs ui) (cur-pos ui))]
    (cond
      (not (or letter? digit?)) nil
      (nil? r) (start-insert ui ch)                             ; END の上は挿入
      (< (r 0) 0x80) (start-over ui (if letter? 0 (:cx ui)) ch)
      ;; SAME・リピート終了の行で数字 (欄が K# 以外): 小節番号・回数を上書き (原作は cx>0 のとき)
      (and digit? (pos? (:cx ui)) (#{0xFC 0xF8} (r 0)))
      (start-over ui 1 ch (if (= (r 0) 0xFC) :same :repeat-end))
      (and letter? (>= (r 0) 0xFC)) (start-insert ui ch)       ; 小節線・SAME の上で音名は挿入
      (and digit? (>= (r 0) 0xFD)) (start-insert ui ch)        ; 小節線の上で数字は挿入
      ;; コントローラの行で数字: その欄を上書き (trk_ed の 0〜9)
      (and digit? (sc/ctrl-over-field r (:cx ui)))
      (let [[cx t] (sc/ctrl-over-field r (:cx ui))]
        (assoc-in (start-over ui cx ch :ctrl) [:edit :ftype] t))
      :else (set-msg ui "この行は直接入力できません"))))

;; ---- キー処理 ----
(defn- ui-action [ui [act & args]]
  (case act
    :page   (run-cmd ui :move [(* (first args) (page ui))])
    :mark   (if (:mark ui) (assoc ui :mark nil) (assoc ui :mark (cur-pos ui)))
    :cut-or-delete
    (if-let [m (:mark ui)]
      (let [p (cur-pos ui) n (Math/abs (long (- m p)))]
        (-> (run-cmd ui :cut [m p]) (assoc :mark nil)
            (cond-> (pos? n) (set-msg (str n " 行を削除しました (y で複写したものと同じく Ctrl-V / F9 で貼り付け)")))))
      (run-cmd ui :delete [1]))
    :copy   (if-let [m (:mark ui)]
              (let [u (-> (run-cmd ui :copy [m (cur-pos ui)]) (assoc :mark nil))]
                (if (= m (cur-pos ui))
                  u
                  (set-msg u (str (count (:clip (:s u))) " 行を複写しました (Ctrl-V / F9 で貼り付け)"))))
              (set-msg ui "先に v (F6) で範囲の開始行を決めてください"))
    :paste  (let [p (cur-pos ui) n (count (:clip (:s ui)))
                  [cmd label] (case (first args)
                                :rev [:paste-reversed "反転して貼り付けました"]
                                :mix [:mix-paste "ミックスしました"]
                                [:paste "貼り付けました"])]
              (if (zero? n)
                (set-msg ui "クリップボードが空です")
                (let [u (run-cmd ui cmd [])]
                  (if (get-in u [:s :msg])                              ; エラーならそのまま見せる
                    u
                    (-> u (update :s ss/goto-pos p)                      ; 原作どおりカーソルは先頭に
                        (set-msg (str n " 行を" label)))))))
    :track  (let [ui (assoc ui :mark nil)
                  n  (count (get-in ui [:s :song :tracks]))
                  to (-> (+ (:tr (:s ui)) (first args)) (max 0) (min (dec n)))]
              (if (= to (:tr (:s ui)))
                ui                                     ; 端のトラックではカーソルを動かさない
                (run-cmd ui :track-at [to])))          ; 同じ小節・同じ時刻へ (原作の CTRL+←→)
    :prompt (let [kind (first args)]
              (assoc ui :mode :input :kind kind
                     :buf (case kind
                            :save (or (:file (:s ui)) "song.edn")
                            :quantize "12 100"
                            :replace (or (:repl (:s ui)) "")
                            :delete-match (or (:delt (:s ui)) "")
                            :search (or (:srch (:s ui)) "")
                            :replicate (str (inc (:tr (:s ui))) " 1 1 1 0")
                            :replicate-all "1 1 1 0"
                            :replicate-range (str (inc (:tr (:s ui))) " 1 1 0")
                            :replicate-range-all "1 1 0"
                            "")))
    :effect (update ui :effects conj (first args))
    :help   (update ui :help? not)
    :onetouch (assoc ui :screen :onetouch :mark nil :ot (or (:ot ui) {:x 0 :y 0}))
    :excview (if (sc/exc-view (recs ui) (cur-pos ui) (ss/uex-table (:s ui)))
               (assoc ui :screen :excview :mark nil)
               (set-msg ui "カーソルをエクスクルーシブ (Tr.Exclu か U0〜U7) の行に置いてください"))
    :uexc   (let [r (get (recs ui) (cur-pos ui))
                  n (if (and r (<= 0x90 (r 0) 0x97)) (- (r 0) 0x90) 0)]   ; 原作どおり、カーソルが U0〜U7 の行ならその番号
              (assoc ui :screen :uexc :mark nil :ux {:x 0 :y (* 2 n)}))
    :rhythm (let [t (ss/cur-track (:s ui))]
              (if-not (:rhythm? t)
                (set-msg ui "RHY が OFF のトラックです (Tab のトラック設定で RHY を ON にすると編集できます)")
                (assoc ui :screen :rhythm :mark nil
                       :rh {:meas (polysted.edit/meas-no (recs ui) (cur-pos ui)) :cx 0 :cy 0 :x0 0
                            :page (if (> (bit-and (or (:rhy-byte t) 0x80) 31) 0) 16 0)})))
    :screen (assoc ui :screen (if (= :trkset (:screen ui)) :edit :trkset) :mark nil
                   :ts-row (+ 2 (:tr (:s ui))) :ts-col 0)
    :direct (assoc ui :direct? (not (:direct? ui)) :cx 0)
    :cx     (direct-move ui (first args))
    :insert-line (start-insert ui nil)
    :insert-cmd  (start-cmd-insert ui (first args))
    :insert-ctrl (start-ctrl-insert ui)
    :chord       (if-let [row (ss/chord-target (:s ui))]
                   (let [root ((get (recs ui) row) 0)]
                     (assoc ui :chord {:row row :root root :no 0 :keys (polysted.edit/chord-keys root 0)}))
                   (set-msg ui "コードにするノートがありません (ノートの行か、その次の小節線・END の上で X)"))
    :val         (run-cmd ui :value-step [(:cx ui) (first args)])
    :exclusive   (let [r (get (recs ui) (cur-pos ui))]
                   (assoc ui :mode :input :kind :exclusive
                          :buf (if (and r (= 0x98 (r 0))) (ss/exclusive-text (:s ui) (cur-pos ui)) "")))
    :same-extract (let [m (:mark ui) p (cur-pos ui)]
                    (-> (if m (run-cmd ui :same-extract [m p]) (run-cmd ui :same-extract []))
                        (assoc :mark nil)))
    :bs-delete   (if (pos? (cur-pos ui))
                   (-> ui (update :s ss/move -1) (run-cmd :delete [1]))
                   ui)
    :quit   (if (and (dirty? ui) (not (:confirm-quit? ui)))
              (-> ui (assoc :confirm-quit? true)
                  (set-msg "未保存の変更があります。終了するなら、もう一度 q (保存は w)"))
              (assoc ui :quit? true))))

(defn- run-binding [ui [kind & more]]
  (case kind
    :cmd (run-cmd ui (first more) (rest more))
    :ui  (ui-action ui more)))

(defn- submit [ui]
  (let [{:keys [kind buf]} ui
        ui (assoc ui :mode :normal :kind nil :buf "")]
    (cond
      (= kind :ux-name) (ux-submit ui kind buf)          ; 名前は空にもできる
      (str/blank? buf) ui
      (#{:ux-val :ux-val-over :ux-copy :ux-load :ux-load-res :ux-save :ux-save-res} kind) (ux-submit ui kind buf)
      (= kind :ot-val) (if-let [v (parse-long (str/trim buf))]
                         (run-cmd ui :set-onetouch [(ot-col ui) (get-in ui [:ot :y]) v])
                         (set-msg ui "数を入力してください"))
      (= kind :notes)  (run-cmd ui :insert-notes [buf])
      (= kind :mml)    (run-cmd ui :insert-mml [buf])
      (= kind :save)   (update ui :effects conj [:save (str/trim buf)])
      (#{:replace :delete-match} kind)
      ;; 範囲があればその範囲、なければカーソルから最後まで (原作どおり)
      (let [m (:mark ui) p (cur-pos ui)
            [from to] (if m [m p] [p (count (recs ui))])]
        (-> (run-cmd ui :replace-range [from to buf (if (= kind :replace) :replace :delete)])
            (assoc :mark nil)))
      (= kind :search) (run-cmd ui :find-next [buf true])
      (= kind :ts-edit) (ts-set ui buf)
      (= kind :exclusive) (run-cmd ui :edit-exclusive [buf])
      (= kind :command)
      (if-let [[_ n data] (re-matches #"(?i)\s*uex\s+(\d+)\s+(.*)" buf)]
        (run-cmd ui :set-user-exc [(Long/parseLong n) data])
      (if-let [[_ op path] (re-matches #"(?i)\s*exc(load|save)\s+(\S.*)" buf)]
        (update ui :effects conj [(if (= "load" (str/lower-case op)) :exc-load :exc-save) (str/trim path)])
      (if-let [[_ n data] (re-matches #"(?i)\s*ras\s+(\d+)\s+(.*)" buf)]
        (run-cmd ui :set-rhythm-assign [(dec (Long/parseLong n)) data])
      (case (str/lower-case (str/trim buf))
        "same"    (run-cmd ui :same-convert [])
        "extract" (ui-action ui [:same-extract])
        "same all" (run-cmd ui :same-convert-all [])
        "repeat"  (run-cmd ui :repeat-compress [false])
        "repeat all" (run-cmd ui :repeat-compress [true])
        "mplay"   (update ui :effects conj [:play-measure])
        "rec"     (open-record ui)
        "ras"     (set-msg ui "ras 番号(1-32) キー GT 名前 (例 ras 16 35 1 Kick)")
        "rev"     (ui-action ui [:paste :rev])
        "mix"     (ui-action ui [:paste :mix])
        (set-msg ui (str "コマンドが不明です: " buf "  (same / same all / extract / repeat / repeat all / rev / mix / mplay / rec / uex / excload / excsave)"))))))
      (#{:replicate :replicate-all :replicate-range :replicate-range-all} kind)
      (let [ns (mapv #(Long/parseLong %) (re-seq #"-?\d+" buf))
            m (:mark ui) p (cur-pos ui)]
        (-> (case kind
              :replicate (let [[tr top end times bias] ns] (run-cmd ui :replicate [(some-> tr dec) top end times bias]))
              :replicate-all (let [[top end times bias] ns] (run-cmd ui :replicate-all [top end times bias]))
              :replicate-range (let [[tr top times bias] ns] (run-cmd ui :replicate-range [(or m p) p (some-> tr dec) top times bias]))
              :replicate-range-all (let [[top times bias] ns] (run-cmd ui :replicate-range [(or m p) p nil top times bias])))
            (assoc :mark nil)))
      (= kind :note-ctrl)
      ;; 例: "L 40 100" "R 60 90" "O -10 10" "U" "G 6"。範囲が無ければカーソルから最後まで
      (let [t (str/upper-case (str/trim buf))
            mode (ss/note-ctrl-modes (first t))
            [a b] (map #(Long/parseLong %) (re-seq #"-?\d+" (subs t 1)))
            m (:mark ui) p (cur-pos ui)
            [from to] (if m [m p] [p (count (recs ui))])
            u (-> (run-cmd ui :note-ctrl [from to mode a b]) (assoc :mark nil))]
        (if (get-in u [:s :msg]) u (set-msg u (str "ノート操作 " t " を実行しました"))))
      (= kind :quantize)
      ;; 範囲があればその範囲、なければカーソルから最後まで (原作どおり)
      (let [[res rate] (map #(Long/parseLong %) (re-seq #"\d+" buf))
            m (:mark ui) p (cur-pos ui)
            [from to] (if m [m p] [p (count (recs ui))])]
        (if (and res rate)
          (let [u (-> (run-cmd ui :quantize [from to res rate]) (assoc :mark nil))]
            (if (get-in u [:s :msg]) u (set-msg u (str "クオンタイズしました (解像度 " res "、レート " rate "%)"))))
          (set-msg ui "解像度とレートを空白で区切って入力してください (例: 12 100)"))))))

(defn- prompt-key [ui key]
  (cond
    (= key :enter)        (submit ui)
    (#{:esc :ctrl-c} key) (assoc ui :mode :normal :kind nil :buf "")
    (= key :backspace)    (update ui :buf #(subs % 0 (max 0 (dec (count %)))))
    (= key :ctrl-u)       (assoc ui :buf "")
    (char? key)           (update ui :buf str key)
    :else                 ui))

(defn- chord-key
  "コード選択中のキー (原作 KEY_SUB.C chord_sel)"
  [ui key]
  (let [{:keys [row root no keys]} (:chord ui)
        set-no (fn [ui n] (assoc ui :chord {:row row :root root :no n :keys (polysted.edit/chord-keys root n)}))
        set-root (fn [ui r] (if (<= 0 r 127) (assoc ui :chord {:row row :root r :no no :keys (polysted.edit/chord-keys r no)}) ui))]
    (cond
      (= key :esc)   (assoc ui :chord nil)
      (= key :enter) (-> (run-cmd (assoc ui :chord nil) :insert-chord [row keys]))
      (= key :left)  (set-no ui (mod (dec no) 20))
      (= key :right) (set-no ui (mod (inc no) 20))
      (= key :up)    (assoc-in ui [:chord :keys] (polysted.edit/chord-rotate keys 1))
      (= key :down)  (assoc-in ui [:chord :keys] (polysted.edit/chord-rotate keys -1))
      (= key :pgdn)  (set-root ui (inc root))
      (= key :pgup)  (set-root ui (dec root))
      (fkey key)     (let [[n sh] (fkey key)] (set-no ui (+ n (if sh 10 0))))
      :else ui)))

(defn- common-key "補助の画面からの、編集画面と共通のキー (保存・終了・再生・ヘルプ・undo・コマンド欄)" [ui key]
  (run-binding ui (normal-keys key)))

(defn handle-key
  "1キー分の処理 (純粋)。副作用 (保存・再生) は :effects に積み、run! が実行する。"
  [ui key]
  (let [ui (-> ui (update :s dissoc :msg)
               (cond-> (not= key \q) (assoc :confirm-quit? false)))]
    (cond
      (and (= :input (:mode ui)) (= :rh-assign (:kind ui)) (= key :enter))
      (-> (assoc ui :mode :normal :kind nil :buf "")
          (run-cmd :set-rhythm-assign [(+ (get-in ui [:rh :page]) (get-in ui [:rh :cy])) (:buf ui)]))
      (and (= :input (:mode ui)) (#{:rh-st :rh-vel :rh-step :rh-jump} (:kind ui)) (= key :enter)) (rh-submit ui)
      (and (= :input (:mode ui)) (#{:rc-ch :rc-meas} (:kind ui)) (= key :enter)) (rc-submit ui)
      (= :input (:mode ui)) (prompt-key ui key)
      (= :rhythm (:screen ui)) (rh-key ui key common-key)
      (= :onetouch (:screen ui)) (ot-key ui key)
      (= :uexc (:screen ui)) (ux-key ui key)
      (= :excview (:screen ui)) (ev-key ui key)
      (= :record (:screen ui)) (rc-key ui key)
      (= :trkset (:screen ui)) (ts-key ui key common-key)
      (:chord ui)           (chord-key ui key)
      (:edit ui)            (edit-key ui key)
      (and (:mark ui) (= key :esc)) (assoc ui :mark nil)
      :else
      (or (when (:direct? ui)
            (if-let [b (direct-keys key)]
              (run-binding ui b)
              (cond
                (char? key) (direct-char ui key)
                ;; 既存行の F キー: ワンタッチ ST/GT/VEL (原作は OPT.1+F。Shift で和音もそろえる)
                (fkey key)  (let [[n sh] (fkey key)
                                  r (get (recs ui) (cur-pos ui))]
                              (if (and r (< (r 0) 0x80))
                                (run-cmd ui :one-touch [(:cx ui) n sh])
                                (set-msg ui "ワンタッチ入力はノートの行で使えます"))))))
          (if-let [b (normal-keys key)] (run-binding ui b) ui)))))

;; ---- 画面の行 ----
(def help-line
  "? ヘルプ  : 直接入力  n 音名  m MML  x 削除  v 範囲  ^V 貼付  u 戻す  p 再生  w 保存  q 終了")
(def direct-help-line
  "直接入力  Enter 挿入  A-G 0-9 上書き  F1-10 ワンタッチ  * [ ] ^ /  : 戻る  ? ヘルプ")
(def range-help-line
  "範囲指定中  ↑↓ 範囲を広げる  x/F7 削除  y/F8 複写  v/F6/Esc 解除")
(def edit-help
  "Enter 確定  →/Space 次の欄  ← 前の欄  Esc 取消")

(def help-text
  ["キー操作 (通常モード)"
   "  ↑ ↓ / k j      カーソル移動          PgUp PgDn   1画面移動"
   "  Home End / g G 先頭 / 末尾           ← → / Ctrl+← →  前後のトラック"
   "  n / Enter      音名入力 (C4 E G)     m           MML 入力 (l8 cdefg)"
   "  x / Delete     カーソル位置を1つ削除  u / Ctrl-R  元に戻す / やり直し"
   "  o 最適化   p / Space 再生・停止   P カーソルの小節から再生   w 保存   q 終了   : 直接入力モード"
   "  *              カーソル行に小節線を挿入 (後ろの SAME の参照をずらし、番号を振り直す)"
   "  [ / ]          リピート開始 / 終了 (] は合計の演奏回数を入力)   ^  小節複写 SAME (小節番号を入力)"
   "  /              コントローラを挿入: 記号 (V 音量 E 表情 H ホールド D モジュ N パン P ベンド"
   "                 G 音色 G@ バンク付き音色 T テンポ M チャンネル C アフター K キー圧 数字 CC番号)"
   "                 のあと ST と値を順に入力。既存のコントローラ行は数字でその欄を上書き"
   "  _              トラックエクスクルーシブ (カーソル行が Tr.Exclu なら書き換え、16進と gt ve ch cs ss)"
   "                 / の記号 U0〜U7 はユーザーエクスクルーシブ、I B R は Roland のデバイス・ベース・パラメータ"
   "                 ユーザーエクスクルーシブの表は Ctrl-X で uex 番号 データ (例 uex 0 41 10 42 12 cs 40 gt ss)"
   "  X              ワンタッチコード入力: カーソルのノートをコードに展開 (←→ 種類 ↑↓ 転回 PgUp/Dn ルート、"
   "                 F1-F10 MAJOR 7 maj7 6 maj9 9 7(b9) 7(#9) dim dim7、Shift+F 7(b5) m7(b5) aug aug7 7sus4 69 m9 m6 m7 m)"
   "  BS / Ctrl-Y    上の行を削除 / カーソル行を削除 (後ろの SAME の参照を直す)"
   "  Ctrl-L / Insert  削除行の復活 (最後に1行削除した行をカーソル位置に挿入)"
   "  v / F6         範囲の開始行 (もう一度で解除)。範囲は開始行とカーソル行の間 (下側の行は含まない)"
   "  x / F7  範囲を削除   y / F8  範囲を複写   Ctrl-V / F9  貼り付け   Ctrl-D / F10  行の二重化"
   "  Shift+F9 / Ctrl-X rev  反転貼付   M / Shift+F8 / Ctrl-X mix  ミックス貼付 (カーソル以降に時刻順に混ぜる)"
   "  r              レプリケート: トラック 開始小節 終了小節 回数 キーシフト → カーソル位置へ"
   "  R              全トラックのレプリケート: 開始小節 終了小節 回数 キーシフト → カーソルの小節へ"
   "  t / T          範囲 (v) を トラック 小節 回数 キーシフト へ / 全トラックの 小節 回数 キーシフト へ"
   "  Q              クオンタイズ (解像度 2〜48 とレート 50〜100%。範囲が無ければカーソルから最後まで)"
   "  }              ワンタッチ表 (ST/GT ASSIGN): 直接入力の F1〜F10 の ST・GT・VEL を変える (曲には保存しない)"
   "  E              エクスクルーシブの表示 (カーソルの Tr.Exclu か U0〜U7 のデータを 16 バイトずつ、SC-55 の画面データも)"
   "  Ctrl-X rec     レコーディング (MODE が Mix・Rec のトラックへ MIDI 入力を録音。メトロノーム・カウント付き)"
   "  Ctrl-U         ユーザーエクスクルーシブの表 (USER EXC): 8 個の名前とデータ、.REX / .RES の読み書き"
   "                 Ctrl-X の excload ファイル / excsave ファイル で、トラックエクスクルーシブを .EXC (テキスト) と読み書き"
   "  Ctrl-G         リズムパターン編集 (RHY が ON のトラック、カーソルの小節。原作の [記号])"
   "  Tab            トラック設定画面 (曲名・テンポ・拍子・調、トラックの MODE・CH・ST+・K#+・RHY・メモ)"
   "  Ctrl-X         コマンド欄: same / same all (重複した小節を SAME に) / extract (SAME を展開) /"
   "                 repeat / repeat all (繰り返しをリピート [ ]n に圧縮) / rev (反転貼付) / mix (ミックス貼付) /"
   "                 mplay (カーソルの小節を編集中のトラックだけ演奏、原作の小節プレイ)"
   "  Ctrl-^         SAME を展開 (範囲が無ければトラック全体、原作の Ctrl+^)"
   "  Ctrl-T / F3    置換  Ctrl-K / Shift+F3 指定削除 (例 C4:D#4  ,,,:+10  ,,:/2  /V:/E  @!36)"
   "  Ctrl-F / F4    検索の指定  Ctrl-N / F5 次を検索  Ctrl-P / Shift+F5 前を検索"
   "  Ctrl-E / Shift+F1  ノート操作: L 直線 / C 和音内 / R ランダム / O 足す (下限 上限) / T ST・GT をずらす"
   "                 U D 和音を低い順・高い順 / G S 和音をずらす (G は GT も、値はずらす ST)"
   "                 (直接入力モードの F キーはワンタッチなので、v x y Ctrl-V Ctrl-D を使う)"
   ""
   "直接入力モード (STED2 のエディット画面の入力)"
   "  Enter          カーソル行に1行挿入 (上のノートのコピー) → K# から入力"
   "  K# の入力      音名 (C4 E# Bb5 C<) かキー番号 (60)。何も打たず Enter で上の行と同じ"
   "  欄の移動       K#→ST: → ↓   ST→GT: →   Enter で行を確定して次の行へ"
   "                 ST で ↓ を押すと GT・VEL は上の行のまま (原作どおり)"
   "  A〜G / 0〜9    既存のノート行の欄を上書き (END・小節線の上では挿入)"
   "  ← → / Space    欄の移動   Ctrl+← → トラック   Home End 先頭・末尾"
   "  + . / - ,      欄の値を ±1 (> < は ±12 か ±10、範囲を超えたら反対側へ)  =  反転 (ベンド・パン・CH の A/B)"
   "  F1〜F10        ワンタッチ (Alt+1〜Alt+0 でも可)。値の表は ST 192 144 96 72 48 36 24 18 12 6"
   "                 入力中の ST 欄: ST と GT を入れて GT 欄へ (Shift+F で GT=0 の休符)"
   "                 入力中の VEL 欄: VEL を入れて確定。既存行: 欄カーソルの値を変えて次の音へ"
   "                 (K#・ST 欄なら ST と GT、GT 欄なら GT、VEL 欄なら VEL。Shift+F で和音もそろえる)"
   "  Esc / :        通常モードに戻る (入力中の Esc はその入力を取り消す)"])

(defn table-line
  "表の1行。=> [文字列 [[K#の開始 終了] [ST..] [GT..] [VEL..]]]
   形: マーク(1) MEAS(5) STEP(5) \": \" NOTE(4) K#(5) ST(5) GT(5) タイ印(1) VEL(5)"
  [mark meas step note vals tie?]
  (let [wide? (and (> (count note) 4) (= "" (vals 0)))          ; コントローラ名は NOTE と K# の幅を使う
        head  (str mark (pad-l (or meas "") 5) (pad-l (or step "") 5) ": " (pad-r note (if wide? 9 4)))]
  (loop [i (if wide? 1 0)
         s head
         spans (if wide? [[(count head) (count head)]] [])]
    (if (= i 4)
      [s spans]
      (let [start (inc (count s))
            s     (str s " " (pad-l (vals i) 4))
            spans (conj spans [start (count s)])
            s     (if (= i 2) (str s (if tie? "*" " ")) s)]
        (recur (inc i) s spans))))))

(def column-header
  (first (table-line " " "MEAS" "STEP" "NOTE" ["K#" "ST" "GT" "VEL"] false)))

(defn render-lines
  "画面全体の行 (装飾なし) と強調する位置を返す。
   => {:lines [文字列...] :cursor-line n または nil :cursor-span [開始 終了] または nil}
   行の並び: 見出し / 列見出し / 本体 (rows-4 行) / 状態行 / ヘルプ行"
  [ui]
  (case (:screen ui)
    :trkset (render-trkset ui)
    :rhythm (render-rhythm ui)
    :onetouch (render-onetouch ui)
    :uexc (render-uexc ui)
    :excview (render-excview ui)
    :record (render-record ui)
  (let [{:keys [cols s edit direct?]} ui
        body-h (body-height ui)
        v      (ss/table-view s (quot (dec body-h) 2))
        [rlo rhi] (when-let [m (:mark ui)] [(min m (:pos s)) (max m (:pos s))])
        in-range? (fn [i] (and rlo (<= rlo i) (< i rhi)))
        header (str (:header v)
                    (if-let [f (:file s)] (str " " f) "")
                    (if (dirty? ui) " *" "")
                    (if direct? "  [直接入力]" "")
                    (if rlo (str "  [範囲 " (- rhi rlo) " 行]") "")
                    (if (ss/playing?) "  ♪再生中" ""))
        line+span
        (fn [{:keys [idx cursor? meas step cells tie? text]}]
          (let [mark (cond cursor? ">" (in-range? idx) "|" :else " ")]
            (if (and cells (not (and edit (= idx (:row edit)) (kind-texts (:kind edit)))))
              (let [vals     (subvec cells 1)
                    editing? (and edit (= idx (:row edit)))
                    insert?  (and editing? (= :insert (:type edit)) (contains? #{nil :note} (:kind edit)))
                    ;; 挿入中の行は原作どおり空欄から: 確定した欄と入力中の文字だけ出す
                    vals     (cond
                               insert? (vec (for [i (range 4)]
                                              (cond (= i (:cx edit)) (:buf edit)
                                                    ((:filled edit) i) (vals i)
                                                    :else "")))
                               editing? (assoc vals (:cx edit) (:buf edit))
                               :else vals)
                    note     (if (and insert? (not ((:filled edit) 0))) "" (cells 0))]
                (table-line mark meas step note vals (and tie? (not insert?))))
              (let [text (if (and edit (= idx (:row edit)) (kind-texts (:kind edit)))
                           (str (kind-texts (:kind edit)) (:buf edit))
                           text)]
                [(str mark (pad-l (or meas "") 5) (pad-l (or step "") 5) ": " text) nil]))))
        rendered (mapv line+span (:rows v))
        body   (if (:help? ui) help-text (mapv first rendered))
        body   (vec (take body-h (concat body (repeat ""))))
        status (cond
                 (= :input (:mode ui)) (str (if (= :ts-edit (:kind ui)) (ts-labels (ts-field ui)) (prompt-labels (:kind ui)))
                                            "> " (:buf ui) "_")
                 (:chord ui) (let [{:keys [root no keys]} (:chord ui)]
                               (str "コード: " (ss/key->name root) " " (polysted.edit/chord-names no) " :"
                                    (apply str (map #(str " " (ss/key->name %)) keys))
                                    "   ←→ 種類 ↑↓ 転回 PgUp/Dn ルート F1-10 Enter 決定 Esc 取消"))
                 (:msg s)              (str "! " (:msg s))
                 (and edit (= :ctrl-sym (:kind edit)))
                 (str "コントローラ> " (:buf edit) "_   " ctrl-sym-help)
                 (and edit (= :ctrl (:kind edit)))
                 (str (edit-label edit) "> " (:buf edit) "_   "
                      (when (str/blank? (:buf edit))
                        (str "空のまま Enter で "
                             (let [r (get (ss/cur-recs s) (:row edit))]
                               (sc/ctrl-field-text r (:cx edit) (:ftype edit)))
                             "  "))
                      "Enter 確定  Esc 取消")
                 (and edit (kind-labels (:kind edit)))
                 (str (edit-label edit) "> " (:buf edit) "_   "
                      (if (= :same (:kind edit)) "複写元の小節 (この行より前)" "合計の演奏回数 (0 は無限)")
                      "  Enter 確定  Esc 取消")
                 edit                  (str (field-names (:cx edit)) "> " (:buf edit) "_   "
                                            (if (and (= :insert (:type edit)) (str/blank? (:buf edit)))
                                              (str "空のまま Enter で上の行と同じ (" (field-names (:cx edit)) "="
                                                   (let [d (:default edit)]
                                                     (if (zero? (:cx edit)) (ss/key->name d) d))
                                                   ")  Esc 取消")
                                              (if (= :insert (:type edit))
                                                (str edit-help "  F1-10 ワンタッチ")
                                                edit-help)))
                 :else                 "")
        cut    #(cut-width % cols)
        ci     (when-not (:help? ui)
                 (some (fn [[i r]] (when (:cursor? r) i)) (map-indexed vector (:rows v))))
        span   (when (and ci direct?)
                 (get (second (rendered ci)) (:cx ui)))]
    {:lines (mapv cut (concat [header column-header] body
                              [status (cond rlo range-help-line direct? direct-help-line :else help-line)]))
     :cursor-line (when ci (+ 2 ci))
     ;; 画面の行 → レコード番号 (GUI のマウスで使う。ヘルプ表示中は無し)
     :row-idxs (if (:help? ui) {} (into {} (map-indexed (fn [i r] [(+ 2 i) (:idx r)]) (:rows v))))
     :cursor-span span
     :range-lines (if (or (:help? ui) (nil? rlo))
                    #{}
                    (set (keep-indexed (fn [i r] (when (in-range? (:idx r)) (+ 2 i))) (:rows v))))})))

;; ============================================================
;; 2. 副作用 (端末)
;; ============================================================
(defn- sh [cmd]
  (let [p (.start (ProcessBuilder. ^java.util.List ["sh" "-c" cmd]))]
    (.waitFor p)
    (str/trim (slurp (.getInputStream p)))))

(defn- term-size []
  (let [[r c] (map #(Long/parseLong %) (str/split (sh "stty size < /dev/tty") #"\s+"))]
    [r c]))

(def ^:private ESC "\u001b")

(defn- draw! [ui]
  (let [{:keys [lines cursor-line cursor-span range-lines]} (render-lines ui)
        rev  (fn [l] (str ESC "[7m" l ESC "[0m"))
        dim  (fn [l] (str ESC "[2m" l ESC "[0m"))
        last-i (dec (count lines))
        out (map-indexed
             (fn [i l]
               (str (cond
                      (= i 0) (rev l)
                      (or (= i 1) (= i last-i)) (dim l)
                      (= i cursor-line)
                      (if-let [[a b] cursor-span]
                        (let [a (min a (count l)) b (min b (count l))]
                          ;; 直接入力: 行は太字、欄カーソルだけ反転
                          (str ESC "[1m" (subs l 0 a) (rev (subs l a b)) ESC "[1m" (subs l b) ESC "[0m"))
                        (rev l))
                      (range-lines i) (str ESC "[4m" l ESC "[0m")   ; 範囲は下線 (原作は網掛け)
                      :else l)
                    ESC "[K"))
             lines)]
    (print (str ESC "[H" (str/join "\r\n" out) ESC "[J"))
    (flush)))

(defn perform
  "副作用 (:effects の1つ) を実行する。TUI と GUI で共通"
  [ui [eff arg]]
  (try
    (case eff
      :save (do (when (re-find #"(?i)\.g36$" (str arg))
                  (throw (ex-info "G36 (RCP v3) の書き出しは未対応です。.rcp か .r36 で保存してください" {})))
                (cond
                  (ss/rcp-path? arg) (polysted.rcp/write-rcp! (:song (:s ui)) arg)   ; .rcp / .r36 は RCP 形式
                  (ss/smf-path? arg) (polysted.mml/write-mid! (ss/->song (:s ui)) arg)   ; .mid は SMF (書き出しのみ)
                  :else (polysted.edn/save-song! (:song (:s ui)) arg))
                (-> ui (assoc-in [:s :file] arg) (assoc :saved (:song (:s ui)))
                    (set-msg (str "保存しました: " arg))))
      ;; 再生を始めるとき、今のトラックが鳴らない MODE (Mute・Rec) なら案内する
      :toggle-play (do (reset! ss/S (:s ui))
                       (if (ss/playing?)
                         (do (ss/stop!) ui)
                         (do (ss/play!) (if-let [h (ss/mute-hint (:s ui))] (set-msg ui h) ui))))
      :play-measure (do (reset! ss/S (:s ui))
                        (if (= :nothing (ss/play-measure!))
                          (if-let [h (ss/mute-hint (:s ui))]
                            (set-msg ui h)
                            (throw (ex-info "この小節には演奏するデータがありません" {})))
                          ui))
      :play-here (do (reset! ss/S (:s ui))
                     (ss/play! (ss/measure-tick (:s ui)))
                     (if-let [h (ss/mute-hint (:s ui))] (set-msg ui h) ui))
      ;; ユーザーエクスクルーシブのファイル: arg = [パス 番号] (番号 nil は .REX で 8 個、番号は .RES で 1 個)
      :rex-load (let [[path n] arg
                      bs (java.nio.file.Files/readAllBytes (.toPath (java.io.File. ^String path)))
                      r (if n (polysted.rcp/res->entry bs) (polysted.rcp/rex->user-exc bs))]
                  (when (:error r) (throw (ex-info (:error r) {})))
                  (update ui :s #(if n (ss/uex-load-entry % n (:entry r)) (ss/uex-load % (:user-exc r)))))
      :rex-save (let [[path n] arg ue (ss/uex-table (:s ui))]
                  (java.nio.file.Files/write (.toPath (java.io.File. ^String path))
                                             ^bytes (if n (polysted.rcp/entry->res (ue n)) (polysted.rcp/user-exc->rex ue))
                                             ^"[Ljava.nio.file.OpenOption;" (make-array java.nio.file.OpenOption 0))
                  (set-msg ui (str "書き出しました: " path)))
      ;; トラックエクスクルーシブのテキストファイル .EXC
      ;; レコーディング
      :record-start (set-msg ui (:msg (ss/record-start! (:s ui) arg)))
      :rc-next-input (assoc-in ui [:rc :device] (ss/next-input (mapv :name (ss/midi-inputs)) (get-in ui [:rc :device])))
      :record-stop (-> ui (update :s ss/record-stop!) (assoc :screen :edit))
      :exc-load (let [msgs (sc/parse-exc-text (slurp arg :encoding "ISO-8859-1"))]
                  (when (nil? msgs) (throw (ex-info "EXC ファイルの中身が不正です (16進 00〜7F と gt ve ch cs ss、F0 〜 F7)" {})))
                  (update ui :s ss/load-exc-messages msgs))
      :exc-save (let [msgs (ss/exc-messages-at (:s ui))]
                  (when (nil? msgs) (throw (ex-info "カーソルをトラックエクスクルーシブ (Tr.Exclu) の行に置いてください" {})))
                  (spit arg (sc/exc-file-text msgs) :encoding "ISO-8859-1")
                  (set-msg ui (str "書き出しました: " arg))))
    (catch Exception e
      (set-msg ui (str "エラー: " (or (.getMessage e) (class e)))))))

(defn- read-keys
  "キーを待って読む。ESC の後は少し待ってシーケンスの続きを読む。
   待っている間、0.5 秒ごとに端末の大きさを調べ、変わっていたら [[:resize 行 桁]] を返す。"
  [^java.io.Reader rdr size]
  (loop [n 0]
    (if (.ready rdr)
      (let [c (.read rdr)]
        (if (neg? c)
          [:eof]
          (do (when (= c 27) (Thread/sleep 25))
              (decode-keys (loop [cs [c]]
                             (if (.ready rdr) (recur (conj cs (.read rdr))) cs))))))
      (do (Thread/sleep 30)
          (let [now (when (zero? (mod (inc n) 16)) (try (term-size) (catch Exception _ nil)))]
            (if (and now (not= now size))
              [(into [:resize] now)]
              (recur (inc n))))))))

(defn run!
  "TUI を起動する。セッションは polysted.session/S。q で終了すると S に結果が残る。"
  []
  (when-not (System/console)
    (throw (ex-info "端末 (TTY) 上で起動してください (nREPL などでは動きません)" {})))
  (let [old (sh "stty -g < /dev/tty")
        rdr *in*]
    (try
      (sh "stty raw -echo < /dev/tty")
      (print (str ESC "[?1049h" ESC "[?25l")) (flush)
      (loop [ui (let [[r c] (term-size)]
                  (assoc (new-ui (dissoc @ss/S :msg)) :rows r :cols c))]
        (reset! ss/S (:s ui))
        (draw! ui)
        (if (:quit? ui)
          :quit
          (let [ks (read-keys rdr [(:rows ui) (:cols ui)])]
            (if (some #{:eof} ks)
              :eof
              (let [ui (reduce (fn [ui k]
                                 (let [ui (cond
                                            (vector? k)   (let [[_ r c] k] (assoc ui :rows r :cols c))
                                            :else         (handle-key ui k))
                                       effs (:effects ui)]
                                   (reduce perform (assoc ui :effects []) effs)))
                               ui ks)]
                (recur ui))))))
      (finally
        (print (str ESC "[?25h" ESC "[?1049l")) (flush)
        (sh (str "stty " old " < /dev/tty"))))))

;; ============================================================
;; 簡易チェック (純粋部分のみ。期待値は手で追って求めた)
;; ============================================================
(defn run-checks []
  (let [keys* (fn [ui ks] (reduce handle-key ui ks))
        ui0   (new-ui (ss/new-session))
        dui   (assoc ui0 :direct? true)                 ; 直接入力モード
        recs* (fn [ui] (ss/cur-recs (:s ui)))
        typed (fn [s] (seq s))
        ks    (fn [& xs] (mapcat #(if (string? %) (seq %) [%]) xs))
        checks
        [["矢印・PgDn・Delete の解読"
          (decode-keys [27 91 65 27 91 66 27 91 54 126 27 91 51 126])
          [:up :down :pgdn :delete]]
         ["ESC 単独・Enter・Backspace・Ctrl"
          (decode-keys [27 13 127 18 97])  [:esc :enter :backspace :ctrl-r \a]]
         ["ESC O 形式・Ctrl+矢印・未知のシーケンスは無視"
          (decode-keys [27 79 72 27 91 50 51 126 27 91 49 59 53 67 120])
          [:home :ctrl-right \x]]
         ["F キー (xterm・Linux コンソール・Shift) と Alt+数字"
          (decode-keys [27 79 80 27 91 49 53 126 27 91 49 53 59 50 126 27 91 50 49 126
                        27 91 91 65 27 91 49 59 50 80 27 53 27 48])
          [:f1 :f5 :shift-f5 :f10 :f1 :shift-f1 :f5 :f10]]
         ["n で音名入力 → Enter で挿入"
          (recs* (keys* ui0 (concat [\n] (typed "C4 E G") [:enter])))
          [[60 48 46 64] [64 48 46 64] [67 48 46 64]]]
         ["m で MML 入力"
          (recs* (keys* ui0 (concat [\m] (typed "l8 c d") [:enter])))
          [[60 24 22 64] [62 24 22 64]]]
         ["Esc で入力を取り消す"
          (let [u (keys* ui0 (concat [\n] (typed "C4") [:esc]))] [(recs* u) (:mode u)])
          [[] :normal]]
         ["上下移動と x 削除・u で戻す"
          (let [u (keys* ui0 (concat [\n] (typed "C4 E G") [:enter :up :up \x]))]
            [(recs* u) (:pos (:s u)) (recs* (handle-key u \u))])
          [[[60 48 46 64] [67 48 46 64]] 1
           [[60 48 46 64] [64 48 46 64] [67 48 46 64]]]]
         ["g で先頭、G で末尾"
          (let [u (keys* ui0 (concat [\n] (typed "C4 E G") [:enter \g]))]
            [(:pos (:s u)) (:pos (:s (handle-key u \G)))])
          [0 3]]
         ["無効な音名はメッセージ行に出る"
          (-> (keys* ui0 (concat [\n] (typed "C4 X") [:enter])) render-lines :lines (nth 22))
          "! 無効な音名: X"]
         ["w は保存の副作用を積む (初期値は song.edn)"
          (:effects (keys* ui0 [\w :enter]))  [[:save "song.edn"]]]
         ["未保存なら q は1回目は終了しない"
          (let [u (keys* ui0 (concat [\n] (typed "C4") [:enter \q]))]
            [(:quit? u) (:quit? (handle-key u \q))])
          [false true]]
         ["変更がなければ q ですぐ終了"
          (:quit? (handle-key ui0 \q))  true]
         ["画面は rows 行、カーソル行の位置 (列見出しの下)"
          (let [r (render-lines (keys* ui0 (concat [\n] (typed "C4 E") [:enter])))]
            [(count (:lines r)) (:cursor-line r)])
          [24 4]]
         ["表の行の形 (MEAS STEP: NOTE K# ST GT VEL)"
          (-> (keys* ui0 (concat [\n] (typed "C4") [:enter \g])) render-lines :lines (nth 2))
          ">    1    1: C 4    60   48   46    64"]
         ["入力中はプロンプトを表示"
          (-> (keys* ui0 (concat [\n] (typed "C4"))) render-lines :lines (nth 22))
          "音名> C4_"]
         ["ヘルプ表示中はカーソル行なし"
          (:cursor-line (render-lines (handle-key ui0 \?)))  nil]
         ["全角は2桁として切る"
          [(cut-width "音名abc" 5) (cut-width "abc" 5) (cut-width "あいう" 5)]
          ["音名a" "abc" "あい"]]
         ;; ---- 直接入力モード (EDIT.C ret() を手で追った期待値) ----
         [": で直接入力モードに入り、もう一度 : で戻る"
          [(:direct? (handle-key ui0 \:)) (:direct? (keys* ui0 [\: \:]))]  [true false]]
         ["空の曲で Enter Enter → 60 48 46 64 が入り次の行へ"
          (let [u (keys* dui [:enter :enter])] [(recs* u) (:pos (:s u)) (:edit u)])
          [[[60 48 46 64]] 1 nil]]
         ["K# に E4、↓ で ST へ、24 を ↓ で確定 (GT VEL は上の行のまま)"
          (recs* (keys* dui (ks :enter "E4" :down "24" :down)))
          [[64 24 46 64]]]
         ["何も打たず Enter → 上の行と同じ内容"
          (recs* (keys* dui (ks :enter "D4" :down "24" :down :enter :enter)))
          [[62 24 46 64] [62 24 46 64]]]
         ["→ で全欄: ST を変えると GT の初期値は ST−1、VEL を 100 に"
          (recs* (keys* dui (ks :enter "60" :right "96" :right :right "100" :enter)))
          [[60 96 95 100]]]
         ["ST を変えなければ GT の初期値は今の GT"
          (recs* (keys* dui (ks :enter :right :right "30" :enter)))
          [[60 48 30 64]]]
         ["Space は → と同じ (C4 Space 24 Space 20 Space 90 Enter)"
          (recs* (keys* dui (ks :enter "C4 24 20 90" :enter)))
          [[60 24 20 90]]]
         ["← で前の欄に戻れる (ST から K# へ)"
          (recs* (keys* dui (ks :enter "C4" :right "24" :left "G4" :enter)))
          [[67 24 46 64]]]
         ["挿入中の Esc は行ごと取り消し、undo も増えない"
          (let [u (keys* dui (ks :enter "E4" :right "24" :esc))]
            [(recs* u) (count (:undo (:s u))) (:edit u)])
          [[] 0 nil]]
         ["1行の入力は undo 1回で消える"
          (recs* (handle-key (keys* dui (ks :enter "E4" :right "24" :right "20" :enter)) \u))
          []]
         ["END の上で音名を打つと挿入が始まる (オクターブは上のノート基準)"
          (recs* (keys* dui (ks :enter "C5" :enter "E" :enter)))
          [[72 48 46 64] [76 48 46 64]]]
         ["既存行で A〜G を打つと K# を上書きし、Enter で下へ"
          (let [u (keys* dui (ks :enter "C4" :enter :up "D" :enter))]
            [(recs* u) (:pos (:s u))])
          [[[62 48 46 64]] 1]]
         ["既存行で数字を打つとカーソルの欄を上書き (GT を 30)、→ で右の欄へ"
          (let [u (keys* dui (ks :enter :enter :up :right :right "30" :right))]
            [(recs* u) (:cx u)])
          [[[60 48 30 64]] 3]]
         ["上書きは undo 1回で戻る"
          (recs* (handle-key (keys* dui (ks :enter :enter :up "E" :enter)) \u))
          [[60 48 46 64]]]
         ["不正な値は入力を続け、メッセージを出す"
          (let [u (keys* dui (ks :enter "X" :enter))]
            [(some? (:edit u)) (-> u render-lines :lines (nth 22))])
          [true "! K# の値が不正です: X"]]
         ["VEL は 127 で頭打ち"
          (recs* (keys* dui (ks :enter :right :right :right "200" :enter)))
          [[60 48 46 127]]]
         ["トラックが1本なら Ctrl+→ でカーソルは動かない"
          (:pos (:s (keys* dui (ks :enter :enter :ctrl-right))))  1]
         ["入力中の ST 欄で F5 → ST48、GT 欄に 45 が入って確定待ち → Enter"
          (let [u (keys* dui (ks :enter "C4" :right :f5))]
            [(get-in u [:edit :cx]) (get-in u [:edit :buf]) (recs* (handle-key u :enter))])
          [2 "45" [[60 48 45 64]]]]
         ["入力中の ST 欄で Shift+F3 → ST96 GT0 (休符)"
          (recs* (keys* dui (ks :enter "C4" :right :shift-f3 :enter)))
          [[60 96 0 64]]]
         ["入力中の VEL 欄で F10 → VEL127 で確定し次の行へ"
          (let [u (keys* dui (ks :enter :right :right :right :f10))]
            [(recs* u) (:edit u) (:pos (:s u))])
          [[[60 48 46 127]] nil 1]]
         ["既存行の ST 欄で F5 → ST48 GT45、次のノートへ"
          (let [u (keys* (handle-key (keys* ui0 (concat [\m] (typed "l2 c d") [:enter])) \:)
                         [:home :right :f5])]
            [(recs* u) (:pos (:s u))])
          [[[60 48 45 64] [62 96 94 64]] 1]]
         ["* で小節線を挿入 (番号付き)、カーソルは下へ"
          (let [u (keys* dui (ks :enter :enter :enter :enter :up \*))]
            [(recs* u) (:pos (:s u))])
          [[[60 48 46 64] [0xFD 0 0 1] [60 48 46 64]] 2]]
         ["[ C4 ] 4 Enter → リピート (合計4回)"
          (recs* (keys* dui (ks \[ :enter "C4" :enter \] "4" :enter)))
          [[0xF9 0 0 0] [60 48 46 64] [0xF8 4 0 0]]]
         ["] の回数入力中は表と状態行に入力中の値"
          (let [l (:lines (render-lines (keys* dui (ks \] "3"))))]
            [(str/trimr (nth l 2)) (nth l 22)])
          [">    1     : ] REPEAT 3" "回数> 3_   合計の演奏回数 (0 は無限)  Enter 確定  Esc 取消"]]
         ["] の回数が不正なら入力を続ける / Esc で行ごと取り消し"
          (let [u (keys* dui (ks \] "300" :enter))]
            [(some? (:edit u)) (recs* (handle-key u :esc))])
          [true []]]
         ["^ で SAME: C4 * ^ 1 Enter → 小節1 を複写"
          (recs* (keys* dui (ks :enter "C4" :enter \* \^ "1" :enter)))
          [[60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]]]
         ["^ で自分以降の小節は指定できない"
          (let [u (keys* dui (ks :enter "C4" :enter \* \^ "2" :enter))]
            [(some? (:edit u)) (-> u render-lines :lines (nth 22))])
          [true "! 指定出来ない小節です (この行より前の小節を指定)"]]
         ["SAME の行で数字 (欄 ST) → 小節番号を上書き"
          (recs* (keys* dui (ks :enter "C4" :enter \* :enter "D4" :enter \* \^ "1" :enter
                                 :up :right "2" :enter)))
          [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFC 1 0 0]]]
         ["] の行で数字 → 回数を上書き"
          (recs* (keys* dui (ks \] "2" :enter :up :right "8" :enter)))
          [[0xF8 8 0 0]]]
         ["v で範囲 → ↓↓ → y で2行を複写、範囲は解除"
          (let [u (keys* ui0 (concat [\n] (typed "C4 D E") [:enter \g \v :down :down \y]))]
            [(:clip (:s u)) (:mark u) (recs* u)])
          [[[60 48 46 64] [62 48 46 64]] nil [[60 48 46 64] [62 48 46 64] [64 48 46 64]]]]
         ["範囲があれば x は範囲を削除、Ctrl-V で貼り付け (カーソルは先頭に残る)"
          (let [u (keys* ui0 (concat [\n] (typed "C4 D E") [:enter \g \v :down \x]))
                u2 (handle-key (handle-key u :down) :ctrl-v)]
            [(recs* u) (:pos (:s u)) (recs* u2) (:pos (:s u2))])
          [[[62 48 46 64] [64 48 46 64]] 0
           [[62 48 46 64] [60 48 46 64] [64 48 46 64]] 1]]
         ["上向きの範囲: 開始行は含まず、カーソル行から"
          (recs* (keys* ui0 (concat [\n] (typed "C4 D E") [:enter \v :up :up \x])))
          [[60 48 46 64]]]
         ["通常モードの F6 F8 F9 F10"
          (let [u (keys* ui0 (concat [\n] (typed "C4 D") [:enter \g :f6 :down :f8 :f9]))]
            [(recs* u) (recs* (handle-key (handle-key u :home) :f10))])
          [[[60 48 46 64] [60 48 46 64] [62 48 46 64]]
           [[60 48 46 64] [60 48 46 64] [60 48 46 64] [62 48 46 64]]]]
         ["直接入力モードでも v y が使え、Esc は範囲だけ解除"
          (let [u (keys* dui (ks :enter :enter :enter :enter :home \v :down \y))
                u2 (keys* dui (ks :enter :enter :home \v :esc))]
            [(count (:clip (:s u))) (:mark u2) (:direct? u2)])
          [1 nil true]]
         ["範囲の行は | 印、見出しに行数、ヘルプ行は範囲用"
          (let [l (:lines (render-lines (keys* ui0 (concat [\n] (typed "C4 D E") [:enter \g \v :down :down]))))]
            [(subs (nth l 2) 0 1) (subs (nth l 3) 0 1) (subs (nth l 4) 0 1)
             (boolean (re-find #"\[範囲 2 行\]" (nth l 0))) (nth l 23)])
          ["|" "|" ">" true "範囲指定中  ↑↓ 範囲を広げる  x/F7 削除  y/F8 複写  v/F6/Esc 解除"]]
         ["/ V Enter 0 Enter 100 Enter → 音量 100 の行、表は名前と値"
          (let [u (keys* dui (ks \/ "V" :enter "0" :enter "100" :enter))]
            [(recs* u) (:pos (:s u)) (str/trimr (nth (:lines (render-lines (handle-key u :up))) 2))])
          [[[0xEB 0 7 100]] 1 ">    1    1: VOLUME       0    7   100"]]
         ["/ の記号入力中は表に / と記号、状態行に記号の一覧"
          (let [l (:lines (render-lines (keys* dui (ks \/ "P"))))]
            [(str/trimr (nth l 2)) (subs (nth l 22) 0 10)])
          [">    1    1: / P" "コントローラ> P_"]]
         ["/ P (ベンド): ST 12、値 -4096"
          (recs* (keys* dui (ks \/ "P" :enter "12" :enter "-4096" :enter)))
          [[0xEE 12 0 32]]]
         ["/ G49 (音色 49 指定): ST だけ入力"
          (recs* (keys* dui (ks \/ "G49" :enter :enter)))
          [[0xEC 0 48 0]]]
         ["/ の記号が不正なら入力を続け、Esc で行ごと取り消し"
          (let [u (keys* dui (ks \/ "W" :enter))]
            [(some? (:edit u)) (recs* (handle-key u :esc))])
          [true []]]
         ["コントローラの行で数字 → その欄を上書き (VEL 欄の値を 80)"
          (recs* (keys* dui (ks \/ "V" :enter :enter "100" :enter :up :right :right :right "80" :enter)))
          [[0xEB 0 7 80]]]
         ["BS は上の行を削除、Ctrl-Y はカーソル行を削除"
          [(recs* (keys* ui0 (concat [\n] (typed "C4 D E") [:enter :up :backspace])))
           (recs* (keys* ui0 (concat [\n] (typed "C4 D E") [:enter :home :ctrl-y])))]
          [[[60 48 46 64] [64 48 46 64]] [[62 48 46 64] [64 48 46 64]]]]
         ["Shift+F9 で反転貼付、M でミックス貼付 (カーソルは先頭のまま)"
          (let [u (keys* ui0 (concat [\n] (typed "C4 D E") [:enter \g \v :down :down \y :end :shift-f9]))
                u2 (keys* ui0 (concat [\n] (typed "C4 D") [:enter \g \v :down \y :down \M]))]
            [(recs* u) (:pos (:s u)) (recs* u2) (:pos (:s u2))])
          [[[60 48 46 64] [62 48 46 64] [64 48 46 64] [62 48 46 64] [60 48 46 64]] 3
           [[60 48 46 64] [62 48 46 64] [60 48 46 64]] 2]]
         ["Q でクオンタイズ (カーソルから最後まで、解像度 48 レート 100)"
          (let [u (keys* (handle-key (keys* ui0 [\Q]) :ctrl-u) (concat (typed "48 100") [:enter]))
                u0 (update u :s #(-> % (ss/insert-recs [[60 50 46 64] [62 46 46 64] [64 48 46 64]]) (ss/goto-pos 0)))
                u1 (keys* u0 (concat [\Q :ctrl-u] (typed "48 100") [:enter]))]
            [(recs* u1) (get-in u1 [:s :msg])])
          [[[60 48 46 64] [62 48 46 64] [64 48 46 64]] "クオンタイズしました (解像度 48、レート 100%)"]]
         ["Ctrl-E L 40 100 で直線、範囲があれば範囲だけ"
          (let [u0 (update ui0 :s #(-> % (ss/insert-recs [[60 48 46 1] [62 48 46 1] [64 48 46 1] [65 48 46 1]]) (ss/goto-pos 0)))
                u1 (keys* u0 (concat [:ctrl-e] (typed "L 40 100") [:enter]))
                u2 (keys* u0 (concat [\v :down :down :ctrl-e] (typed "L 10 20") [:enter]))]
            [(mapv #(% 3) (recs* u1)) (mapv #(% 3) (recs* u2))])
          [[40 60 80 100] [10 20 1 1]]]
         ["Ctrl-E のモードが不正ならメッセージ"
          (get-in (keys* ui0 (concat [:ctrl-e] (typed "X 1 2") [:enter])) [:s :msg])
          "モードは L C R O T U D G S のどれかです"]
         ["Ctrl-T で置換 (カーソルから最後まで)、Ctrl-F で検索、Ctrl-N で次"
          (let [u0 (keys* ui0 (concat [\n] (typed "C4 D C4 E") [:enter :home]))
                u1 (keys* u0 (concat [:ctrl-t :ctrl-u] (typed ",,,:+10") [:enter]))
                u2 (keys* u0 (concat [:ctrl-f :ctrl-u] (typed "C4") [:enter]))
                u3 (handle-key u2 :ctrl-n)]
            [(mapv #(% 3) (recs* u1)) (get-in u1 [:s :msg]) (:pos (:s u2)) (:pos (:s u3))])
          [[74 74 74 74] "4 か所を置換しました" 2 2]]
         ["Ctrl-K で指定削除、指定の誤りはメッセージ"
          (let [u0 (keys* ui0 (concat [\n] (typed "C4 D C4") [:enter :home]))]
            [(mapv first (recs* (keys* u0 (concat [:ctrl-k :ctrl-u] (typed "C4") [:enter]))))
             (get-in (keys* u0 (concat [:ctrl-t :ctrl-u] (typed "/W:1") [:enter])) [:s :msg])])
          [[62] "コントローラの記号が不正です: /W"]]
         ["Ctrl-X same で SAME に変換、Ctrl-^ で展開、不明なコマンドはメッセージ"
          (let [u0 (update ui0 :s #(ss/insert-recs % [[60 48 46 64] [0xFD 0 0 1] [60 48 46 64] [0xFD 0 0 2]]))
                u1 (keys* u0 (concat [:ctrl-x] (typed "same") [:enter]))
                u2 (handle-key u1 :ctrl-caret)]
            [(count (recs* u1)) (count (recs* u2))
             (get-in (keys* ui0 (concat [:ctrl-x] (typed "foo") [:enter])) [:s :msg])
             (decode-keys [30])])
          [3 4 "コマンドが不明です: foo  (same / same all / extract / repeat / repeat all / rev / mix / mplay / rec / uex / excload / excsave)" [:ctrl-caret]]]
         ["_ でトラックエクスクルーシブ、/ X も同じ入力欄、Ctrl-X uex で表を設定"
          (let [u1 (keys* ui0 (concat [\_] (typed "41 10 cs 40 ss") [:enter]))
                u2 (keys* ui0 (concat [\/] (typed "X") [:enter]))
                u3 (keys* ui0 (concat [:ctrl-x] (typed "uex 1 43 10 gt") [:enter]))]
            [(recs* u1) [(:mode u2) (:kind u2) (recs* u2)]
             (vec (take 4 (second (get-in u3 [:s :song :user-exc 1]))))])
          [[[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x83 0x40] [0xF7 0 0x84 0xF7]]
           [:input :exclusive []]
           [0x43 0x10 0x80 0xF7]]]
         ["Ctrl-X mplay は小節プレイの副作用を積む"
          (:effects (keys* ui0 (concat [:ctrl-x] (typed "mplay") [:enter])))  [[:play-measure]]]
         ["Ctrl-X repeat でループ圧縮"
          (recs* (keys* (update ui0 :s #(ss/insert-recs % (vec (repeat 4 [60 12 10 64])))) (concat [:ctrl-x] (typed "repeat") [:enter])))
          [[0xF9 0 0 0] [60 12 10 64] [0xF8 4 0 0]]]
         ["r でレプリケート (トラック1 の小節1 を 2 回、+12)"
          (let [u0 (update ui0 :s #(-> % (ss/insert-recs [[60 48 46 64] [0xFD 0 0 1]])))
                u1 (keys* u0 (concat [\r :ctrl-u] (typed "1 1 1 2 12") [:enter]))]
            [(mapv first (recs* u1)) (get-in u1 [:s :msg])])
          [[60 0xFD 72 0xFD 72 0xFD] "TR1 の小節 1〜1 を 2 回コピーしました"]]
         ["Tab でトラック設定画面: K#+ を -5、MODE を Mute、TR で編集画面へ"
          (let [u0 (handle-key ui0 :tab)
                u1 (keys* u0 (concat [:right :right :right :right :enter :ctrl-u] (typed "-5") [:enter]))
                u2 (keys* u1 [:left :left :left :enter])
                t  (get-in u2 [:s :song :tracks 0])
                u3 (keys* u2 [:left :enter])]
            [(:screen u0) (sc/key-shift-str (:key-shift t)) (:mute? t) (:screen u3)
             (subs (nth (:lines (render-lines u2)) 5) 0 24)])
          [:trkset "-5" true :edit "> 1  Mute A1     0  -5  "]]
         ["トラック設定画面: 曲の項目 (テンポ +1、拍子、調)、a でトラック追加"
          (let [u0 (keys* (handle-key ui0 :tab) [:up :up])
                u1 (keys* u0 [:down \+ :right :right :enter :ctrl-u \3 \/ \4 :enter :right :enter :ctrl-u \G :enter])
                u2 (handle-key u1 \a)]
            [(get-in u1 [:s :song :tempo]) (get-in u1 [:s :song :time-sig]) (get-in u1 [:s :song :key])
             (count (get-in u2 [:s :song :tracks])) (get-in u2 [:s :song :tracks 1 :ch])])
          [121 [3 4] 1 2 2]]
         ["Ctrl-G でリズム画面 (RHY ON のみ)、Acou SD に 8、Space で次の強さ、Esc で戻る"
          (let [u0 (update ui0 :s #(-> % (ss/insert-recs [[0xFD 0 0 1]]) (assoc-in [:song :tracks 0 :rhythm?] true) (ss/goto-pos 1)))
                u1 (keys* u0 [:ctrl-g :up :up :up \8 :right \space \space])
                p  (ss/rhythm-pattern (:s u1) 2)]
            [(get-in (handle-key ui0 :ctrl-g) [:s :msg] "") (:screen u1)
             (get-in p [:steps 0 :hits 14]) (get-in p [:steps 1 :hits 14]) (:screen (handle-key u1 :esc))
             (subs (nth (:lines (render-lines u1)) 16) 0 17)])
          ["RHY が OFF のトラックです (Tab のトラック設定で RHY を ON にすると編集できます)" :rhythm
           [112 1] [28 1] :edit "15 Acou SD     8 "]]
         ["リズム画面の小節: y で複写 → 次の小節で Ctrl-V (前に貼付)、x で切り取り、j で移動、Del のステップを Ctrl-L で戻す"
          (let [rs [[36 12 1 100] [0 180 0 0] [0xFD 0 0 1] [38 192 1 100] [0xFD 0 0 2]]
                u0 (update ui0 :s #(-> % (ss/insert-recs rs) (assoc-in [:song :tracks 0 :rhythm?] true) (ss/goto-pos 0)))
                u1 (keys* u0 [:ctrl-g \y :pgdn :ctrl-v])
                u2 (keys* u1 [\x])
                u3 (keys* u2 (ks \j "1" :enter))
                u4 (keys* u3 [:delete])
                u5 (keys* u4 [:ctrl-l])]
            [(count (recs* u1)) (get-in u1 [:rh :meas]) (= rs (recs* u2)) (get-in u3 [:rh :meas])
             (take 2 (recs* u4)) (= rs (recs* u5)) (get-in (keys* u0 [:ctrl-g :ctrl-l]) [:s :msg])])
          [8 2 true 1 [[0 180 0 0] [0xFD 0 0 1]] true "削除したステップがありません"]]
         ["レコーディング: Ctrl-X rec で開き、C O S で設定、Enter で開始の副作用、トラック設定の MODE は Enter で Play→Mute→Mix→Rec"
          (let [u (keys* ui0 (ks :ctrl-x "rec" :enter \c :ctrl-u "5" :enter \o \s :ctrl-u "3 4" :enter))
                u2 (keys* u [:enter])
                t (keys* ui0 [:tab :right :enter])
                modes (map #(polysted.ctrl/mode-str (first (get-in (keys* ui0 (concat [:tab :right] (repeat % :enter))) [:s :song :tracks]))) [1 2 3 4])]
            [(:screen u) (:rc u) (:effects u2) (str/trim (nth (:lines (render-lines u)) 5)) (:screen (keys* u [:esc]))
             (:screen t) modes])
          [:record {:ch 5 :thru? false :metronome? true :top 3 :btm 4}
           [[:record-start {:ch 5 :thru? false :metronome? true :top 3 :btm 4}]]
           "録音するトラック: なし (Tab のトラック設定で MODE を Mix か Rec に)" :edit :trkset ["Mute" "Mix" "Rec" "Play"]]]
         ["直接入力モードで + > - < = (欄の値の増減と反転)"
          (let [u (keys* dui (ks :enter :enter :up \+ \> :right :right :right \< \- :left \=))]
            (recs* u))
          [[73 48 46 53]]]
         ["Ctrl-Y で削除、Ctrl-L で復活"
          (let [u (keys* ui0 (concat [\n] (typed "C4 D") [:enter :home :ctrl-y :down :ctrl-l]))]
            (recs* u))
          [[62 48 46 64] [60 48 46 64]]]
         ["X でコード入力: → で 7、↑ で転回、Enter で展開 (END の上なら前のノート)"
          (let [u0 (keys* ui0 (concat [\n] (typed "C4") [:enter]))
                u1 (keys* u0 [\X :right :up])
                u2 (handle-key u1 :enter)]
            [(get-in u1 [:chord :keys]) (subs (nth (:lines (render-lines u1)) 22) 0 24) (recs* u2) (:pos (:s u2))])
          [[64 67 70 72] "コード: C4 7 : E4 G4 A#4 C5" [[64 0 46 64] [67 0 46 64] [70 0 46 64] [72 48 46 64]] 3]]
         ["リズム画面の a で割り当てを変える、Ctrl-X ras でも"
          (let [u0 (update ui0 :s #(-> % (ss/insert-recs [[0xFD 0 0 1]]) (assoc-in [:song :tracks 0 :rhythm?] true) (ss/goto-pos 1)))
                u1 (keys* u0 (concat [:ctrl-g :up :up \a :ctrl-u] (typed "35 2 Kick") [:enter]))
                u2 (keys* u0 (concat [:ctrl-x] (typed "ras 1 75 1 Claves") [:enter]))]
            [(get-in u1 [:s :song :rhythm-assign 15]) (get-in u2 [:s :song :rhythm-assign 0])])
          [["Kick" 35 2] ["Claves" 75 1]]]
         ["Q の値が範囲外ならメッセージ"
          (get-in (keys* ui0 (concat [\Q :ctrl-u] (typed "60 100") [:enter])) [:s :msg])
          "解像度は 2〜48 です"]
         ["← は左端で前の行の右端へ"
          (let [u (keys* dui (ks :enter :enter :left))] [(:pos (:s u)) (:cx u)])
          [0 3]]
         ["Enter 1回目は空欄の行 (原作どおり)、欄カーソルは K#"
          (let [r (render-lines (keys* dui [:enter]))]
            [(str/trimr (nth (:lines r) 2)) (:cursor-span r)])
          [">    1    1:" [18 22]]]
         ["入力中は打った文字だけ、確定した欄は値を出す"
          [(str/trimr (nth (:lines (render-lines (keys* dui (ks :enter "E5")))) 2))
           (str/trimr (nth (:lines (render-lines (keys* dui (ks :enter "E5" :right "2")))) 2))]
          [">    1    1:        E5" ">    1    1: E 5    76    2"]]
         ["状態行に既定値 (上の行の値) を案内"
          (nth (:lines (render-lines (keys* dui [:enter]))) 22)
          "K#> _   空のまま Enter で上の行と同じ (K#=C4)  Esc 取消"]
         ["Enter 2回目で上の行の内容が記入される"
          (str/trimr (nth (:lines (render-lines (keys* dui (ks :enter "D4" :enter :enter :enter :up)))) 3))
          ">         2: D 4    62   48   46    64"]
         ["ワンタッチ表の画面: } で開き、GT を入力して > で +12、表示とカーソル、Esc で戻る"
          (let [u (keys* ui0 (ks \} :right :enter :ctrl-u "60" :enter \>))
                {:keys [lines cursor-line cursor-span]} (render-lines u)
                l (nth lines cursor-line)]
            [(get-in u [:s :onetouch :gt 0]) l (subs l (first cursor-span) (second cursor-span)) (:screen (keys* u [:esc]))])
          [72 "  F 1:   192   72    1" "  72" :edit]]
         ["USER EXC の画面: Ctrl-U で開き、16進で上書き・F2 で ve を挿入・名前、表示"
          (let [u (keys* ui0 (ks :ctrl-u :down "41" :enter :f2 :up :enter "GS" :enter))
                {:keys [lines]} (render-lines u)]
            [(subvec (second ((ss/uex-table (:s u)) 0)) 0 3) (nth lines 1) (subs (nth lines 2) 0 22)
             (:screen (keys* u [:esc])) (count (:undo (:s u)))])
          [[0x41 0x81 0xF7] " 0  MEMO: GS" "     F0 41 ve F7 .. .." :edit 3]]
         ["エクスクルーシブの表示: E で開く (エクスクルーシブの行でなければメッセージ)、↓ で次のエクスクルーシブへ、Esc で戻る"
          (let [s (-> (ss/new-session)
                      (ss/insert-recs [[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0xF7 0xF7] [60 48 46 64]
                                       [0x98 0 0 0] [0xF7 0 0x43 0x10] [0xF7 0 0xF7 0xF7]])
                      (ss/goto-pos 0))
                u (keys* (new-ui s) [\E])
                u2 (keys* u [:down])]
            [(:screen u) (nth (:lines (render-lines u)) 4) (:pos (:s u2)) (nth (:lines (render-lines u2)) 4)
             (:screen (keys* u2 [:esc])) (get-in (keys* (new-ui (ss/goto-pos s 3)) [\E]) [:s :msg])])
          [:excview "00 F0 41 10 F7" 4 "00 F0 43 10 F7" :edit "カーソルをエクスクルーシブ (Tr.Exclu か U0〜U7) の行に置いてください"]]
         ["ファイル: Ctrl-X excsave / excload は副作用に、.REX と .EXC を書いて読むと同じ"
          (let [tmp (fn [ext] (doto (java.io.File/createTempFile "polysted" ext) (.deleteOnExit)))
                rex (str (tmp ".rex")) exc (str (tmp ".exc"))
                u (keys* ui0 (ks :ctrl-u :down "41" :enter :up :enter "GS" :enter))
                u2 (perform (new-ui (ss/new-session)) [:rex-load [rex nil]])
                _ (perform u [:rex-save [rex nil]])
                u3 (perform (new-ui (ss/new-session)) [:rex-load [rex nil]])
                es (new-ui (-> (ss/new-session) (ss/load-exc-messages [[0x41 0x10 0x80]]) (ss/goto-pos 0)))
                _ (perform es [:exc-save exc])
                u4 (perform (new-ui (ss/new-session)) [:exc-load exc])]
            [(:effects (keys* ui0 (ks :ctrl-x "excsave a.exc" :enter)))
             (some? (get-in u2 [:s :msg]))
             (= (ss/uex-table (:s u)) (ss/uex-table (:s u3)))
             (slurp exc) (ss/cur-recs (:s u4))])
          [[[:exc-save "a.exc"]] true true "F0 41 10 GT F7\r\n" [[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x80 0xF7]]]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted.tui: OK " (count checks) "件"))
      (doseq [[l a e] bad]
        (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(defn -main
  "clojure -M:tui [file]  TUI を単体で起動する (file は .edn / .rcp / .r36 / .g36 / .mid)"
  [& args]
  (ss/open-at-start! (first args))
  (run!)
  (ss/stop!)
  (shutdown-agents)
  (System/exit 0))
