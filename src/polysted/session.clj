;; src/polysted/session.clj -- 編集セッション (UI非依存コア + REPL層) (動作未確認)
;;
;; 構成:
;;   1. コア: すべて純粋関数 (state, 引数...) -> state。TUI/GUI からも同じ関数を使う。
;;   2. view: 画面に出す内容を「データ」で返す。描画は各UIが行う。
;;   3. REPL層: atom S に状態を持ち、短い名前の関数で操作する。
;;
;; 使い方 (REPL):
;;   (require '[polysted.session :as s])   ; clojure -M:repl なら dev/user.clj で済んでいる
;;   (s/mml "l8 o4 cdefgab<c")  (s/n "C4 E G")  (s/show)  (s/play!)
(ns polysted.session
  (:refer-clojure :exclude [replicate])
  (:require [clojure.string :as str]
            [polysted.mml] [polysted.rcp] [polysted.edit] [polysted.notename] [polysted.edn] [polysted.smf] [polysted.replace] [polysted.rhythm] [polysted.roll] [polysted.visual] [polysted.record])
  ;; コントローラ・調などの純粋関数は src/polysted/ctrl.clj に分けた (名前はそのまま使う)
  (:require [polysted.ctrl :refer [ctrl-cells ctrl-insert-fields exc-hex key-shift-str key-str mode-str parse-ch
                                parse-exc-data parse-key parse-rhythm-assign rhy-str set-ctrl-field
                                spc-code supported-ctrls uex-data uex-ins uex-del uex-sum track-mode]])
  (:import (javax.sound.midi MidiSystem MetaEventListener)))

;; ============================================================
;; 1. コア (純粋)
;; ============================================================
(defn new-song []
  {:title "" :timebase 48 :tempo 120 :key 0 :time-sig [4 4]
   :tracks [{:no 0 :ch 1 :recs [] :mute? false :rhythm? false :comment ""}]})

(defn new-session
  ([] (new-session (new-song)))
  ([song] {:song song :tr 0 :pos 0 :undo [] :redo [] :clip []}))

(defn song-from-rcp
  "RCP/R36 を読んで、レコード列(:recs)を持つ曲にする。レコードはファイルの4バイトそのまま
   (エクスクルーシブなども失わない)。ヘッダの項目 (メモ・リズム割り当てなど) も持つので、
   save-rcp! で書き戻せる。G36 (RCP v3) は 4 バイトの形に変換して読む (小節線の番号を振り直す。
   変換の注意は :load-warnings)"
  [path]
  (let [song (polysted.rcp/read-rcp path)
        g36? (seq (:load-warnings song))]
    (update song :tracks
            (fn [ts] (mapv #(cond-> (dissoc % :events) g36? (update :recs polysted.edit/renumber-measures)) ts)))))

(defn cur-track [s] (get-in s [:song :tracks (:tr s)]))
(defn cur-recs [s] (:recs (cur-track s)))
(defn- set-recs [s recs] (assoc-in s [:song :tracks (:tr s) :recs] recs))

(defn- push-undo [s]
  (-> s
      (update :undo #(vec (take-last 100 (conj % (select-keys s [:song :tr :pos])))))
      (assoc :redo [])))

(defn undo [s]
  (if-let [snap (peek (:undo s))]
    (-> s (update :undo pop)
        (update :redo conj (select-keys s [:song :tr :pos]))
        (merge snap))
    s))

(defn redo [s]
  (if-let [snap (peek (:redo s))]
    (-> s (update :redo pop)
        (update :undo conj (select-keys s [:song :tr :pos]))
        (merge snap))
    s))

(defn move [s n]
  (assoc s :pos (-> (+ (:pos s) n) (max 0) (min (count (cur-recs s))))))

(defn goto-pos [s n]
  (assoc s :pos (-> n (max 0) (min (count (cur-recs s))))))

(defn select-track [s n]
  (assoc s :tr (-> n (max 0) (min (dec (count (get-in s [:song :tracks]))))) :pos 0))

(defn add-track [s ch]
  (let [n (count (get-in s [:song :tracks]))]
    (-> s push-undo
        (update-in [:song :tracks] conj
                   {:no n :ch ch :recs [] :mute? false :rhythm? false :comment ""})
        (assoc :tr n :pos 0))))

(defn set-rec
  "idx 番のレコードを置き換える (undo 履歴は積まない。入力途中の更新用)"
  [s idx rec]
  (assoc-in s [:song :tracks (:tr s) :recs idx] rec))

(defn replace-rec
  "idx 番のレコードを置き換える (undo 1回分)"
  [s idx rec]
  (if (= rec (get (cur-recs s) idx))
    s
    (-> s push-undo (set-rec idx rec))))

(defn insert-recs [s new-recs]
  (if (empty? new-recs)
    s
    (-> s push-undo
        (set-recs (polysted.edit/insert-records (cur-recs s) (:pos s) new-recs))
        (update :pos + (count new-recs)))))

(defn delete-recs
  "カーソル位置から n 個のレコードを削除。後ろの SAME の参照を直す
   (1行なら DEL キーの規則 polysted.edit/delete-line、複数行なら範囲削除の規則)。
   1行のときは消した行を削除行バッファ :line-buf に入れる (原作の lcbuf。Ctrl-L で復活)。
   トラックエクスクルーシブ (0x98) とコメント (0xF6) は続きの 0xF7 の行ごと消す"
  [s n]
  (let [recs (cur-recs s)
        p (:pos s)
        r (get recs p)
        n (if (and (= n 1) r (#{0x98 0xF6} (r 0)))
            (inc (count (take-while #(= 0xF7 (% 0)) (subvec recs (inc p)))))
            n)
        to (min (count recs) (+ p n))
        group? (and r (#{0x98 0xF6} (r 0)))]
    (if (>= p to)
      s
      (-> s push-undo
          (cond-> (or (= to (inc p)) group?) (assoc :line-buf (subvec recs p to)))
          (set-recs (if (= to (inc p))
                      (polysted.edit/delete-line recs p)
                      (:recs (polysted.edit/delete-range recs p to))))))))

(declare insert-line)

(defn restore-line
  "削除行バッファの行をカーソル位置に挿入する (原作の UNDO キー / Ctrl-L)。小節の数が変われば SAME の参照をずらす"
  [s]
  (if-let [buf (seq (:line-buf s))]
    (let [s1 (insert-line s (first buf))
          row (dec (:pos s1))]
      (if (next buf)
        (set-recs s1 (vec (concat (subvec (cur-recs s1) 0 (inc row)) (rest buf) (subvec (cur-recs s1) (inc row)))))
        s1))
    (assoc s :msg "削除行バッファは空です")))

(defn mml-defaults
  "STED2 と同じ: カーソル直前の最も近いノート/休符から ST GT vel を引き継ぐ"
  [recs pos]
  (if-let [r (->> (subvec recs 0 pos) reverse (filter #(< (% 0) 0x80)) first)]
    (let [st  (if (zero? (r 1)) 48 (r 1))
          gt  (if (and (zero? (r 2)) (> st 2)) (- st 2) (r 2))
          vel (if (zero? (r 3)) 64 (r 3))]
      {:st st :gt gt :vel vel})
    {}))

(defn key-at
  "位置 pos に入れる音に使う調 (原作 KEY_SUB.C scale_no、EDIT.C・mml_inp の呼び出し側)。
   リズムトラックは 0。それ以外は pos の直前の行から上へたどり、0xF5 (調の変更) があればその調、
   小節線・SAME・END (0xFC 以上) か先頭まで無ければ曲の調。
   原作はカーソル行から数え始める (カーソルが小節線や END の上だと、その小節の 0xF5 を見ない) が、
   挿入する音は カーソル行の前に入るので、直前の行から数える【設計判断】"
  ([s] (key-at s (:pos s)))
  ([s pos]
   (if (:rhythm? (cur-track s))
     0
     (let [recs (cur-recs s)
           song-key (or (get-in s [:song :key]) 0)]
       (loop [j (dec (min pos (count recs)))]
         (if (neg? j)
           song-key
           (let [a ((recs j) 0)]
             (cond (= a 0xF5) ((recs j) 1)
                   (> a 0xFB) song-key
                   :else      (recur (dec j))))))))))

(defn insert-mml [s text]
  (let [recs (cur-recs s)
        opts (merge {:key-sig (key-at s)} (mml-defaults recs (:pos s)))
        evs  (polysted.mml/parse-mml text opts)]
    (insert-recs s (polysted.edit/events->records evs))))

(defn last-note-key
  "カーソル直前の最も近い発音ノート(休符は除く)のキー番号。なければ nil。
   音名入力でオクターブを省略したときの基準にする。"
  [recs pos]
  (some (fn [r] (when (and (< (r 0) 0x80) (pos? (r 2)) (pos? (r 3))) (r 0)))
        (rseq (subvec recs 0 pos))))

(defn insert-notes
  "音名入力 (\"C4 E G\") をカーソル位置に挿入する。
   ST GT vel は MML と同じく直前のノート/休符から引き継ぎ、
   オクターブ省略時の基準は直前の発音ノートのキー。調号は key-at (小節の中の 0xF5 か曲の :key)。
   リズムトラックでは調号を無視する。無効なトークンは捨てて :msg に残す。"
  [s text]
  (let [recs (cur-recs s)
        pos  (:pos s)
        ctx  (merge {:key-sig (key-at s)}
                    (mml-defaults recs pos)
                    (when-let [k (last-note-key recs pos)] {:last-key k})
                    (when (:rhythm? (cur-track s)) {:ignore-keysig? true}))
        {:keys [records errors]} (polysted.notename/parse-notes text ctx)]
    (cond-> (insert-recs s records)
      (seq errors) (assoc :msg (str "無効な音名: " (str/join " " errors))))))

(def onetouch
  "ワンタッチ入力の表 (F1〜F10)。STED2.CNF の #fkey_st / #fkey_gt / #fkey_ve の既定値"
  {:st  [192 144 96 72 48 36 24 18 12 6]
   :gt  [188 140 92 68 45 34 22 16 10 5]
   :vel [1 15 29 43 57 71 85 99 113 127]})

(defn onetouch-table "今のワンタッチ表 (ST/GT ASSIGN で変えていなければ既定)" [s] (or (:onetouch s) onetouch))

(defn set-onetouch
  "ワンタッチ表 (原作の } = ST/GT ASSIGN、ASSIGN.C stgt_as) の col (:st :gt :vel) の i 番 (0〜9) を v に。
   ST・GT は 0〜255、VEL は 0〜127。曲ではなくエディタの設定なので undo は積まない (原作も曲に保存しない)"
  [s col i v]
  (let [mx (if (= col :vel) 127 255)]
    (if (and (integer? v) (<= 0 v mx) (integer? i) (<= 0 i 9))
      (assoc s :onetouch (assoc-in (onetouch-table s) [col i] v))
      (assoc s :msg (str "値は 0〜" mx " です")))))

(defn reset-onetouch
  "ワンタッチ表を既定に戻す (col と i を渡すとその 1 つ。stgt_init / stgt_init2)"
  ([s] (dissoc s :onetouch))
  ([s col i] (assoc s :onetouch (assoc-in (onetouch-table s) [col i] (get-in onetouch [col i])))))

(defn one-touch
  "ワンタッチ ST/GT/VEL (EDIT.C ed_ch_stgt)。カーソル行のノートに表の n 番 (0〜9) を入れる。
   cx 0・1 → ST と GT、2 → GT、3 → VEL。chord? なら直前の和音の音 (ST=0) の GT/VEL もそろえる。
   そのあと、欄 cx の値が 0 でない次のノートへカーソルを進める (なければ END)。undo 1回分。"
  [s cx n chord?]
  (let [recs (cur-recs s) p (:pos s) r (get recs p)]
    (if-not (and r (< (r 0) 0x80))
      s
      (let [{:keys [st gt vel]} (onetouch-table s)
            main  (fn [r] (case (int cx) (0 1) (assoc r 1 (st n) 2 (gt n)) 2 (assoc r 2 (gt n)) 3 (assoc r 3 (vel n))))
            chord (fn [r] (if (= cx 3) (assoc r 3 (vel n)) (assoc r 2 (gt n))))
            recs  (assoc recs p (main r))
            recs  (if chord?
                    (loop [j (dec p) rs recs]
                      (let [x (get rs j)]
                        (if (and x (< (x 0) 0x80) (zero? (x 1)) (pos? (x 2)) (pos? (x 3)))
                          (recur (dec j) (assoc rs j (chord x)))
                          rs)))
                    recs)
            nxt   (or (some (fn [j] (let [x (recs j)] (when (and (< (x 0) 0x80) (pos? (x cx))) j)))
                            (range (inc p) (count recs)))
                      (count recs))]
        (-> s push-undo (set-recs recs) (assoc :pos nxt))))))

(defn insert-line
  "カーソル行に1行挿入し、カーソルを1つ下へ。小節の数が変わるときは後ろの SAME の参照をずらす
   (polysted.edit/insert-line)。undo 1回分"
  [s rec]
  (-> s push-undo
      (set-recs (polysted.edit/insert-line (cur-recs s) (:pos s) rec))
      (update :pos inc)))

(defn insert-measure
  "カーソル行に小節線を挿入 (原作の '*')"
  [s]
  (insert-line s [0xFD 0 0 0]))

(defn insert-repeat-start
  "カーソル行にリピート開始を挿入 (原作の '[')"
  [s]
  (insert-line s [0xF9 0 0 0]))

(defn repeat-count-error
  "リピート回数 n が使えなければ理由、使えれば nil"
  [n]
  (when-not (and (integer? n) (<= 0 n 255))
    "回数は 0〜255 です (0 と 255 は無限ループ)"))

(defn insert-repeat-end
  "カーソル行にリピート終了を挿入 (原作の ']')。n: 合計の演奏回数。使えない回数なら :msg を出して何もしない"
  [s n]
  (if-let [err (repeat-count-error n)]
    (assoc s :msg err)
    (insert-line s [0xF8 n 0 0])))

(defn same-error
  "row 番の SAME が小節 j (1始まり) を指せなければ理由、指せれば nil。
   原作: 1〜1024 で、その SAME より前の小節 (meas_no)。
   参照先の小節が SAME で始まるものも不可 (原作は数値の上書き時だけ確認)【設計判断】"
  [recs row j]
  (cond
    (not (and (integer? j) (<= 1 j 1024))) "指定出来ない小節です (1〜1024)"
    (>= j (polysted.edit/meas-no recs row))    "指定出来ない小節です (この行より前の小節を指定)"
    (= 0xFC (get-in recs [(polysted.edit/meas-add recs j) 0])) "指定出来ない小節です (SAME の小節は指定できない)"))

(defn set-same-ref [rec j]
  (let [m (dec j)] (assoc rec 1 (bit-and m 0xFF) 2 (bit-shift-right m 8))))

(defn insert-same
  "カーソル行に小節複写 SAME を挿入し、小節 j (1始まり) を指す (原作の '^')。指せなければ :msg を出して何もしない"
  [s j]
  (let [s' (insert-line s [0xFC 0 0 0])
        row (dec (:pos s'))]
    (if-let [err (same-error (cur-recs s') row j)]
      (assoc s :msg err)
      (set-rec s' row (set-same-ref [0xFC 0 0 0] j)))))

;; ------------------------------------------------------------
;; コントローラ (スペシャルコントローラ) の入力と表示
;; EDIT.C spcon・trk_dis、EDITS.C spc_code・ctrl_type。音色番号は STED2.CNF 既定の #prog_min=1 (1〜128)
;; ------------------------------------------------------------
(defn- prog-before "idx より前の最後の音色 (0xEC/0xE2 の byte2)。無ければ 0 (prog_no)" [recs idx]
  (or (some #(when (#{0xEC 0xE2} (% 0)) (% 2)) (rseq (subvec recs 0 idx))) 0))

(defn- channel-at "idx の時点の MIDI チャンネル (直前の 0xE6、無ければトラックの ch)" [s idx]
  (or (some #(when (= 0xE6 (% 0)) (% 2)) (rseq (subvec (cur-recs s) 0 idx)))
      (or (:ch (cur-track s)) 0)))

(defn ctrl-init-rec
  "記号を決めた直後のコントローラ行 (spcon の初期値)。ST は 0。
   テンポ 0xE7 の初期値は 64 (=100%) とする【設計判断。原作は 0 → 1 に丸める】"
  [s row {:keys [code ctl]}]
  (let [recs (cur-recs s)
        ctl  (if (and (#{0xE2 0xEC} code) (pos? ctl)) (dec ctl) ctl)   ; 音色は 1 始まりで入力
        b2   (case (int code)
               (0xE2 0xEC) (prog-before recs row)
               0xEB 128                                  ; 番号未定 (入力させる)
               0xE6 (channel-at s row)
               0xE7 64
               0)
        b2   (if (>= ctl 0) (min 127 ctl) b2)]
    (case (int code)
      0xEE [0xEE 0 0 64]
      [code 0 b2 0])))

(defn set-exclusive
  "row のトラックエクスクルーシブ (0x98) のデータを bytes (F7 で終わる) に置き換える (続く 0xF7 の行を作り直す。undo は積まない)"
  [s row bytes]
  (let [recs  (cur-recs s)
        [_ j] (polysted.rcp/track-exclusive-data recs row)
        d     (if (odd? (count bytes)) (conj (vec bytes) 0xF7) (vec bytes))
        block (mapv (fn [[x y]] [0xF7 0 x y]) (partition 2 d))]
    (set-recs s (vec (concat (subvec recs 0 (inc row)) block (subvec recs j))))))

(defn edit-exclusive
  "原作の _ キー: カーソル行が Tr.Exclu ならそのデータを書き換え、それ以外ならカーソル位置に
   新しいトラックエクスクルーシブ (ST 0) を挿入する。text は parse-exc-data の書式。undo 1回分"
  [s text]
  (if-let [bytes (parse-exc-data text)]
    (let [r (get (cur-recs s) (:pos s))]
      (if (and r (= 0x98 (r 0)))
        (-> s push-undo (set-exclusive (:pos s) bytes))
        (let [s1 (insert-line s [0x98 0 0 0]) row (dec (:pos s1))]
          (-> s1 (set-exclusive row bytes) (assoc :pos row)))))
    (assoc s :msg "エクスクルーシブのデータが不正です (16進 00〜7F と gt ve ch cs ss)")))

(defn exclusive-text
  "row のトラックエクスクルーシブのデータを入力の書式で (編集の初期値)"
  [s row]
  (exc-hex (first (polysted.rcp/track-exclusive-data (cur-recs s) row))))

(defn set-user-exc
  "ヘッダのユーザーエクスクルーシブ n (0〜7) のデータを設定する (24 バイトまで、残りは F7 で埋める)"
  [s n text]
  (let [bytes (parse-exc-data text)]
    (cond
      (not (and (integer? n) (<= 0 n 7))) (assoc s :msg "ユーザーエクスクルーシブの番号は 0〜7 です")
      (nil? bytes) (assoc s :msg "エクスクルーシブのデータが不正です (16進 00〜7F と gt ve ch cs ss)")
      (> (count bytes) 24) (assoc s :msg "ユーザーエクスクルーシブは F7 を含めて 24 バイトまでです")
      :else (let [data (vec (take 24 (concat bytes (repeat 0xF7))))
                  ue (vec (take 8 (concat (or (get-in s [:song :user-exc]) []) (repeat ["" (vec (repeat 24 0))]))))]
              (-> s push-undo
                  (assoc-in [:song :user-exc] (assoc-in ue [n 1] data))
                  (assoc :msg (str "ユーザーエクスクルーシブ " n " を設定しました: " (exc-hex bytes))))))))

;; ---- ユーザーエクスクルーシブの表の編集 (原作の USER EXC 画面、EXCLU.C user_exc_as) ----
(defn uex-table "ユーザーエクスクルーシブの表 (8 × [名前 24 バイトのデータ])。空はデータが全部 F7" [s]
  (mapv (fn [i] (let [[nm d] (get-in s [:song :user-exc i] ["" []])] [(or nm "") (uex-data d)])) (range 8)))

(defn- set-uex-table [s ue] (if (= ue (uex-table s)) s (-> s push-undo (assoc-in [:song :user-exc] ue))))

(defn uex-name "表 n の名前 (24 文字まで)。undo 1回分" [s n nm]
  (set-uex-table s (assoc-in (uex-table s) [n 0] (let [t (str/trim (str nm))] (subs t 0 (min 24 (count t)))))))

(defn uex-op
  "表 n のデータの x 番目を変える。op: :set v (上書き) :ins v (挿入) :del (削除) :sum (チェックサムを挿入。x>5)
   :end (そこで終わり = F7) :dup (同じ値を挿入) :step v (±v、7 ビットで回す。特殊コードと F7 は変えない)。undo 1回分"
  [s n op x v]
  (let [ue (uex-table s) d (second (ue n))
        d2 (case op
             :set (assoc d x v) :ins (uex-ins d x v) :del (uex-del d x) :end (assoc d x 0xF7)
             :sum (if (> x 5) (uex-sum d x) d)
             :dup (uex-ins d x (d x))
             :step (if (< (d x) 128) (assoc d x (bit-and (+ (d x) v) 127)) d))]
    (set-uex-table s (assoc-in ue [n 1] (uex-data d2)))))

(defn uex-copy "表 from を表 n へ写す (名前も。原作の U)。undo 1回分" [s n from]
  (if (and (integer? from) (<= 0 from 7))
    (let [ue (uex-table s)] (set-uex-table s (assoc ue n (ue from))))
    (assoc s :msg "番号は 0〜7 です")))

(defn uex-reset "表 n (nil なら全部) を空に (原作は定義ファイルの既定。既定は空)。undo 1回分" [s n]
  (let [e ["" (vec (repeat 24 0xF7))]]
    (set-uex-table s (if n (assoc (uex-table s) n e) (vec (repeat 8 e))))))

(defn uex-load "表を丸ごと置き換える (.REX の読み込み)。undo 1回分" [s ue]
  (-> (set-uex-table s (mapv (fn [[nm d]] [nm (uex-data d)]) ue)) (assoc :msg "ユーザーエクスクルーシブを読み込みました")))

(defn uex-load-entry "表 n を置き換える (.RES の読み込み)。undo 1回分" [s n [nm d]]
  (-> (set-uex-table s (assoc (uex-table s) n [nm (uex-data d)])) (assoc :msg (str "ユーザーエクスクルーシブ " n " を読み込みました"))))

;; ---- エクスクルーシブのテキストファイル .EXC (EXCLU.C exc_load / exc_save) ----
(defn exc-messages-at "カーソル行のトラックエクスクルーシブのデータ (F7 を除く) を 1 メッセージの列で。0x98 の行でなければ nil" [s]
  (let [r (get (cur-recs s) (:pos s))]
    (when (and r (= 0x98 (r 0)))
      [(vec (take-while #(not= % 0xF7) (first (polysted.rcp/track-exclusive-data (cur-recs s) (:pos s)))))])))

(defn load-exc-messages
  "EXC ファイルのメッセージをトラックエクスクルーシブにする: カーソルが Tr.Exclu ならそれを 1 つ目で置き換え、
   それ以外なら挿入。2 つ目からは続けて新しい 0x98 の行に (原作は F0 を含めて 1 つのデータにし、F0 で行を分ける)。undo 1回分"
  [s msgs]
  (if (empty? msgs)
    (assoc s :msg "エクスクルーシブのデータがありません (F0 で始まるメッセージ)")
    (let [recs (cur-recs s) p (:pos s) r (get recs p)
          block (fn [head m] (let [d (conj (vec m) 0xF7) d (if (odd? (count d)) (conj d 0xF7) d)]
                               (into [head] (map (fn [[x y]] [0xF7 0 x y]) (partition 2 d)))))
          [head end] (if (and r (= 0x98 (r 0))) [r (second (polysted.rcp/track-exclusive-data recs p))] [[0x98 0 0 0] p])
          blocks (into (block head (first msgs)) (mapcat #(block [0x98 0 0 0] %) (rest msgs)))]
      (-> s push-undo
          (set-recs (vec (concat (subvec recs 0 p) blocks (subvec recs end))))
          (assoc :msg (str "エクスクルーシブを " (count msgs) " 個読み込みました"))))))

(defn insert-ctrl
  "カーソル行にコントローラを挿入する (REPL 用。TUI の '/' と同じ値になる)。
   (insert-ctrl s \"V\" 0 100) → 音量 100。params は ST のあとの欄に順に入る。
   不正なら :msg を出して何もしない"
  [s sym & params]
  (if-let [{:keys [code] :as sc} (spc-code sym)]
    (cond
      (= code 0x98) (assoc s :msg "トラックエクスクルーシブは edit-exclusive (TUI の _ キー) で入力します")
      (not (supported-ctrls code)) (assoc s :msg (str "未対応のコントローラです: " sym))
      :else
      (let [row (:pos s)
            rec (ctrl-init-rec s row sc)
            rec (reduce (fn [r [[cx t] v]] (when r (set-ctrl-field r cx t (str v))))
                        rec (map vector (ctrl-insert-fields sc) params))]
        (if rec
          (insert-line s rec)
          (assoc s :msg (str "コントローラの値が不正です: " sym " " (str/join " " params))))))
    (assoc s :msg (str "コントローラの記号が不正です: " sym))))

(defn optimize [s]
  (let [t (cur-track s)
        recs (polysted.edit/optimize-records (:recs t) (or (:ch t) 0))]
    (-> s push-undo (set-recs recs) (goto-pos (:pos s)))))

(defn- clamp-range [s from to]
  (let [n (count (cur-recs s))
        lo (-> (min from to) (max 0) (min n))
        hi (-> (max from to) (max 0) (min n))]
    [lo hi]))

(defn copy
  "[from, to) をクリップボードに複写 (F8)。SAME は中身に展開する"
  [s from to]
  (let [[lo hi] (clamp-range s from to)]
    (if (< lo hi)
      (assoc s :clip (polysted.edit/expand-same-range (cur-recs s) lo hi))
      s)))

(defn cut
  "[from, to) を削除してクリップボードに移す (F7)。カーソルは範囲の先頭へ。undo 1回分"
  [s from to]
  (let [[lo hi] (clamp-range s from to)]
    (if (< lo hi)
      (let [{:keys [recs buf]} (polysted.edit/delete-range (cur-recs s) lo hi)]
        (-> s push-undo (set-recs recs) (assoc :clip buf) (goto-pos lo)))
      s)))

(defn paste
  "クリップボードをカーソル位置に挿入 (F9)。後ろの SAME の参照をずらす。
   カーソルは貼り付けた範囲の後ろへ (REPL 向け。TUI は原作どおり先頭に戻す)"
  [s]
  (let [clip (:clip s)]
    (if (empty? clip)
      s
      (-> s push-undo
          (set-recs (polysted.edit/paste-records (cur-recs s) (:pos s) clip))
          (update :pos + (count clip))))))

(defn paste-reversed
  "クリップボードを前後反転してカーソル位置に挿入 (Shift+F9)。カーソルは paste と同じく後ろへ"
  [s]
  (let [clip (:clip s)]
    (if (empty? clip)
      s
      (let [rev (polysted.edit/reverse-records clip)]
        (-> s push-undo
            (set-recs (polysted.edit/paste-records (cur-recs s) (:pos s) rev))
            (update :pos + (count rev)))))))

(defn mix-paste
  "カーソル位置からのデータにクリップボードを時刻順に混ぜる (Shift+F8)。カーソルは動かさない"
  [s]
  (let [clip (:clip s) recs (cur-recs s) p (:pos s)]
    (if (empty? clip)
      s
      (-> s push-undo
          (set-recs (polysted.edit/renumber-measures
                     (into (subvec recs 0 p) (polysted.edit/mix-records (subvec recs p) clip))))))))

(defn quantize
  "[from, to) をクオンタイズ (原作の Q)。res: 解像度 2〜48、rate: 50〜100 (%)。範囲外の値なら :msg"
  [s from to res rate]
  (cond
    (not (and (integer? res) (<= 2 res 48)))    (assoc s :msg "解像度は 2〜48 です")
    (not (and (integer? rate) (<= 50 rate 100))) (assoc s :msg "レートは 50〜100 (%) です")
    :else
    (let [[lo hi] (clamp-range s from to)]
      (-> s push-undo (set-recs (polysted.edit/quantize (cur-recs s) lo hi res rate))))))

(def note-ctrl-modes
  "Shift+F1 のモード文字 → キーワード (TRACK.C note_ctrl)"
  {\L :line \C :chord \R :random \O :offset \T :stgt \U :sort-up \D :sort-down \G :strum-gt \S :strum-st})

(defn note-ctrl-error
  "値の検査 (velo_ctrl・note_ctrl の入力範囲)。問題なければ nil"
  [mode a b]
  (case mode
    (:line :chord) (when-not (and a b (<= 0 a 127) (<= 0 b 127)) "開始・終了は 0〜127 です")
    :random        (when-not (and a b (<= 0 a 127) (<= a b 127)) "下限・上限は 0〜127 (下限 ≤ 上限) です")
    (:offset :stgt) (when-not (and a b (<= -127 a 127) (<= 0 b 127) (<= a b)) "下限は -127〜127、上限は 0〜127 (下限 ≤ 上限) です")
    (:strum-gt :strum-st) (when-not (and a (<= 1 a 256)) "ずらす ST は 1〜256 です")
    (:sort-up :sort-down) nil
    "モードは L C R O T U D G S のどれかです"))

(defn note-ctrl
  "[from, to) のノートを一括操作 (原作の Shift+F1)。mode は note-ctrl-modes の値、a b は数値。
   rng を省略すると乱数は毎回変わる"
  ([s from to mode a b] (note-ctrl s from to mode a b (java.util.Random.)))
  ([s from to mode a b rng]
   (if-let [err (note-ctrl-error mode a b)]
     (assoc s :msg err)
     (let [[lo hi] (clamp-range s from to)
           recs (cur-recs s)
           recs' (case mode
                   (:sort-up :sort-down) (polysted.edit/sort-chords recs lo hi (= mode :sort-up))
                   (:strum-gt :strum-st) (polysted.edit/strum-chords recs lo hi a (= mode :strum-gt))
                   (polysted.edit/velocity-ctrl recs lo hi mode a b rng))]
       (if (= recs recs') s (-> s push-undo (set-recs recs')))))))

(defn- replace-opts [] {:spc-code #'spc-code :prog-min 1})

(defn replace-range
  "[from, to) に置換 (mode :replace、原作の F3) か指定削除 (:delete、Shift+F3) を当てる。
   指定は polysted.replace の書式 (例 \"C4:D#4\" \",,,:+10\" \"/V:/E\")。指定は :repl / :delt に覚える"
  [s from to text mode]
  (let [[lo hi] (clamp-range s from to)
        s (assoc s (if (= mode :delete) :delt :repl) text)
        {:keys [recs hits error]} (polysted.replace/apply-specs (cur-recs s) lo hi text mode (replace-opts))]
    (cond
      error (assoc s :msg error)
      (zero? hits) (assoc s :msg "該当するデータはありません")
      :else (-> s push-undo (set-recs (polysted.edit/renumber-measures recs))
                (assoc :msg (str hits " か所を" (if (= mode :delete) "削除" "置換") "しました"))))))

(defn find-next
  "検索 (原作の F4/F5)。text を省略すると前回の指定。forward? false で前へ"
  ([s forward?] (find-next s (:srch s) forward?))
  ([s text forward?]
   (if (str/blank? text)
     (assoc s :msg "検索の指定がありません (F4 / Ctrl-F で指定)")
     (let [s (assoc s :srch text)
           r (polysted.replace/find-match (cur-recs s) (:pos s) text forward? (replace-opts))]
       (cond
         (map? r) (assoc s :msg (:error r))
         r (assoc s :pos r)
         :else (assoc s :msg "見つかりません"))))))

(declare measure-st track-pos-at)

(defn- keep-position
  "f で曲を変えたあと、カーソルを変更前と同じ小節・同じ時刻へ (原作の meas_add + sttm_ad)"
  [s f]
  (let [recs (cur-recs s) p (:pos s)
        meas (polysted.edit/meas-no recs p) st (measure-st recs p)
        s2 (f s)]
    (assoc s2 :pos (min (track-pos-at (cur-recs s2) meas st) (count (cur-recs s2))))))

(defn same-convert
  "重複した小節を SAME に置き換える (原作の Shift+^)"
  [s]
  (let [{:keys [recs count]} (polysted.edit/same-meas-conv (cur-recs s))]
    (if (zero? count)
      (assoc s :msg "重複した小節はありません")
      (-> s push-undo (keep-position #(set-recs % recs))
          (assoc :msg (str count " 小節を SAME に変換しました"))))))

(defn same-extract
  "[from, to) の SAME を中身に展開する (原作の Ctrl+^)。範囲を省略するとトラック全体"
  ([s] (same-extract s 0 (count (cur-recs s))))
  ([s from to]
   (let [[lo hi] (clamp-range s from to)
         n (count (filter #(= 0xFC (% 0)) (subvec (cur-recs s) lo hi)))]
     (if (zero? n)
       (assoc s :msg "展開する SAME はありません")
       (-> s push-undo (keep-position #(set-recs % (polysted.edit/same-meas-extract (cur-recs %) lo hi)))
           (assoc :msg (str n " か所の SAME を展開しました")))))))

(defn repeat-compress
  "繰り返しをリピートに圧縮する (原作のトラック設定の Repeat Compress)。all? なら全トラック"
  ([s] (repeat-compress s false))
  ([s all?]
   (let [ts (get-in s [:song :tracks])
         res (mapv (fn [i t] (if (or all? (= i (:tr s))) (polysted.edit/repeat-compress (:recs t)) {:recs (:recs t) :count 0}))
                   (range) ts)
         n (reduce + (map :count res))]
     (if (zero? n)
       (assoc s :msg "圧縮できる繰り返しはありません")
       (-> s push-undo
           (assoc-in [:song :tracks] (mapv (fn [t r] (assoc t :recs (:recs r))) ts res))
           (goto-pos 0)
           (assoc :msg (str n " か所をリピートに圧縮しました")))))))

(defn same-convert-all
  "全トラックで重複した小節を SAME に置き換える (原作のトラック設定の A)"
  [s]
  (let [ts (get-in s [:song :tracks])
        res (mapv #(if (seq (:recs %)) (polysted.edit/same-meas-conv (:recs %)) {:recs (:recs %) :count 0}) ts)
        n (reduce + (map :count res))]
    (if (zero? n)
      (assoc s :msg "重複した小節はありません")
      (-> s push-undo
          (assoc-in [:song :tracks] (mapv (fn [t r] (assoc t :recs (:recs r))) ts res))
          (goto-pos 0)
          (assoc :msg (str "全トラックで " n " 小節を SAME に変換しました"))))))

(defn- replicate-error [top end times bias]
  (cond
    (not (and top (>= top 1)))           "開始小節は 1 以上です"
    (not (and end (>= end top)))         "終了小節は開始小節以上です"
    (not (and times (<= 1 times 255)))   "回数は 1〜255 です"
    (not (and bias (<= -24 bias 24)))    "キーシフトは -24〜24 です"))

(defn replicate
  "トラック src (0始まり) の小節 top〜end を、カーソル位置に times 回、キーを bias ずらして挿入 (原作の R)。
   SAME は展開してコピーし、クリップボードにも入れる"
  [s src top end times bias]
  (let [ts (get-in s [:song :tracks])]
    (if-let [err (or (when-not (and src (< -1 src (count ts))) (str "トラックは 1〜" (count ts) " です"))
                     (replicate-error top end times bias))]
      (assoc s :msg err)
      (let [buf (polysted.edit/shift-keys (polysted.edit/measure-range-buffer (:recs (ts src)) top end) bias)]
        (if (empty? buf)
          (assoc s :msg "コピー元の小節にデータがありません")
          (-> s push-undo (assoc :clip buf)
              (set-recs (polysted.edit/renumber-measures (polysted.edit/insert-times (cur-recs s) (:pos s) buf times)))
              (assoc :msg (str "TR" (inc src) " の小節 " top "〜" end " を " times " 回コピーしました"))))))))

(defn replicate-all
  "全トラックで、小節 top〜end をカーソルのある小節の先頭へ times 回コピー (原作の Shift+R)。空のトラックは飛ばす"
  [s top end times bias]
  (if-let [err (replicate-error top end times bias)]
    (assoc s :msg err)
    (let [dst (polysted.edit/meas-no (cur-recs s) (:pos s))
          s1  (push-undo s)
          tracks (mapv (fn [t]
                         (let [recs (:recs t)
                               buf  (polysted.edit/shift-keys (polysted.edit/measure-range-buffer recs top end) bias)]
                           (if (or (empty? recs) (empty? buf))
                             t
                             (let [ad (or (polysted.edit/meas-add recs dst) (count recs))]
                               (assoc t :recs (polysted.edit/renumber-measures (polysted.edit/insert-times recs ad buf times)))))))
                       (get-in s [:song :tracks]))]
      (-> s1 (assoc-in [:song :tracks] tracks) (goto-pos (:pos s))
          (assoc :msg (str "全トラックの小節 " top "〜" end " を " times " 回コピーしました"))))))

(defn replicate-range
  "範囲 [from, to) の内容 (SAME は展開、最後に小節線) を、トラック dst (0始まり、nil なら全トラックの ch が OFF でないもの) の
   小節 top の先頭へ times 回、キーを bias ずらして挿入 (原作の T / Shift+T)"
  [s from to dst top times bias]
  (let [[lo hi] (clamp-range s from to)
        ts (get-in s [:song :tracks])]
    (if-let [err (or (when (>= lo hi) "先に v (F6) で範囲を指定してください")
                     (when (and dst (not (< -1 dst (count ts)))) (str "トラックは 1〜" (count ts) " です"))
                     (replicate-error top top times bias))]
      (assoc s :msg err)
      (let [buf (polysted.edit/expand-same-range (cur-recs s) lo hi)
            buf (polysted.edit/shift-keys (if (not= ((peek buf) 0) 0xFD) (conj buf [0xFD 0 0 0]) buf) bias)
            put (fn [t] (let [recs (:recs t) ad (or (polysted.edit/meas-add recs top) (count recs))]
                          (assoc t :recs (polysted.edit/renumber-measures (polysted.edit/insert-times recs ad buf times)))))
            tracks (vec (map-indexed (fn [i t] (if (if dst (= i dst) (:ch t)) (put t) t)) ts))]
        (-> s push-undo (assoc :clip buf) (assoc-in [:song :tracks] tracks) (goto-pos (:pos s))
            (assoc :msg (str "範囲を " (if dst (str "TR" (inc dst)) "全トラック") " の小節 " top " へ " times " 回コピーしました")))))))

;; ------------------------------------------------------------
;; リズムパターン編集 (原作 REDIT.C。変換は polysted.rhythm)
;; ------------------------------------------------------------
(declare key->name)
(defn- ss-key-name [k] (if (zero? k) "OFF" (key->name k)))

(defn rhythm-assign "使うリズム割り当て表 (空なら SC55.DEF の既定)" [s]
  (polysted.rhythm/effective-assign (get-in s [:song :rhythm-assign])))

(defn bar-st "1小節の ST (タイムベース × 4 × 拍子の分子 ÷ 分母、REDIT.C の barstep)" [s]
  (let [tb (or (get-in s [:song :timebase]) 48) [a b] (or (get-in s [:song :time-sig]) [4 4])]
    (quot (* tb 4 a) b)))

(defn rhythm-pattern "今のトラックの小節 meas のパターン" [s meas]
  (polysted.rhythm/measure->pattern (cur-recs s) meas (rhythm-assign s) (bar-st s) (or (:rhy-step s) 12)))

(defn measure-count "今のトラックの小節の数 (末尾の空の小節を含む)" [s]
  (let [recs (cur-recs s)] (polysted.edit/meas-no recs (count recs))))

(defn rhythm-update
  "小節 meas のパターンを f で変えて書き戻す (undo 1回分)。SAME の小節・ステップが多すぎる小節は :msg"
  [s meas f]
  (let [p (rhythm-pattern s meas)]
    (cond
      (:same? p) (assoc s :msg "SAME の小節はパターン編集できません")
      (:too-many? p) (assoc s :msg "この小節はステップが多すぎます。編集できません")
      :else
      (let [as (rhythm-assign s)
            recs (polysted.rhythm/replace-measure (cur-recs s) meas (polysted.rhythm/pattern->records (f p) as))]
        (-> s push-undo (set-recs recs)
            ;; 既定の割り当て表を使ったときは曲にも記録する (RCP 保存で残るように)
            (cond-> (not (some #(pos? (second %)) (get-in s [:song :rhythm-assign])))
              (assoc-in [:song :rhythm-assign] as)))))))

(defn set-rhythm-assign
  "リズム割り当て表の楽器 idx (0〜31) を設定する (原作の RHY ASSI)。キーが変わったら、RHY が ON の全トラックで
   元のキーの鳴る音を新しいキーに書き換える (ASSIGN.C rhy_as。新しいキーが 0 なら書き換えない)。undo 1回分"
  [s idx text]
  (if-let [[key gt nm] (parse-rhythm-assign text)]
    (if-not (<= 0 idx 31)
      (assoc s :msg "楽器の番号は 1〜32 です")
      (let [old (rhythm-assign s)
            table (assoc old idx [(subs nm 0 (min 14 (count nm))) key gt])
            ;; 元のキーごとに、最初に一致する楽器 n の新しいキー (ASSIGN.C の rno[n] の照合と同じ)
            first-n (into {} (for [n (range 31 -1 -1) :let [ok (second (old n))] :when (pos? ok)] [ok n]))
            remap (into {} (for [[ok n] first-n :let [nk (second (table n))] :when (and (pos? nk) (not= ok nk))] [ok nk]))
            tracks (mapv (fn [t] (if (:rhythm? t)
                                   (update t :recs (fn [rs] (mapv (fn [[a b c d :as r]]
                                                                    (if (and (< 0 a 0x80) (pos? c) (pos? d) (remap a)) [(remap a) b c d] r))
                                                                  rs)))
                                   t))
                         (get-in s [:song :tracks]))]
        (-> s push-undo
            (assoc-in [:song :rhythm-assign] table)
            (assoc-in [:song :tracks] tracks)
            (assoc :msg (str "楽器 " (inc idx) " を " (ss-key-name key) " GT " gt " に設定しました")))))
    (assoc s :msg "割り当ては「キー GT 名前」で入力してください (例 36 1 Acou BD、キー 0 は無し)")))

(defn rhythm-set-hit "パターンのステップ cx・楽器 idx を [vel gt] に (vel 0 で消す)。コマンドのステップは変えない"
  [p cx idx vel gt]
  (if (= :raw (get-in p [:steps cx :kind]))
    p
    (update-in p [:steps cx :hits] #(if (pos? vel) (assoc % idx [vel gt]) (dissoc % idx)))))

(defn rhythm-new-measure
  "トラックの末尾に空の小節を足す (原作の [登録])。step-st を指定すると1ステップの ST も変える (6〜96, 192)。
   => 新しい小節の番号は :rhy-meas"
  ([s] (rhythm-new-measure s nil))
  ([s step-st]
   (if (and step-st (not (or (<= 6 step-st 96) (= step-st 192))))
     (assoc s :msg "ステップの ST は 6〜96 か 192 です")
     (let [recs (polysted.edit/renumber-measures (conj (cur-recs s) [0xFD 0 0 0]))]
       (-> s push-undo (set-recs recs)
           (cond-> step-st (assoc :rhy-step step-st))
           (assoc :rhy-meas (polysted.edit/meas-no recs (dec (count recs)))))))))

;; ---- リズム画面の小節の切り取り・複写・貼り付け (REDIT.C の F7 / F8 / F9) ----
(defn- rhythm-measure-range
  "小節 meas の行の範囲 [lo hi) (小節線か SAME まで、END は含まない。meas_len)。小節が無ければ nil"
  [recs meas]
  (when-let [lo (polysted.edit/meas-add recs meas)]
    (let [n (count recs)]
      [lo (loop [j lo] (cond (>= j n) n (= 0xFE ((recs j) 0)) j (> ((recs j) 0) 0xFB) (inc j) :else (recur (inc j))))])))

(defn- close-bar "小節線で終わっていなければ足す" [buf]
  (if (and (seq buf) (< ((peek buf) 0) 0xFC)) (conj buf [0xFD 0 0 0]) buf))

(defn rhythm-copy-measure
  "小節 meas をリズム画面のバッファ (:rhy-clip) へ写す (原作の F8)。SAME の小節は参照先の中身にして写す【設計判断】"
  [s meas]
  (let [recs (cur-recs s) [lo hi] (rhythm-measure-range recs meas)]
    (if (and lo (< lo hi))
      (assoc s :rhy-clip (close-bar (polysted.edit/expand-same-range recs lo hi)) :msg (str "小節 " meas " を複写しました"))
      (assoc s :msg "空の小節です"))))

(defn rhythm-cut-measure
  "小節 meas を切り取ってバッファへ (原作の F7)。後ろの SAME の参照を直し、消える小節を指す SAME は中身に展開する。undo 1回分"
  [s meas]
  (let [recs (cur-recs s) [lo hi] (rhythm-measure-range recs meas)]
    (if (and lo (< lo hi))
      (let [{r :recs buf :buf} (polysted.edit/delete-range recs lo hi)]
        (-> s push-undo (set-recs r) (assoc :rhy-clip (close-bar buf) :msg (str "小節 " meas " を切り取りました"))))
      (assoc s :msg "空の小節です"))))

(defn rhythm-paste-measure
  "バッファの小節を小節 meas の前に挿入する (原作の F9)。後ろの SAME の参照をずらす。undo 1回分"
  [s meas]
  (if-let [clip (:rhy-clip s)]
    (let [recs (cur-recs s) lo (or (polysted.edit/meas-add recs meas) (count recs))]
      (-> s push-undo (set-recs (polysted.edit/paste-records recs lo clip)) (assoc :msg (str "小節 " meas " の前に貼り付けました"))))
    (assoc s :msg "複写した小節がありません (F8 / y で複写、F7 / x で切り取り)")))

(defn value-step
  "カーソル行の欄 cx の値を増減・反転する (原作の + . > / - , < / =)。op: :inc :inc-big :dec :dec-big :rev。undo 1回分"
  [s cx op]
  (let [recs (cur-recs s) p (:pos s) r (get recs p)]
    (if-not r
      s
      (let [r2 (case op
                 :rev (polysted.edit/val-rev r cx)
                 (polysted.edit/val-step r cx (if (#{:inc :inc-big} op) 1 -1) (#{:inc-big :dec-big} op)
                                     (polysted.edit/meas-no recs p)))]
        (replace-rec s p r2)))))

(defn chord-target
  "コード入力の対象の行: カーソル行がノートならそこ、小節線・END の上なら1行前のノート (原作の X)。無ければ nil"
  [s]
  (let [recs (cur-recs s) p (:pos s) r (get recs p) prev (when (pos? p) (recs (dec p)))]
    (cond
      (and r (< (r 0) 0x80)) p
      (and (or (nil? r) (>= (r 0) 0xFC)) prev (< (prev 0) 0x80)) (dec p))))

(defn insert-chord
  "row のノートをコード (構成音 ks) に展開する。カーソルはコードの最後の音へ。undo 1回分"
  [s row ks]
  (let [recs (cur-recs s) r (recs row)]
    (-> s push-undo
        (set-recs (vec (concat (subvec recs 0 row) (polysted.edit/chord-records r ks) (subvec recs (inc row)))))
        (assoc :pos (+ row (dec (count ks)))))))

(defn roll-edit
  "ピアノロールでの編集 (今のトラック、undo 1回分。カーソルは編集した音の行へ)。
   op: :key [idx キー] / :gt [idx GT] / :move [idx tick キー] / :add [tick キー GT VEL] / :delete [idx]。
   規則は src/polysted/roll.clj。できないときは :msg"
  [s op args]
  (let [recs (cur-recs s)
        note! (fn [i] (when-not (polysted.roll/note-rec? (get recs i)) {:error "音の行ではありません"}))
        res (case op
              :key    (let [[i k] args] (or (note! i) (if (<= 0 k 127) {:recs (assoc-in recs [i 0] k) :idx i} {:error "キーは 0〜127 です"})))
              :gt     (let [[i g] args] (or (note! i) (if (<= 1 g 255) {:recs (assoc-in recs [i 2] g) :idx i} {:error "GT は 1〜255 です"})))
              :move   (let [[i t k] args] (if (<= 0 k 127) (polysted.roll/move-note recs i (max 0 t) k) {:error "キーは 0〜127 です"}))
              :add    (let [[t k g v] args] (polysted.roll/insert-note recs (max 0 t) (-> k (max 0) (min 127)) (-> g (max 1) (min 255)) (-> v (max 1) (min 127))))
              :delete (let [[i] args] (or (note! i) {:recs (polysted.roll/remove-note recs i) :idx (min i (dec (count recs)))})))]
    (cond
      (:error res)         (assoc s :msg (:error res))
      (= (:recs res) recs) s
      :else (-> s push-undo (set-recs (polysted.edit/renumber-measures (:recs res))) (assoc :pos (:idx res))))))

(defn bar-length "1小節の ST (タイムベース × 4 × 拍子の分子 ÷ 分母)" [s]
  (let [tb (or (get-in s [:song :timebase]) 48) [a b] (or (get-in s [:song :time-sig]) [4 4])]
    (quot (* tb 4 a) (max 1 b))))

(defn visual-commit
  "コントローラのビジュアルエディットの確定 (原作 vis_edit の Enter。undo 1回分)。
   v: {:vmo :vmo2 :para :step :from 範囲の先頭 (新規なら挿入位置) :len 範囲の行数 (0 なら新規)
       :ost 始めたときの合計 ST :changed? ステップを変えたか}。mix? は Shift+Enter。
   新規: 同じ値の続く点をつないで行を挿入し、カーソルはその後ろへ (END の位置なら小節線も入れる)。
         mix? (END 以外) なら既存のデータに時刻順に混ぜる。
   範囲: ステップを変えていなければ値だけ書き戻す、変えていれば混ぜる"
  [s {:keys [vmo para step from len ost changed?] :as v} mix?]
  (let [recs (cur-recs s)
        kind (select-keys v [:vmo :vmo2 :exc])
        pts (polysted.visual/link-points {:para para :step step})
        at-end? (let [r (get recs from)] (or (nil? r) (= 0xFE (r 0))))
        [new-recs pos]
        (cond
          (pos? len) [(if changed?
                        (polysted.visual/mix-in recs from pts kind ost)
                        (polysted.visual/overwrite recs from (+ from len) kind para))
                      from]
          (and mix? (not at-end?)) [(polysted.visual/mix-in recs from pts kind ost) from]
          :else
          (let [rs (if at-end?
                     (let [d (polysted.roll/roll-data recs) t (polysted.roll/cursor-tick d from)
                           bar (or (last (filter #(<= % t) (:bars d))) 0)]
                       (polysted.visual/points->records pts kind (- t bar) (bar-length s)))
                     (polysted.visual/points->records pts kind))]
            [(vec (concat (subvec recs 0 from) rs (subvec recs from))) (+ from (count rs))]))]
    (if (= new-recs recs)
      s
      (-> s push-undo (set-recs (polysted.edit/renumber-measures new-recs)) (assoc :pos (min pos (count new-recs)))
          (assoc :msg (str (polysted.visual/kind-name vmo) " を書き込みました"))))))

(defn duplicate-line
  "カーソル行を二重化する (F10)。カーソルは下 (元の行) へ"
  [s]
  (if-let [r (get (cur-recs s) (:pos s))]
    (insert-line s r)
    s))

(defn set-tempo [s bpm]
  (-> s push-undo (assoc-in [:song :tempo] bpm)))

(defn set-channel [s ch]
  (-> s push-undo (assoc-in [:song :tracks (:tr s) :ch] ch)))

;; ------------------------------------------------------------
;; トラック設定 (原作のメイン画面 TRK.SET。TRKSET.C の表示と入力範囲)
;; ------------------------------------------------------------
(defn track-stats
  "トラック設定の STEP (レコード数) と TOTAL ST (ST の合計。255/256 回のループは1回として数える)"
  [t]
  (let [evs (polysted.mml/expand-track {:events (polysted.edit/records->events (:recs t))})]
    {:steps (count (:recs t))
     :total-st (reduce + (map #(:st % 0) (:events evs)))}))

(defn set-song-field
  "曲の項目を文字列で設定する。field: :title :tempo :timebase :beat :key :pbias。不正なら :msg"
  [s field text]
  (let [t (str/trim (str text))
        n (when (re-matches #"-?\d+" t) (Long/parseLong t))
        ok (fn [k v] (-> s push-undo (assoc-in [:song k] v)))]
    (case field
      :title    (ok :title (str text))
      :tempo    (if (and n (<= 1 n 255)) (ok :tempo n) (assoc s :msg "テンポは 1〜255 です"))
      :timebase (if (and n (<= 1 n 9999)) (ok :timebase n) (assoc s :msg "タイムベースは 1〜9999 です"))
      :pbias    (if (and n (<= 0 n 255)) (ok :pbias n) (assoc s :msg "PLAYBIAS は 0〜255 です"))
      :beat     (if-let [[_ a b] (re-matches #"(\d+)\s*/\s*(\d+)" t)]
                  (let [a (Long/parseLong a) b (Long/parseLong b)]
                    (if (and (<= 2 b 16) (<= 1 a (* 2 b))) (ok :time-sig [a b]) (assoc s :msg "拍子は 1〜32 / 2〜16 です (例 3/4)")))
                  (assoc s :msg "拍子は 4/4 のように入力してください"))
      :key      (if-let [k (parse-key t)] (ok :key k) (assoc s :msg "調は C G D … や Am、または 0〜31 です")))))

(defn set-track-field
  "トラック tr (0始まり) の項目を設定する。field: :ch :st-shift :key-shift :comment は文字列、
   :mute? :rhy は切り替え、:mode-next は MODE を Play → Mute → Mix → Rec の順に (value は無視)。不正なら :msg"
  [s tr field text]
  (let [t (str/trim (str text))
        n (when (re-matches #"-?\d+" t) (Long/parseLong t))
        path [:song :tracks tr]
        ok (fn [f] (-> s push-undo (update-in path f)))]
    (case field
      :ch        (if-let [ch (parse-ch t)] (ok #(assoc % :ch (when (pos? ch) ch))) (assoc s :msg "チャンネルは A1〜B16 か 0〜32 (0=OFF) です"))
      :st-shift  (if (and n (<= -99 n 99)) (ok #(assoc % :st-shift (if (neg? n) (+ n 256) n))) (assoc s :msg "ST+ は -99〜99 です"))
      :key-shift (cond (= (str/upper-case t) "OFF") (ok #(assoc % :key-shift 128))
                       (and n (or (<= -64 n 63) (= n 128))) (ok #(assoc % :key-shift (if (neg? n) (+ n 128) n)))
                       :else (assoc s :msg "K#+ は -64〜63、128 か OFF です"))
      :comment   (ok #(assoc % :comment (str text)))
      :mute?     (ok #(-> % (update :mute? not) (assoc :mode (bit-or (bit-and (or (:mode %) 0) 0xFE) (if (:mute? %) 0 1)))))
      ;; MODE を Play → Mute → Mix → Rec の順に (原作の Enter、TRKSET.C の (ex+1)&3)
      :mode-next (ok (fn [t] (let [m (mod (inc (track-mode t)) 4)]
                               (assoc t :mute? (bit-test m 0) :mode (bit-or (bit-and (or (:mode t) 0) 0xFC) m)))))
      :rhy       (ok (fn [t] (let [b (or (:rhy-byte t) (if (:rhythm? t) 0x80 0))
                                   b (cond (< b 0x80) 0x80 (= b 0x80) 0x90 :else 0)]
                               (assoc t :rhy-byte b :rhythm? (>= b 0x80))))))))

(declare select-track-at)

;; UI から名前で呼び出すためのコマンド表 (TUI/GUI のキー割り当てもこれに結びつける)
(def commands
  {:insert-mml insert-mml :insert-notes insert-notes :insert-recs insert-recs :delete delete-recs
   :move move :goto goto-pos :track select-track :add-track add-track
   :optimize optimize :copy copy :paste paste :undo undo :redo redo
   :tempo set-tempo :channel set-channel
   :one-touch one-touch :insert-measure insert-measure :insert-line insert-line
   :insert-repeat-start insert-repeat-start :insert-repeat-end insert-repeat-end
   :insert-same insert-same :cut cut :duplicate duplicate-line :insert-ctrl insert-ctrl
   :track-at #'select-track-at :paste-reversed paste-reversed :mix-paste mix-paste
   :quantize quantize :note-ctrl note-ctrl :replace-range replace-range :find-next find-next
   :edit-exclusive #'edit-exclusive :set-user-exc #'set-user-exc
   :rhythm-update #'rhythm-update :rhythm-new-measure #'rhythm-new-measure
   :value-step #'value-step :restore-line #'restore-line :insert-chord #'insert-chord
   :set-rhythm-assign #'set-rhythm-assign :roll-edit #'roll-edit
   :rhythm-copy-measure #'rhythm-copy-measure :rhythm-cut-measure #'rhythm-cut-measure :rhythm-paste-measure #'rhythm-paste-measure
   :set-onetouch #'set-onetouch :reset-onetouch #'reset-onetouch
   :uex-name #'uex-name :uex-op #'uex-op :uex-copy #'uex-copy :uex-reset #'uex-reset
   :uex-load #'uex-load :uex-load-entry #'uex-load-entry :load-exc-messages #'load-exc-messages :visual-commit #'visual-commit
   :same-convert same-convert :same-extract same-extract
   :replicate replicate :replicate-all replicate-all :replicate-range replicate-range
   :repeat-compress repeat-compress :same-convert-all same-convert-all
   :set-song-field #'set-song-field :set-track-field #'set-track-field})                    ; 後で定義するので var で参照

(defn dispatch [s cmd & args]
  (if-let [f (commands cmd)]
    (apply f s args)
    (throw (ex-info "未知のコマンド" {:cmd cmd}))))

;; ---- 再生・書き出し用に、イベント形式の曲へ変換 ----
(defn ->song [s]
  (let [{:keys [tempo tracks] :as song} (:song s)
        fix (fn [e] (if (= :tempo (:type e))
                      (assoc e :value (polysted.rcp/tempo-bpm tempo (:raw e)))
                      e))
        ;; ミュートだけ除く。リズムトラックも鳴らす (原作の RHY は演奏に影響しない)。
        ;; K#+ (:key-shift) と ST+ (:st-shift) は polysted.mml/->sequence が反映する
        tracks (->> tracks
                    (remove :mute?)
                    (mapv #(assoc % :events (mapv fix (polysted.rcp/records->events-ex (:recs %) {:user-exc (:user-exc song) :ch (:ch %)})))))
        tracks (if (seq tracks)
                 (update-in tracks [0 :events] #(into [{:type :tempo :st 0 :value tempo}] %))
                 tracks)]
    (assoc song :tracks tracks)))

;; ============================================================
;; 2. view (画面を表すデータ)
;; ============================================================
(def ^:private note-names ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])

(defn key->name
  "C4=60。オクターブ -1 は '.'"
  [k]
  (let [o (dec (quot k 12))]
    (str (note-names (mod k 12)) (if (neg? o) "." o))))

(def ^:private cmd-names
  {0x98 "Tr.Exclu" 0xF7 "(cont.)" 0xF6 "COMMENT" 0xDD "Rol.Base" 0xDE "Rol.Para"
   0x90 "UserExc0" 0x91 "UserExc1" 0x92 "UserExc2" 0x93 "UserExc3"
   0x94 "UserExc4" 0x95 "UserExc5" 0x96 "UserExc6" 0x97 "UserExc7"
   0xDF "Rol.Dev#" 0xE2 "BankProg" 0xE6 "MIDI CH." 0xE7 "TEMPO" 0xEA "AFTER C."
   0xEB "CONTROL" 0xEC "PROGRAM" 0xED "AFTER K." 0xEE "PITCH" 0xF5 "KEY"
   0xF8 "]REPEAT" 0xF9 "[REPEAT" 0xFC "SAME" 0xFD "MEASURE"})

(defn rec->text [[b0 b1 b2 b3]]
  (cond
    (and (zero? b0) (zero? b2) (zero? b3)) (format "%-8s %3d" "R" b1)
    (< b0 0x80) (format "%-8s %3d %3d %3d" (key->name b0) b1 b2 b3)
    :else (format "%-8s %3d %3d %3d"
                  (get cmd-names b0 (format "0x%02X" b0)) b1 b2 b3)))

(defn view
  "表示用データ: {:header 文字列 :rows [{:idx :cursor? :text}] :msg 文字列またはnil}"
  ([s] (view s 10))
  ([s half]
   (let [recs (cur-recs s)
         n (count recs)
         pos (:pos s)
         t (cur-track s)
         from (max 0 (- pos half))
         to (min n (+ pos half))]
     {:header (format "TR%02d ch=%s tempo=%d pos=%d/%d%s"
                      (inc (:tr s)) (or (:ch t) "OFF") (get-in s [:song :tempo])
                      pos n (if (:mute? t) " [MUTE]" ""))
      :rows (mapv (fn [i] {:idx i :cursor? (= i pos)
                           :text (if (< i n) (rec->text (recs i)) "<END>")})
                  (range from (inc to)))
      :msg (:msg s)})))

(defn note-text
  "STED2 の NOTE 欄の表記: \"C 4\" \"C#4\" \"A#.\" (半音記号が無いときは空白)"
  [k]
  (let [nm (note-names (mod k 12))
        o  (dec (quot k 12))]
    (str nm (if (= 1 (count nm)) " " "") (if (neg? o) "." o))))

(defn meas-steps
  "各レコードの [MEAS STEP] (表示しない所は nil)。STED2 の trk_dis に準じる:
   - MEAS は小節の先頭行にだけ出す。0xFC 以上 (小節線・SAME・終端) と、SAME の直前で小節が終わる
   - STEP は小節内の発音タイミングの番号。和音の途中 (直前の ST=0) では増えない
   ※ 0x98/0xF7 (エクスクルーシブ) の特別扱いは省略"
  [recs]
  (let [n (count recs)]
    (loop [i 0 meas 1 step 1 out []]
      (if (>= i n)
        out
        (let [r     (recs i)
              prev  (when (pos? i) (recs (dec i)))
              nxt   (when (< (inc i) n) (recs (inc i)))
              head? (or (nil? prev) (>= (prev 0) 0xFC) (= (r 0) 0xFC))
              step? (and (< (r 0) 0xF0)
                         (or (nil? prev) (pos? (prev 1)) (>= (prev 0) 0xF0)))
              end?  (or (>= (r 0) 0xFC) (and nxt (= (nxt 0) 0xFC)))
              out   (conj out [(when head? meas) (when step? step)])]
          (recur (inc i)
                 (if end? (inc meas) meas)
                 (cond end? 1 step? (inc step) :else step)
                 out))))))

(defn measure-st
  "i 番の小節線で終わる小節の ST の合計 (原作の小節線の表示 EDITS.C step_cluc)。
   後ろから 0xFC 以上まで戻りながら足す。リピートの中は回数倍 (0 と 255 は1回)。
   小節の外で始まるリピートの分は、最後に割り戻す (原作どおり)"
  [recs i]
  (loop [j (dec i) a 0 b 1 stack []]              ; stack: [外側の倍率 その時点の合計 回数]
    (if (or (neg? j) (> ((recs j) 0) 0xFB))
      (reduce (fn [a [_ lp lx]] (+ lp (quot (- a lp) lx))) a (rseq stack))
      (let [[c d] (recs j)]
        (cond
          (< c 0xF0) (recur (dec j) (+ a (* d b)) b stack)
          (and (= c 0xF8) (< (count stack) 16))
          (let [ct (if (#{0 255} d) 1 d)]
            (recur (dec j) a (* b ct) (conj stack [b a ct])))
          (and (= c 0xF9) (seq stack))
          (recur (dec j) a (first (peek stack)) (pop stack))
          :else (recur (dec j) a b stack))))))

(defn track-pos-at
  "小節 meas (1始まり) の先頭から ST 経過 st の位置の行 (EDITS.C meas_add + sttm_ad)。
   その小節が無ければ末尾 (END)"
  [recs meas st]
  (if-let [start (polysted.edit/meas-add recs meas)]
    (loop [p start]
      (if (or (>= p (count recs)) (>= (measure-st recs p) st) (>= ((recs p) 0) 0xFC))
        p
        (recur (inc p))))
    (count recs)))

(defn select-track-at
  "トラック n に移り、移動前と同じ小節・同じ ST 経過の行にカーソルを置く (原作の CTRL+←/→)。
   端のトラックでは何もしない。原作と違い、空のトラックも飛ばさない【設計判断】"
  [s n]
  (let [cnt (count (get-in s [:song :tracks]))
        t   (-> n (max 0) (min (dec cnt)))]
    (if (= t (:tr s))
      s
      (let [recs (cur-recs s)
            p    (:pos s)
            meas (polysted.edit/meas-no recs p)
            st   (measure-st recs p)
            s2   (select-track s t)]
        (assoc s2 :pos (track-pos-at (cur-recs s2) meas st))))))

(defn table-view
  "STED2 のエディット画面の形 (MEAS STEP: NOTE K# ST GT VEL) の表示データ。
   rows: [{:idx :cursor? :meas :step :note? :cells [NOTE K# ST GT VEL] :tie? :text}]
   ノート以外の行は :note? false で、:text に内容。"
  ([s] (table-view s 10))
  ([s half]
   (let [recs (cur-recs s)
         n    (count recs)
         pos  (:pos s)
         ;; 2*half+1 行の窓。カーソルを中央に置くが、先頭・末尾の近くでは窓をずらして画面を埋める
         from (max 0 (min (- pos half) (- n (* 2 half))))
         to   (min n (+ from (* 2 half)))
         ms   (meas-steps recs)
         base (view s half)]
     (assoc base :rows
            (mapv (fn [i]
                    (if (>= i n)
                      {:idx i :cursor? (= i pos) :note? false :text "<END>"}
                      (let [[b0 b1 b2 b3 :as r] (recs i)
                            [meas step] (ms i)
                            chord? (and (zero? b1) (< (inc i) n) (< ((recs (inc i)) 0) 0x80))]
                        (merge {:idx i :cursor? (= i pos) :meas meas :step step}
                               (if (< b0 0x80)
                                 {:note? true
                                  :cells [(if (or (zero? b2) (zero? b3)) "R" (note-text b0))
                                          (str b0) (if chord? "" (str b1)) (str b2) (str b3)]
                                  :tie? (and (not chord?) (> b2 b1))}
                                 {:note? false
                                  :cells (ctrl-cells r)
                                  ;; 原作の表記: 小節線 "--------" + 小節の ST 合計、小節複写 "========"
                                  :text (case (int b0)
                                          0xFD (str "-------- MEAS END  ST=" (measure-st recs i))
                                          0xFC (str "======== SAME MEAS " (inc (polysted.edit/same-ref r)))
                                          0x98 (let [[data _] (polysted.rcp/track-exclusive-data recs i)]
                                                 (str "Tr.Exclu  " (exc-hex data)))
                                          0xF7 (str "          " (exc-hex (take-while #(not= % 0xF7) [b2 b3])))
                                          0xF9 "[ REPEAT"
                                          0xF8 (str "] REPEAT " (if (#{0 255} b1) "inf" b1))
                                          (rec->text r))})))))
                  (range from (inc to)))))))

(defn render-text [v]
  (str/join "\n"
            (concat [(:header v)]
                    (map (fn [{:keys [idx cursor? text]}]
                           (format "%s%4d  %s" (if cursor? ">" " ") idx text))
                         (:rows v))
                    (when (:msg v) [(str "! " (:msg v))]))))

;; ============================================================
;; 3. REPL 層
;; ============================================================
(defonce S (atom (new-session)))
(defonce ^:private seqr (atom nil))

(defn show [] (println (render-text (view @S))) :ok)

;; メッセージ(:msg)は次の操作で消える
(defn- act! [f & args] (swap! S #(apply f (dissoc % :msg) args)) (show))

(defn mml [text] (act! insert-mml text))
(defn n [text] (act! insert-notes text))
(defn bar "小節線を挿入" [] (act! insert-measure))
(defn rep-start "リピート開始 [ を挿入" [] (act! insert-repeat-start))
(defn rep-end "リピート終了 ] を挿入 (n: 合計の演奏回数)" [n] (act! insert-repeat-end n))
(defn same "小節複写 SAME を挿入 (j: 複写元の小節番号 1〜)" [j] (act! insert-same j))
(defn ins
  "レコードのベクタを直接挿入: (s/ins [[60 48 46 64]])"
  [recs] (act! insert-recs recs))
(defn del ([] (del 1)) ([n] (act! delete-recs n)))
(defn mv [n] (act! move n))
(defn go [n] (act! goto-pos n))
(defn trk [n] (act! select-track n))
(defn newtrk [ch] (act! add-track ch))
(defn opt [] (act! optimize))
(defn cp [from to] (act! copy from to))
(defn pst [] (act! paste))
(defn cut! "from 以上 to 未満の行を削除してクリップボードへ" [from to] (act! cut from to))
(defn dup "カーソル行を二重化" [] (act! duplicate-line))
(defn pst-rev "クリップボードを前後反転して貼り付け" [] (act! paste-reversed))
(defn pst-mix "カーソル以降にクリップボードを時刻順に混ぜる" [] (act! mix-paste))
(defn nctl
  "ノートの一括操作 (原作の Shift+F1)。カーソルから最後まで。
   (s/nctl \\L 40 100) 直線 / \\C 和音内 / \\R ランダム / \\O 足す / \\T ST・GT をずらす /
   (s/nctl \\U) 和音を低い順 / \\D 高い順 / (s/nctl \\G 6) ストラム (ST と GT) / \\S (ST だけ)"
  ([m] (nctl m nil nil))
  ([m a] (nctl m a nil))
  ([m a b] (act! note-ctrl (:pos @S) (count (cur-recs @S)) (note-ctrl-modes (Character/toUpperCase (char m))) a b)))

(defn same-conv "重複した小節を SAME に置き換える" [] (act! same-convert))
(defn same-ext "SAME を展開する (省略でトラック全体)" ([] (act! same-extract)) ([from to] (act! same-extract from to)))

(defn rcomp "繰り返しをリピートに圧縮: (s/rcomp) 今のトラック / (s/rcomp true) 全トラック"
  ([] (act! repeat-compress false)) ([all?] (act! repeat-compress all?)))

(defn rpl "レプリケート: (s/rpl トラック 開始小節 終了小節 回数 キーシフト)。トラックは 1 始まり"
  [tr top end times bias] (act! replicate (dec tr) top end times bias))

(defn rep "置換 (カーソルから最後まで): (s/rep \"C4:D#4\") (s/rep \",,,:+10\")"
  [text] (act! replace-range (:pos @S) (count (cur-recs @S)) text :replace))
(defn del-match "指定削除 (カーソルから最後まで): (s/del-match \"/S\")"
  [text] (act! replace-range (:pos @S) (count (cur-recs @S)) text :delete))
(defn fnd "検索して移動: (s/fnd \"C4\") 次へ、(s/fnd) 前回の指定で次へ、(s/fnd \"C4\" false) 前へ"
  ([] (act! find-next true))
  ([text] (act! find-next text true))
  ([text forward?] (act! find-next text forward?)))

(defn quan "クオンタイズ: (s/quan 12 100) はカーソルから最後まで、(s/quan 0 16 12 100) は行 0〜15"
  ([res rate] (quan (:pos @S) (count (cur-recs @S)) res rate))
  ([from to res rate] (act! quantize from to res rate)))
(defn ctl "コントローラを挿入: (s/ctl \"V\" 0 100) 音量 / (s/ctl \"P\" 0 -4096) ベンド / (s/ctl \"G\" 0 49) 音色"
  [sym & params] (apply act! insert-ctrl sym params))
(defn un [] (act! undo))
(defn re [] (act! redo))
(defn tempo [bpm] (act! set-tempo bpm))
(defn ch [n] (act! set-channel n))

(defn smf-path? [path]
  (boolean (re-find #"(?i)\.(mid|midi|smf)$" (str path))))

(defn song-from-file
  "拡張子で読み分ける: .mid/.midi/.smf → SMF、それ以外 → RCP/R36/G36 (見出しで判定)。=> {:song 曲 :warnings [...]}"
  [path]
  (if (smf-path? path)
    (polysted.smf/read-smf path)
    (let [song (song-from-rcp path)]
      {:song (dissoc song :load-warnings) :warnings (vec (:load-warnings song))})))

(defn open!
  "RCP/R36 または SMF (.mid) を読み込む (保存先は未設定のまま。保存は (s/save! \"x.edn\") など)"
  [path]
  (let [{:keys [song warnings]} (song-from-file path)]
    (reset! S (cond-> (new-session song)
                (seq warnings) (assoc :msg (str/join " / " warnings))))
    (show)))

(defn load!
  "EDN の曲を読み込む。以後 (s/save!) で同じファイルに保存できる。undo 履歴は消える。"
  [path]
  (reset! S (assoc (new-session (polysted.edn/load-song path)) :file path))
  (show))

(defn save!
  "曲を EDN で保存。パス省略時は最後に load!/save! したファイル。"
  ([] (if-let [path (:file @S)]
        (save! path)
        (throw (ex-info "保存先が未設定です。(s/save! \"x.edn\") で指定してください" {}))))
  ([path]
   (polysted.edn/save-song! (:song @S) path)
   (swap! S assoc :file path)
   (println "saved" path)
   :saved))

(defn new!
  "新しい空の曲にする (保存していない変更は失われる)"
  []
  (reset! S (new-session))
  (show))

(defn save-rcp!
  "曲を RCP (.rcp) / R36 (.r36) で書き出す"
  [path]
  (polysted.rcp/write-rcp! (:song @S) path)
  (println "wrote" path)
  :saved)

(defn rcp-path? [path]
  (boolean (re-find #"(?i)\.(rcp|r36)$" (str path))))

(defn save-mid! [path]
  (polysted.mml/write-mid! (->song @S) path)
  (println "wrote" path))

(defn playing? [] (boolean (some-> @seqr .isRunning)))

(defn open-at-start!
  "起動時に path を開いて S に入れる (TUI・GUI の -main から)。nil なら空の曲のまま。
   .edn が無ければ新規 (w でそこに保存)。RCP/R36/G36/.mid の変換の注意はメッセージに出す"
  [path]
  (let [lc (some-> path str/lower-case)]
    (cond
      (nil? path) nil
      (str/ends-with? lc ".edn")
      (if (.exists (java.io.File. ^String path))
        (reset! S (assoc (new-session (polysted.edn/load-song path)) :file path))
        (swap! S assoc :file path))
      :else
      (let [{:keys [song warnings]} (song-from-file path)]
        (reset! S (cond-> (new-session song)
                    (seq warnings) (assoc :msg (str/join " / " warnings))))))))

(defn tui
  "TUI を起動する (polysted.tui は初回に読み込まれる)。終了すると REPL に戻る。"
  []
  ((requiring-resolve 'polysted.tui/run!)))

(defn gui
  "GUI (Swing のウィンドウ) を開く (polysted.gui は初回に読み込まれる)。
   REPL はそのまま使え、S は GUI と共有する (REPL で変えたら GUI のウィンドウをクリックすると反映)"
  []
  ((requiring-resolve 'polysted.gui/run!)))

(defn stop! []
  (when-let [sr @seqr]
    (when (.isOpen sr) (.stop sr) (.close sr))
    (reset! seqr nil))
  :stopped)

(defn measure-tick
  "カーソルのある小節の先頭の tick (今のトラックで、それより前のリピート・SAME を展開して数える)。
   原作のポイントプレイ (カーソルのある小節から演奏) の開始位置"
  [s]
  (let [recs (cur-recs s)
        start (or (polysted.edit/meas-add recs (polysted.edit/meas-no recs (:pos s))) (count recs))
        evs (polysted.mml/expand-track {:events (polysted.edit/records->events (subvec recs 0 start))})]
    (reduce + (map #(:st % 0) (:events evs)))))

(defn play!
  "曲を演奏する。tick を指定するとそこから (ポイントプレイ)。s を渡すとその状態の曲 (既定は S)"
  ([] (play! 0))
  ([tick] (play! tick @S))
  ([tick s]
  (stop!)
  (let [sq (polysted.mml/->sequence (->song s))
        sr (MidiSystem/getSequencer)]
    (.open sr)
    (.setSequence sr sq)
    (when (pos? tick) (.setTickPosition sr (long tick)))
    (.addMetaEventListener
     sr (reify MetaEventListener
          (meta [_ m]
            (when (= 47 (.getType ^javax.sound.midi.MetaMessage m))
              (future (stop!))))))
    (.start sr)
    (reset! seqr sr)
    :playing)))

(defn play-here! "カーソルのある小節から演奏 (原作のポイントプレイ)" [] (play! (measure-tick @S)))

(defn measure-span
  "カーソルのある小節の [開始 tick 終了 tick] (今のトラック。小節の長さは小節線までの ST をリピート込みで数える)"
  [s]
  (let [recs (cur-recs s)
        m    (polysted.edit/meas-no recs (:pos s))
        a    (or (polysted.edit/meas-add recs m) (count recs))
        b    (or (polysted.edit/meas-add recs (inc m)) (count recs))
        len  (fn [rs] (reduce + (map #(:st % 0) (:events (polysted.mml/expand-track {:events (polysted.edit/records->events rs)})))))
        t0   (len (subvec recs 0 a))]
    [t0 (+ t0 (len (subvec recs a b)))]))

(defn play-measure!
  "カーソルのある小節を、編集中のトラックだけ演奏する (原作の小節プレイ XF1 / M)。s を渡すとその状態で"
  ([] (play-measure! @S))
  ([s]
  (let [[t0 t1] (measure-span s)
        ;; ->song はミュートのトラックを除くので、シーケンス上の番号を数え直す
        idx (count (remove :mute? (take (:tr s) (get-in s [:song :tracks]))))]
    (if (or (:mute? (cur-track s)) (>= t0 t1))
      :nothing
      (do (play! t0 s)
          (let [^javax.sound.midi.Sequencer sr @seqr
                n (alength (.getTracks (.getSequence sr)))]
            (doseq [i (range n)] (.setTrackMute sr i (not= i idx)))
            (future (loop [] (Thread/sleep 10)
                      (when (identical? sr @seqr)
                        (if (and (.isRunning sr) (< (.getTickPosition sr) t1)) (recur) (stop!))))))
          :playing)))))

;; ============================================================
;; レコーディング (原作 RECORD.C real_record。変換は src/polysted/record.clj)
;; ============================================================
;; 録音中: ほかのトラック (MODE が Play か Mix) とメトロノームを演奏し、MIDI 入力をシーケンサの tick 付きで貯める。
;; 1 小節目から始めるときは 2 小節のカウント (原作どおり)。止めたら MODE が Mix・Rec のトラックへ書き込む。
;; メトロノームは原作の既定 (ch16、キー 64、拍頭 127・ほか 117、GT 2) 【原作 STED2.CNF #rec_met の既定】。
(defonce ^:private rec-state (atom nil))

(defn midi-inputs "MIDI 入力の機器 (送信できるもの。シーケンサと音源は除く)" []
  (vec (for [info (MidiSystem/getMidiDeviceInfo)
             :let [d (MidiSystem/getMidiDevice info)]
             :when (and (not= 0 (.getMaxTransmitters d))
                        (not (instance? javax.sound.midi.Sequencer d))
                        (not (instance? javax.sound.midi.Synthesizer d)))]
         {:name (.getName info) :device d})))

(defn rec-tracks "録音するトラック (MODE が Mix か Rec) の番号" [s]
  (vec (keep-indexed (fn [i t] (when (bit-test (track-mode t) 1) i)) (get-in s [:song :tracks]))))

(defn recording? [] (boolean (:running? @rec-state)))

(def ^:private play-mode-advice "Tab のトラック設定で MODE を Play か Mix に")

(defn mute-hint
  "今のトラックが再生で鳴らない MODE (Mute・Rec。原作どおり bit0 のトラックは演奏しない) なら案内の文。鳴るなら nil"
  [s]
  (let [t (cur-track s)]
    (when (odd? (track-mode t))
      (str "TR" (inc (:tr s)) " は MODE が " (mode-str t) " なので再生で鳴りません (" play-mode-advice ")"))))

(defn record-event!
  "MIDI 入力を 1 つ記録する (入力の機器の受け手から呼ぶ。テストでは直接呼ぶ)。カウントの間のものは捨てる"
  [bytes]
  (when-let [{:keys [^javax.sound.midi.Sequencer seq offset events]} (when (recording?) @rec-state)]
    (let [t (- (.getTickPosition seq) offset)]
      (when (>= t 0) (swap! events conj {:tick t :bytes (mapv #(bit-and % 0xFF) bytes)})))))

(defn- metronome-track! [^javax.sound.midi.Sequence sq bar-len beats from to]
  (let [tr (.createTrack sq) beat (max 1 (quot bar-len (max 1 beats)))]
    (doseq [t (range from to beat)
            :let [v (if (zero? (mod (- t from) bar-len)) 127 117)]]
      (.add tr (javax.sound.midi.MidiEvent. (javax.sound.midi.ShortMessage. javax.sound.midi.ShortMessage/NOTE_ON 15 64 v) (long t)))
      (.add tr (javax.sound.midi.MidiEvent. (javax.sound.midi.ShortMessage. javax.sound.midi.ShortMessage/NOTE_OFF 15 64 0) (long (+ t 2)))))))

(defn record-start!
  "録音を始める。opts: {:ch 録音チャンネル 1〜16 :thru? 入力を音源へも送る :metronome? :top 開始小節 :btm 終わりの小節
   :device 入力の機器の名前 (nil なら最初のもの) :no-input? 入力の機器を使わない (テスト用)}。=> {:ok? :msg}"
  [s {:keys [thru? metronome? top btm device no-input?] :or {top 1 btm 999} :as opts}]
  (stop!)
  (let [ins (midi-inputs)
        in (if device (first (filter #(= device (:name %)) ins)) (first ins))]
    (cond
      (empty? (rec-tracks s)) {:ok? false :msg "レコーディングトラックが設定されていません (Tab のトラック設定で MODE を Mix か Rec に)"}
      (and (nil? in) (not no-input?)) {:ok? false :msg "MIDI 入力の機器が見つかりません"}
      :else
      (let [bar-len (bar-length s)
            count-in (if (= 1 top) (* 2 bar-len) 0)
            start (if (= 1 top) 0 (measure-tick (assoc s :pos (or (polysted.edit/meas-add (cur-recs s) top) (count (cur-recs s))))))
            sq (polysted.mml/->sequence (->song s))
            _ (when (pos? count-in)                            ; カウントの 2 小節ぶん全体を後ろへ
                (doseq [^javax.sound.midi.Track tr (.getTracks sq) i (range (.size tr))]
                  (let [e (.get tr i)] (when (pos? (.getTick e)) (.setTick e (+ (.getTick e) count-in))))))
            offset (+ start count-in)
            end (+ offset (* bar-len (inc (- btm top))))
            _ (when metronome? (metronome-track! sq bar-len (first (or (get-in s [:song :time-sig]) [4 4])) start end))
            sr (MidiSystem/getSequencer)
            thru (when thru? (try (MidiSystem/getReceiver) (catch Exception _ nil)))
            dev ^javax.sound.midi.MidiDevice (:device in)]
        (.open sr) (.setSequence sr sq) (.setTickPosition sr (long start))
        (.setTempoInBPM sr (float (or (get-in s [:song :tempo]) 120)))   ; 最初のテンポ (テンポの行があれば演奏中に変わる)
        (reset! seqr sr)
        (reset! rec-state {:running? true :seq sr :offset offset :end end :events (atom []) :opts opts :thru thru :dev dev})
        (when dev
          (.open dev)
          (.setReceiver (.getTransmitter dev)
                        (reify javax.sound.midi.Receiver
                          (send [_ msg _]
                            (record-event! (.getMessage ^javax.sound.midi.MidiMessage msg))
                            (when thru (.send ^javax.sound.midi.Receiver thru msg -1)))
                          (close [_]))))
        (.start sr)
        ;; 終わりの小節に達したら演奏を止める (取り込みは record-stop!)
        (future (loop [] (Thread/sleep 20)
                  (when (and (identical? sr @seqr) (.isRunning sr) (< (.getTickPosition sr) end)) (recur)))
                (when (identical? sr @seqr) (.stop sr)))
        {:ok? true :msg (str "録音中 (" (if in (:name in) "入力なし") ")。止めるときは Esc")}))))

(defn record-stop!
  "録音を止めて、MODE が Mix・Rec のトラックへ書き込む (undo 1回分)。録音中でなければそのまま"
  [s]
  (if-let [{:keys [^javax.sound.midi.Sequencer seq offset end events opts dev]} @rec-state]
    (let [stop-tick (min end (.getTickPosition seq))
          _ (do (when dev (.close ^javax.sound.midi.MidiDevice dev)) (stop!) (reset! rec-state nil))
          evs (sort-by :tick @events)
          trs (rec-tracks s)
          bar-len (bar-length s)
          one? (= 1 (count trs))
          song (reduce (fn [song i]
                         (let [t (get-in song [:tracks i])
                               ch (if one? (dec (or (:ch opts) 1)) (bit-and (dec (or (:ch t) 1)) 15))
                               nw (polysted.record/midi->records evs {:ch ch :bar-len bar-len :end (- stop-tick offset)
                                                                  :sysex? (or one? (zero? ch))})]
                           (update-in song [:tracks i :recs]
                                      #(polysted.record/apply-recording (vec %) nw {:mix? (= 2 (track-mode t)) :top (:top opts 1)
                                                                               :btm (:btm opts 999) :bar-len bar-len :ch (:ch t)}))))
                       (:song s) trs)]
      (-> s push-undo (assoc :song song)
          (assoc :msg (str "録音を取り込みました (" (count evs) " 個の入力、トラック " (str/join " " (map inc trs)) ")"
                           (when (some #(= 3 (track-mode (get-in song [:tracks %]))) trs)
                             (str "。Rec のトラックは再生で鳴りません (" play-mode-advice ")"))))))
    s))

(defn visual-preview!
  "ビジュアルエディットの試し再生 (原作の M = poplay(po,3)): 仮に書き込んだ状態で、始めた位置の小節を
   編集中のトラックだけ演奏する。曲 (S) は変えない"
  [s ve mix?]
  (play-measure! (assoc (visual-commit s ve mix?) :pos (:from ve))))

;; ============================================================
;; 簡易チェック (純粋なコアのみ)
;; ============================================================
(defn- check [label expected actual]
  (if (= expected actual)
    (println "OK " label)
    (do (println "NG " label)
        (println "    expected:" expected)
        (println "    actual:  " actual))))

(defn run-checks []
  (let [s1 (-> (new-session) (insert-mml "c4 d4"))]
    (check "MMLを挿入するとカーソルが進む"
           [[[60 48 46 64] [62 48 46 64]] 2]
           [(cur-recs s1) (:pos s1)])
    (check "削除とundo"
           [[[60 48 46 64]] [[60 48 46 64] [62 48 46 64]]]
           (let [s2 (-> s1 (move -1) (delete-recs 1))]
             [(cur-recs s2) (cur-recs (undo s2))]))
    (check "長さは直前のノートから引き継ぐ (c8 のあとの d は8分)"
           [[60 24 22 64] [62 24 22 64]]
           (-> (new-session) (insert-mml "c8") (insert-mml "d") cur-recs))
    (check "最適化: 連続する休符が前に結合される"
           [[60 96 46 64]]
           (-> (new-session) (insert-mml "c4 r8 r8") optimize cur-recs))
    (check "音名の表示"
           ["C4" "C#." "A#5"]
           [(key->name 60) (key->name 1) (key->name 82)])
    (check "dispatch でも同じ結果"
           [[60 48 46 64]]
           (-> (new-session) (dispatch :insert-mml "c4") cur-recs))
    (check "再生用の曲への変換 (->song)"
           [:tempo :note]
           (->> (-> (new-session) (insert-mml "c4") ->song :tracks first :events)
                (mapv :type)))
    ;; ---- 音名入力 (期待値は md 4.5 の規則を手で追って求めた) ----
    (check "音名入力: C4 E G を挿入しカーソルが進む"
           [[[60 48 46 64] [64 48 46 64] [67 48 46 64]] 3]
           (let [s (insert-notes (new-session) "C4 E G")] [(cur-recs s) (:pos s)]))
    (check "音名入力: 長さとオクターブは直前のノートから (o5 c8 → E は E5 の8分)"
           [[72 24 22 64] [76 24 22 64]]
           (-> (new-session) (insert-mml "o5 c8") (insert-notes "E") cur-recs))
    (check "音名入力: オクターブの基準は休符を飛ばした直前のノート"
           [74 48 46 64]
           (-> (new-session) (insert-mml "o5 c4 r4") (insert-notes "D") cur-recs last))
    (check "音名入力: 曲の調号を使う (ト長調で F → F#)"
           [66 60]
           (->> (-> (new-session) (assoc-in [:song :key] 1) (insert-notes "F4 C4") cur-recs)
                (mapv first)))
    (check "音名入力: リズムトラックでは調号を無視"
           [65]
           (->> (-> (new-session) (assoc-in [:song :key] 1)
                    (assoc-in [:song :tracks 0 :rhythm?] true)
                    (insert-notes "F4") cur-recs)
                (mapv first)))
    (let [with-recs (fn [recs pos] (-> (new-session) (assoc-in [:song :tracks 0 :recs] recs) (assoc :pos pos)))]
      (check "調 (scale_no): 小節の中の 0xF5 を使い、小節線で曲の調に戻る (音名・MML)"
             [66 65 66 1]
             [(first (last (cur-recs (insert-notes (with-recs [[0xF5 1 0 0]] 1) "F4"))))
              (first (last (cur-recs (insert-notes (with-recs [[0xF5 1 0 0] [0xFD 0 0 0]] 2) "F4"))))
              (first (last (cur-recs (insert-mml (with-recs [[0xF5 1 0 0]] 1) "f"))))
              (key-at (with-recs [[0xF5 1 0 0] [60 48 46 64] [0xFD 0 0 0]] 2))])
      (check "MML: リズムトラックでは調号を無視 (原作 mml_inp の trrhy)"
             [65 1]
             (let [s (-> (new-session) (assoc-in [:song :key] 1) (assoc-in [:song :tracks 0 :rhythm?] true))]
               [(first (first (cur-recs (insert-mml s "f")))) (get-in s [:song :key])])))
    (check "音名入力: 無効なトークンは捨てて :msg に残す"
           [[[60 48 46 64]] "無効な音名: X"]
           (let [s (insert-notes (new-session) "C4 X")] [(cur-recs s) (:msg s)]))
    (check "音名入力: 全部無効なら何も変えない (undo 履歴も増えない)"
           [[] 0]
           (let [s (insert-notes (new-session) "X")] [(cur-recs s) (count (:undo s))]))
    (check "音名入力: undo で戻る / dispatch でも同じ"
           [[] [[60 48 46 64]]]
           [(-> (new-session) (insert-notes "C4") undo cur-recs)
            (-> (new-session) (dispatch :insert-notes "C4") cur-recs)])
    (check "表示: MEAS は小節の先頭、STEP は和音の途中で増えない"
           [[1 1] [nil 2] [nil nil] [nil 3] [nil nil] [2 1] [nil nil]]
           (meas-steps [[60 48 46 64] [64 0 46 64] [67 48 46 64] [60 48 46 64]
                        [0xFD 0 0 0] [62 48 46 64] [0xFE 0 0 0]]))
    (check "表示の窓: 先頭・末尾の近くでも 2*half+1 行を使う"
           [[0 4] [6 10] [3 7]]
           (let [s (insert-notes (new-session) "C4 D E F G A B C5 D E")]
             (mapv (fn [p] (let [rs (:rows (table-view (goto-pos s p) 2))] [(:idx (first rs)) (:idx (last rs))]))
                   [0 10 5])))
    (check "表示: NOTE 欄の表記"
           ["C 4" "C#4" "A#." "B 9"] (mapv note-text [60 61 10 131]))
    (check "ピアノロール: 音の途中に置く・動かす (undo 1回ずつ)、音でない行やキーの範囲外は :msg"
           [[[60 24 46 64] [67 24 20 100] [64 48 46 64]] 1 2
            [[60 48 46 64] [69 0 20 100] [64 48 46 64]] 1 3
            "音の行ではありません" "キーは 0〜127 です"]
           (let [s1 (-> (new-session) (insert-notes "C4 E") (roll-edit :add [24 67 20 100]))
                 s2 (roll-edit s1 :move [1 48 69])]
             [(cur-recs s1) (:pos s1) (count (:undo s1)) (cur-recs s2) (:pos s2) (count (:undo s2))
              (:msg (roll-edit s2 :delete [3])) (:msg (roll-edit s2 :key [0 200]))]))
    (check "ビジュアルエディット: 新規は挿入してカーソルを後ろへ、範囲はステップを変えなければ上書き、Shift は混ぜる"
           [[[0xEB 24 7 10] [0xEB 24 7 20] [60 48 46 64]] 2 2
            [[0xEB 24 7 1] [60 48 46 64]]
            [[60 0 46 64] [0xEB 48 7 10]]]
           (let [base (-> (new-session) (insert-notes "C4") (goto-pos 0))
                 v {:vmo 7 :vmo2 0 :para [10 20] :step [24 24] :from 0 :len 0 :ost 48}
                 s1 (visual-commit base v false)
                 s2 (visual-commit (-> base (insert-recs [[0xEB 24 7 9]]) (goto-pos 0))
                                   {:vmo 7 :vmo2 0 :para [1] :step [24] :from 0 :len 1 :ost 24} false)
                 s3 (visual-commit base (assoc v :para [10 10]) true)]
             [(cur-recs s1) (:pos s1) (count (:undo s1)) (cur-recs s2) (cur-recs s3)]))
    (check "ワンタッチ表: 変えた表で F キー、範囲外は :msg、既定に戻す"
           [[[60 60 50 64]] "値は 0〜127 です" true [192 144 96 72 48 36 24 18 12 6]]
           (let [s (-> (new-session) (insert-mml "l2 c") (goto-pos 0) (set-onetouch :st 4 60) (set-onetouch :gt 4 50))]
             [(cur-recs (one-touch s 1 4 false)) (:msg (set-onetouch s :vel 0 200))
              (= onetouch (onetouch-table (reset-onetouch s))) (:st (onetouch-table (reset-onetouch s :st 4)))]))
    (check "ユーザーエクスクルーシブの表: 挿入・名前・写す・空に (undo 1回ずつ)"
           [[0x41 0xF7] ["test" [0x41 0xF7]] 3 true]
           (let [s1 (-> (new-session) (uex-op 0 :ins 0 0x41)) s2 (uex-name s1 0 " test ") s3 (uex-copy s2 1 0)]
             [(subvec (second ((uex-table s1) 0)) 0 2) (update ((uex-table s3) 1) 1 subvec 0 2) (count (:undo s3))
              (every? #(= % ["" (vec (repeat 24 0xF7))]) (uex-table (uex-reset s3 nil)))]))
    (check "EXC ファイルの読み込み: 挿入 (2 つ目は新しい行)、Tr.Exclu の上なら置き換え、書き出し用のデータ"
           [[[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0xF7 0xF7] [0x98 0 0 0] [0xF7 0 0x43 0xF7] [60 48 46 64]]
            [[0x98 0 0 0] [0xF7 0 0x7F 0xF7] [0x98 0 0 0] [0xF7 0 0x43 0xF7] [60 48 46 64]]
            [[0x41 0x10]] nil]
           (let [s1 (-> (new-session) (insert-notes "C4") (goto-pos 0) (load-exc-messages [[0x41 0x10] [0x43]]))
                 s2 (load-exc-messages s1 [[0x7F]])]
             [(cur-recs s1) (cur-recs s2) (exc-messages-at s1) (exc-messages-at (goto-pos s1 5))]))
    (check "リズム画面の小節: 複写 (SAME は中身に)、切り取り (消える小節を指す SAME は展開)、貼り付け、空の小節・バッファ無し"
           [[[38 48 1 100] [0xFD 0 0 2]] [[36 48 1 100] [0xFD 0 0 0]]
            [[38 48 1 100] [0xFD 0 0 1] [36 48 1 100] [0xFD 0 0 2] [0xFE 0 0 0]]
            [[38 48 1 100] [0xFD 0 0 1] [36 48 1 100] [0xFD 0 0 2] [36 48 1 100] [0xFD 0 0 3] [0xFE 0 0 0]] 3
            "空の小節です" "複写した小節がありません (F8 / y で複写、F7 / x で切り取り)"]
           (let [s (-> (new-session) (insert-recs [[36 48 1 100] [0xFD 0 0 1] [38 48 1 100] [0xFD 0 0 2] [0xFC 0 0 0] [0xFE 0 0 0]]))
                 c (rhythm-cut-measure s 1)
                 pst (rhythm-paste-measure c 2)]
             [(:rhy-clip (rhythm-copy-measure s 2)) (:rhy-clip (rhythm-copy-measure s 3))
              (cur-recs c) (cur-recs pst) (count (:undo pst))
              (:msg (rhythm-copy-measure s 4)) (:msg (rhythm-paste-measure s 1))]))
    (check "G36 (RCP v3) の読み込み: 6 バイトのレコード → 4 バイト (長い ST は休符で、GT は 255 に、コメント・エクスクルーシブは 2 バイトずつに)、見出しとトラック"
           [[[60 240 255 100] [0 60 0 0] [0xEB 0 7 90]
             [0xF6 0 72 101] [0xF7 0 108 108] [0xF7 0 111 32] [0xF7 0 119 111] [0xF7 0 114 108] [0xF7 0 100 32] [0xF7 0 32 32] [0xF7 0 32 32]
             [0x98 0 0x10 0x40] [0xF7 0 0x41 0x10] [0xF7 0 0x42 0x12] [0xF7 0 0x83 0x40] [0xF7 0 0 0x7F] [0xF7 0 0 0x84] [0xF7 0 0xF7 0xF7]
             [0xFD 0 0 0] [0xFC 1 0 0] [0xF9 0 0 0] [62 48 46 64] [0xF8 2 0 0] [0xFD 0 0 0]]
            ["G36 TEST" 48 120 [4 4] 18 254 "TRK" ["BD" 36 1] ["GS" [0x41 0x10 0xF7]] 2 false]]
           (let [hdr (byte-array 0xC98)
                 put-s! (fn [^bytes b o ^String t] (let [x (.getBytes t "US-ASCII")] (System/arraycopy x 0 b o (alength x))))
                 put! (fn [^bytes b o v] (aset b (int o) (unchecked-byte v)))
                 rows [[60 100 44 1 144 1] [0xEB 90 0 0 7 0]
                       (into [0xF6] (map int "Hello")) (into [0xF7] (map int " worl")) [0xF7 (int \d) 32 32 32 32]
                       [0x98 0x40 0 0 0x10 0] [0xF7 0x41 0x10 0x42 0x12 0x83] [0xF7 0x40 0 0x7F 0 0x84] [0xF7 0xF7 0xF7 0xF7 0xF7 0xF7]
                       [0xFD 0 0 0 0 0] [0xFC 0 1 0 0x30 0] [0xF9 0 0 0 0 0] [62 64 48 0 46 0] [0xF8 0 2 0 0 0] [0xFD 0 0 0 0 0]
                       [0xFE 0 0 0 0 0]]
                 trk (byte-array (+ 46 (* 6 (count rows))))
                 _ (do (put-s! hdr 0 polysted.rcp/g36-id) (put-s! hdr 0x20 "G36 TEST")
                       (put! hdr 0x208 1) (put! hdr 0x20A 48) (put! hdr 0x20C 120) (put! hdr 0x20E 4) (put! hdr 0x20F 4)
                       (put-s! hdr 0x318 "BD") (put! hdr (+ 0x318 14) 36) (put! hdr (+ 0x318 15) 1)
                       (put-s! hdr 0xB18 "GS") (doseq [[i v] (map-indexed vector (concat [0x41 0x10] (repeat 23 0xF7)))] (put! hdr (+ 0xB18 23 i) v))
                       (put! trk 0 (alength trk)) (put! trk 4 1) (put! trk 6 17) (put! trk 8 0xFE) (put-s! trk 10 "TRK")
                       (doseq [[k r] (map-indexed vector rows) [j v] (map-indexed vector r)] (put! trk (+ 46 (* 6 k) j) v)))
                 bs (byte-array (concat hdr trk))
                 song (polysted.rcp/read-g36-bytes bs)
                 t (first (:tracks song))]
             [(:recs t)
              [(:title song) (:timebase song) (:tempo song) (:time-sig song) (:ch t) (:st-shift t) (:comment t)
               (first (:rhythm-assign song)) (update (first (:user-exc song)) 1 #(vec (take 3 %)))
               (count (:load-warnings song)) (polysted.rcp/g36-bytes? (byte-array 600))]]))
    (check "鳴らない MODE の案内: Play・Mix は nil、Mute・Rec は案内"
           [nil "TR1 は MODE が Mute なので再生で鳴りません (Tab のトラック設定で MODE を Play か Mix に)" nil
            "TR1 は MODE が Rec なので再生で鳴りません (Tab のトラック設定で MODE を Play か Mix に)"]
           (map mute-hint (take 4 (iterate #(set-track-field % 0 :mode-next nil) (new-session)))))
    (check "replace-rec は undo 1回分、同じ値なら何もしない"
           [[[62 48 46 64]] 2 2]
           (let [s (-> (new-session) (insert-notes "C4") (replace-rec 0 [62 48 46 64]))]
             [(cur-recs s) (count (:undo s)) (count (:undo (replace-rec s 0 [62 48 46 64])))]))
    (check "ワンタッチ: ST 欄で F5 → ST48 GT45、次のノートへ"
           [[[60 48 45 64] [62 96 94 64]] 1]
           (let [s (-> (new-session) (insert-mml "l2 c d") (goto-pos 0) (one-touch 1 4 false))]
             [(cur-recs s) (:pos s)]))
    (check "ワンタッチ: VEL 欄、和音指定で直前の和音の音もそろえる"
           [[60 0 46 127] [64 0 46 127] [67 48 46 127]]
           (-> (new-session) (insert-recs [[60 0 46 64] [64 0 46 64] [67 48 46 64]])
               (goto-pos 2) (one-touch 3 9 true) cur-recs))
    (check "ワンタッチ: 和音指定なしなら、その行だけ。次が無ければ END へ"
           [[[60 0 46 64] [67 48 22 64]] 2]
           (let [s (-> (new-session) (insert-recs [[60 0 46 64] [67 48 46 64]])
                       (goto-pos 1) (one-touch 2 6 false))]
             [(cur-recs s) (:pos s)]))
    (check "小節線の挿入: カーソルは下へ、undo で戻る"
           [[[60 48 46 64] [0xFD 0 0 1] [62 48 46 64]] 2 [[60 48 46 64] [62 48 46 64]]]
           (let [s (-> (new-session) (insert-mml "c d") (goto-pos 1) insert-measure)]
             [(cur-recs s) (:pos s) (cur-recs (undo s))]))
    (check "リピート [ ] の挿入 (回数 4)"
           [[0xF9 0 0 0] [60 48 46 64] [0xF8 4 0 0]]
           (-> (new-session) insert-repeat-start (insert-notes "C4") (insert-repeat-end 4) cur-recs))
    (check "リピート回数 256 は不可 (何も変えず :msg)"
           [[] true]
           (let [s (insert-repeat-end (new-session) 256)] [(cur-recs s) (some? (:msg s))]))
    (check "SAME の挿入: 小節1 を指す"
           [[60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]]
           (-> (new-session) (insert-notes "C4") insert-measure (insert-same 1) cur-recs))
    (check "SAME は自分より前の小節しか指せない"
           [[[60 48 46 64] [0xFD 0 0 1]] true]
           (let [s (-> (new-session) (insert-notes "C4") insert-measure (insert-same 2))]
             [(cur-recs s) (some? (:msg s))]))
    (check "SAME の小節は指せない"
           true
           (some? (:msg (-> (new-session) (insert-notes "C4") insert-measure (insert-same 1) (insert-same 2)))))
    (check "表示: 小節線に小節の ST 合計、リピートと SAME の表記"
           ["[ REPEAT" "-------- MEAS END  ST=96" "======== SAME MEAS 1" "] REPEAT 2"]
           (->> (-> (new-session) insert-repeat-start (insert-mml "c d") insert-measure
                    (insert-same 1) (insert-repeat-end 2) table-view :rows)
                (remove :note?) (map :text) (take 4)))
    (check "小節の ST 合計: リピートの中は回数倍、小節の外で始まるリピートは割り戻す"
           [288 24]
           [(-> (new-session) insert-repeat-start (insert-mml "c d") (insert-repeat-end 2)
                (insert-mml "c d") insert-measure cur-recs (measure-st 6))
            (measure-st [[0xF9 0 0 0] [60 48 46 64] [0xFD 0 0 1] [62 24 22 64] [0xF8 3 0 0] [0xFD 0 0 2]] 5)])
    (check "範囲のコピー・削除・貼り付け (逆向きの指定も可)"
           [[[60 48 46 64] [62 48 46 64] [64 48 46 64]] [[62 48 46 64]]
            [[60 48 46 64] [64 48 46 64]] 1
            [[60 48 46 64] [62 48 46 64] [64 48 46 64]]]
           (let [s0 (-> (new-session) (insert-notes "C4 D E"))
                 s1 (copy s0 2 1)
                 s2 (cut s0 1 2)
                 s3 (-> s2 paste)]
             [(cur-recs s1) (:clip s1) (cur-recs s2) (:pos s2) (cur-recs s3)]))
    (check "二重化: カーソル行のコピーが入り、カーソルは元の行へ"
           [[[60 48 46 64] [60 48 46 64] [62 48 46 64]] 1]
           (let [s (-> (new-session) (insert-notes "C4 D") (goto-pos 0) duplicate-line)]
             [(cur-recs s) (:pos s)]))
    (check "1行削除 (delete-recs 1) で小節線を消すと後ろの SAME がずれる"
           [[60 48 46 64] [62 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]]
           (-> (new-session) (insert-recs [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [0xFD 0 0 2] [0xFC 1 0 0]])
               (goto-pos 1) (delete-recs 1) cur-recs))
    (check "コントローラの記号"
           [{:code 0xEB :ctl 7} {:code 0xEB :ctl 11} {:code 0xEB :ctl 93} {:code 0xEB :ctl 74}
            {:code 0xEA :ctl -1} {:code 0xEC :ctl 5} {:code 0xE2 :ctl -1} {:code 0xEE :ctl -1}
            {:code 0xEB :ctl 65} {:code 0xEB :ctl 20} {:code 0xEB :ctl 32} nil]
           (mapv spc-code ["V" "e" "F3" "CO" "C" "G5" "G@" "P" "PO" "20" "@L" "W"]))
    (check "コントローラの挿入: 音量・ベンド・音色 (1始まり)・テンポ・チャンネル"
           [[0xEB 0 7 100] [0xEE 12 0 32] [0xEC 0 48 0] [0xE7 0 80 0] [0xE6 0 18 0] [0xEB 0 20 5]]
           (-> (new-session) (insert-ctrl "V" 0 100) (insert-ctrl "P" 12 -4096) (insert-ctrl "G" 0 49)
               (insert-ctrl "T" 0 80) (insert-ctrl "M" 0 "B2") (insert-ctrl "L" 0 20 5) cur-recs))
    (check "コントローラの挿入: 範囲外は何もせず :msg"
           [[] true true]
           (let [a (insert-ctrl (new-session) "G" 0 129) b (insert-ctrl (new-session) "X" 0)]
             [(cur-recs a) (some? (:msg a)) (some? (:msg b))]))
    (check "コントローラの表示"
           [["VOLUME" "" "0" "7" "100"] ["PITCH" "" "12" "-4096" ""] ["PROGRAM" "" "0" "49" ""]
            ["MIDI CH." "" "0" "B2" ""]]
           (mapv ctrl-cells [[0xEB 0 7 100] [0xEE 12 0 32] [0xEC 0 48 0] [0xE6 0 18 0]]))
    (check "トラック移動: 同じ小節・同じ時刻の行へ、小節が無ければ末尾"
           [3 4 4]
           (let [s (-> (new-session)
                       (insert-recs [[60 48 46 64] [0xFD 0 0 1] [62 24 22 64] [64 24 22 64] [65 48 46 64]])
                       (add-track 2)
                       (insert-recs [[48 96 94 64] [0xFD 0 0 1] [50 48 46 64] [52 48 46 64] [0xFD 0 0 2]])
                       (select-track 0))]
             [(:pos (select-track-at (goto-pos s 4) 1))      ; 小節2 の ST 48 → 行3 (D3 の後)
              (:pos (select-track-at (goto-pos s 5) 1))      ; 末尾 (小節2 の ST 96) → 小節線の行4
              (:pos (dispatch (-> s (select-track 1) (goto-pos 3)) :track-at 0))]))  ; 小節2 の ST 48 → 行4 (dispatch 経由)
    (check "反転貼付・ミックス貼付"
           [[[60 48 46 64] [62 48 46 64] [64 48 46 64] [62 48 46 64] [60 48 46 64]]
            [[60 0 46 64] [67 24 22 64] [69 24 22 64] [62 48 46 64]]]
           [(-> (new-session) (insert-notes "C4 D E") (copy 0 2) paste-reversed cur-recs)
            (-> (new-session) (insert-notes "C4 D") (assoc :clip [[67 24 22 64] [69 24 22 64]])
                (goto-pos 0) mix-paste cur-recs)])
    (check "ミックス貼付: 末尾では後ろに足すだけ"
           [[60 48 46 64] [62 48 46 64] [60 48 46 64]]
           (-> (new-session) (insert-notes "C4 D") (assoc :clip [[60 48 46 64]]) mix-paste cur-recs))
    (check "RCP 書き出し → 読み込みの往復 (18 トラック、10 トラック目はリズム、小節線・SAME・コントローラ)"
           [18 [[0xEB 0 7 100] [60 48 46 64] [0xFD 1 0 0] [0xFC 0 44 0] [0xE6 0 3 0]] true [true 128] "テスト" 1]
           (let [s (-> (new-session) (insert-ctrl "V" 0 100) (insert-notes "C4") insert-measure
                       (insert-same 1) (insert-ctrl "M" 0 3)
                       (assoc-in [:song :title] "テスト"))
                 f (java.io.File/createTempFile "polysted" ".rcp")
                 _ (polysted.rcp/write-rcp! (:song s) (.getPath f))
                 s2 (song-from-rcp (.getPath f))]
             (.delete f)
             [(count (:tracks s2)) (:recs (first (:tracks s2)))
              (= (:recs (first (:tracks s2)))
                 (vec (butlast (polysted.rcp/normalize-track-records (cur-recs s)))))
              ((juxt :rhythm? :key-shift) ((:tracks s2) 9)) (:title s2) (:ch (first (:tracks s2)))]))
    (check "R36: 拡張子 .r36 なら 36 トラック、ヘッダのトラック数も 36"
           [36 36]
           (let [b (polysted.rcp/song->rcp-bytes (:song (new-session)) true)
                 f (java.io.File/createTempFile "polysted" ".r36")]
             (java.nio.file.Files/write (.toPath f) ^bytes b (into-array java.nio.file.OpenOption []))
             (let [r (count (:tracks (song-from-rcp (.getPath f))))]
               (.delete f)
               [r (bit-and (aget ^bytes b 0x1E6) 0xFF)])))
    (check "SMF の往復: MML の曲 → SMF 書き出し → 読み込み (最後の音の ST は終端までの時間)"
           [120 [[60 24 22 64] [62 24 22 64] [64 24 22 64] [0xEB 0 7 100] [65 48 46 100]]]
           (let [s (-> (new-session) (insert-mml "l8 c d e") (insert-ctrl "V" 0 100) (insert-mml "v100 l4 f"))
                 f (java.io.File/createTempFile "polysted" ".mid")
                 _ (polysted.mml/write-mid! (->song s) (.getPath f))
                 {:keys [song]} (polysted.smf/read-smf (.getPath f))]
             (.delete f)
             [(:tempo song) (let [r (:recs (first (:tracks song)))] (conj (vec (butlast r)) (assoc (peek r) 1 48)))]))
    (check "ノート操作: 直線 (undo 1回)、値の検査"
           [[40 70 100] [1 1 1] true]
           (let [s (-> (new-session) (insert-notes "C4 D E") (insert-recs []) (goto-pos 0))
                 s (assoc-in s [:song :tracks 0 :recs] [[60 48 46 1] [62 48 46 1] [64 48 46 1]])
                 s2 (note-ctrl s 0 3 :line 40 100)]
             [(mapv #(% 3) (cur-recs s2)) (mapv #(% 3) (cur-recs (undo s2)))
              (some? (:msg (note-ctrl s 0 3 :strum-gt 0 nil)))]))
    (check "置換・指定削除・検索 (undo で戻る)"
           [[63 62 63] [60 62 60] "2 か所を置換しました" [[62 48 46 64]] 2 "見つかりません"]
           (let [s (-> (new-session) (insert-notes "C4 D C4") (goto-pos 0))
                 s1 (replace-range s 0 3 "C4:D#4" :replace)]
             [(mapv first (cur-recs s1)) (mapv first (cur-recs (undo s1))) (:msg s1)
              (cur-recs (replace-range s 0 3 "C4" :delete))
              (:pos (find-next s "C4" true)) (:msg (find-next (goto-pos s 2) "C4" true))]))
    (check "SAME 自動変換・展開 (カーソルは同じ小節・時刻へ、undo で戻る)"
           [[[60 48 46 64] [0xFD 0 0 1] [0xFC 0 0 0]] 2 "1 小節を SAME に変換しました" 4 4]
           (let [s (-> (new-session) (insert-recs [[60 48 46 64] [0xFD 0 0 1] [60 48 46 64] [0xFD 0 0 2]]) (goto-pos 3))
                 s1 (same-convert s)
                 s2 (same-extract s1)]
             [(cur-recs s1) (:pos s1) (:msg s1) (count (cur-recs (undo s1))) (count (cur-recs s2))]))
    (check "レプリケート: TR1 の小節 1 を 2 回、+12 でカーソル位置へ (クリップボードにも)"
           [[[60 48 46 64] [0xFD 0 0 1] [72 48 46 64] [0xFD 0 0 2] [72 48 46 64] [0xFD 0 0 3]] [[72 48 46 64] [0xFD 0 0 1]] true]
           (let [s (-> (new-session) (insert-recs [[60 48 46 64] [0xFD 0 0 1]]))
                 s1 (replicate s 0 1 1 2 12)]
             [(cur-recs s1) (:clip s1) (some? (:msg (replicate s 0 1 1 0 0)))]))
    (check "全トラックのレプリケートと、範囲のレプリケート (T)"
           [[[60 48 46 64] [0xFD 0 0 1] [60 48 46 64] [0xFD 0 0 2]] [[48 96 94 64] [0xFD 0 0 1] [48 96 94 64] [0xFD 0 0 2]]
            [[62 48 46 64] [0xFD 0 0 1] [48 96 94 64] [0xFD 0 0 2]]]
           (let [s (-> (new-session) (insert-recs [[60 48 46 64] [0xFD 0 0 1]])
                       (add-track 2) (insert-recs [[48 96 94 64] [0xFD 0 0 1]]) (select-track 0))
                 s1 (replicate-all s 1 1 1 0)
                 s2 (-> (new-session) (insert-notes "D4") (add-track 2) (insert-recs [[48 96 94 64] [0xFD 0 0 1]]) (select-track 0))
                 s3 (replicate-range s2 0 1 1 1 1 0)]
             [(:recs ((get-in s1 [:song :tracks]) 0)) (:recs ((get-in s1 [:song :tracks]) 1))
              (:recs ((get-in s3 [:song :tracks]) 1))]))
    (check "ループ圧縮 (今のトラック / 全トラック) と、演奏内容が変わらないこと"
           [[[0xF9 0 0 0] [60 12 10 64] [0xF8 4 0 0]] [[48 96 94 64]] 2 true]
           (let [s (-> (new-session) (insert-recs (vec (repeat 4 [60 12 10 64])))
                       (add-track 2) (insert-recs (vec (repeat 4 [62 12 10 64]))) (select-track 0))
                 s1 (repeat-compress s)
                 s2 (repeat-compress s true)
                 play (fn [s] (->> (->song s) :tracks (mapcat #(polysted.mml/track->abs (polysted.mml/expand-track %)))
                                   (filter #(= :note (:kind %))) (map (juxt :tick :key)) sort))]
             [(cur-recs s1) [[48 96 94 64]] (count (filter #(= 0xF9 (% 0)) (mapcat :recs (get-in s2 [:song :tracks]))))
              (= (play s) (play s2))]))
    (check "トラック設定: 調の表示と入力、ST+/K#+ の符号、MODE と RHY の切り替え"
           ["G MAJOR(#1)" "D minor(b1)" [1 25 9 0 nil] ["-5" "OFF" "ON" "ON+" "OFF" "Mute"] [3 4] "テンポは 1〜255 です"]
           (let [s (new-session)
                 s1 (-> s (set-track-field 0 :key-shift "-5") (set-track-field 0 :st-shift "-3")
                        (set-track-field 0 :mute? nil) (set-track-field 0 :rhy nil))
                 t1 ((get-in s1 [:song :tracks]) 0)
                 t2 ((get-in (set-track-field s1 0 :rhy nil) [:song :tracks]) 0)
                 t3 ((get-in (-> s1 (set-track-field 0 :rhy nil) (set-track-field 0 :rhy nil)) [:song :tracks]) 0)]
             [(key-str 1) (key-str 25) (mapv parse-key ["G" "Dm" "F" "C" "H"])
              [(key-shift-str (:key-shift t1)) (key-shift-str 128) (rhy-str (:rhy-byte t1)) (rhy-str (:rhy-byte t2)) (rhy-str (:rhy-byte t3)) (mode-str t1)]
              (:time-sig (:song (set-song-field s :beat "3/4"))) (:msg (set-song-field s :tempo "300"))]))
    (check "再生: K#+ でキーをずらす (OFF・範囲外は鳴らさない)、ST+ で時間をずらす、リズムトラックも鳴らす"
           [[[0 55] [48 121]] [[0 65]] [[10 60]] [[0 60]] 2 48]
           (let [notes (fn [s] (->> (polysted.mml/->sequence (->song s)) .getTracks
                                    (mapcat (fn [^javax.sound.midi.Track t] (for [i (range (.size t)) :let [e (.get t i) m (.getMessage e)]
                                                                               :when (and (instance? javax.sound.midi.ShortMessage m)
                                                                                          (= javax.sound.midi.ShortMessage/NOTE_ON (.getCommand ^javax.sound.midi.ShortMessage m)))]
                                                                           [(.getTick e) (.getData1 ^javax.sound.midi.ShortMessage m)])))
                                    vec))
                 base (-> (new-session) (insert-recs [[60 48 46 64] [126 48 46 64]]))]
             [(notes (assoc-in base [:song :tracks 0 :key-shift] 0x7B))           ; -5: 60→55、126→121
              (notes (assoc-in base [:song :tracks 0 :key-shift] 5))              ; +5: 126→131 は範囲外で鳴らさない
              (notes (-> (new-session) (insert-notes "C4") (assoc-in [:song :tracks 0 :st-shift] 10)))
              (notes (-> (new-session) (insert-notes "C4") (assoc-in [:song :tracks 0 :st-shift] 246)))   ; -10 → 0 で止める
              (count (:tracks (->song (-> (new-session) (add-track 10) (assoc-in [:song :tracks 1 :rhythm?] true)))))
              (measure-tick (-> (new-session) (insert-recs [[60 48 46 64] [0xFD 0 0 1] [62 48 46 64] [64 48 46 64]]) (goto-pos 3)))]))
    (check "小節プレイの範囲: 2小節目は tick 48〜144 (リピート 2 回を含む)"
           [[48 144] [0 48]]
           (let [s (-> (new-session) (insert-recs [[60 48 46 64] [0xFD 0 0 1] [0xF9 0 0 0] [62 48 46 64] [0xF8 2 0 0] [0xFD 0 0 2]]))]
             [(measure-span (goto-pos s 3)) (measure-span (goto-pos s 0))]))
    (check "エクスクルーシブ: Roland チェックサム、トラック/ユーザー/Roland 形式を SysEx に"
           [[0xF0 0x41 0x10 0x16 0x12 0x10 0x00 0x01 0x64 0x0B 0xF7]
            [0xF0 0x41 0x10 0x16 0x12 0x10 0x00 0x01 0x64 0x0B 0xF7]
            [0xF0 0x41 0x10 0x42 0x12 0x40 0x01 0x30 0x05 0x0A 0xF7]
            [0xF0 0x43 0x05 0x40 0xF7]
            "Tr.Exclu  41 10 16 12 cs 10 00 01 64 ss F7"]
           (let [recs [[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x16 0x12] [0xF7 0 0x83 0x10]
                       [0xF7 0 0x00 0x01] [0xF7 0 0x64 0x84] [0xF7 0 0xF7 0xF7]]
                 ev (polysted.rcp/records->events-ex
                     (into recs [[0xDF 0 0x10 0x42] [0xDD 0 0x40 0x01] [0xDE 0 0x30 0x05] [0x90 0 5 0x40]])
                     {:user-exc [["" [0x43 0x80 0x81 0xF7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]] :ch 1})
                 sx (filterv #(= :sysex (:type %)) ev)]
             [(polysted.rcp/exclusive-bytes [0x41 0x10 0x16 0x12 0x83 0x10 0x00 0x01 0x64 0x84 0xF7] {})
              (:bytes (sx 0)) (:bytes (sx 1)) (:bytes (sx 2))
              (:text (first (:rows (table-view (-> (new-session) (insert-recs recs) (goto-pos 0)) 3))))]))
    (check "エクスクルーシブを SMF に書き出す (SysexMessage)"
           [[0xF0 0x41 0x10 0x16 0x12 0x10 0x00 0x01 0x64 0x0B 0xF7]]
           (let [s (-> (new-session) (insert-recs [[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x16 0x12] [0xF7 0 0x83 0x10]
                                                   [0xF7 0 0x00 0x01] [0xF7 0 0x64 0x84] [0xF7 0 0xF7 0xF7] [60 48 46 64]]))]
             (vec (for [^javax.sound.midi.Track t (.getTracks (polysted.mml/->sequence (->song s)))
                        i (range (.size t))
                        :let [m (.getMessage (.get t i))]
                        :when (instance? javax.sound.midi.SysexMessage m)]
                    (mapv #(bit-and % 0xFF) (.getMessage m))))))
    (check "エクスクルーシブの SMF 往復 (書き出し → 読み込みで同じ行に戻る)"
           [[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x42 0x12] [0xF7 0 0xF7 0xF7]]
           (let [s (-> (new-session) (insert-recs [[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x42 0x12] [0xF7 0 0xF7 0xF7] [60 48 46 64]]))
                 f (java.io.File/createTempFile "polysted" ".mid")
                 _ (polysted.mml/write-mid! (->song s) (.getPath f))
                 song (:song (polysted.smf/read-smf (.getPath f)))]
             (.delete f)
             (vec (take 4 (:recs (first (:tracks song)))))))
    (check "エクスクルーシブの入力: 新規挿入・書き換え (undo 1回)、不正な入力、ユーザーエクスクルーシブの表"
           [[[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x42 0x12] [0xF7 0 0x83 0x40] [0xF7 0 0x84 0xF7] [60 48 46 64]]
            [[0x98 0 0 0] [0xF7 0 0x43 0xF7] [60 48 46 64]]
            "41 10 42 12 cs 40 ss F7" true
            [0x41 0x80 0x81 0xF7 0xF7]]
           (let [s0 (-> (new-session) (insert-notes "C4") (goto-pos 0))
                 s1 (edit-exclusive s0 "41 10 42 12 cs 40 ss")
                 s2 (edit-exclusive s1 "43")
                 s3 (set-user-exc s0 2 "41 gt ve")]
             [(cur-recs s1) (cur-recs s2) (exclusive-text s1 0)
              (some? (:msg (edit-exclusive s0 "41 XX")))
              (vec (take 5 (second (get-in s3 [:song :user-exc 2]))))]))
    (check "/ の記号: U0 X B R I"
           [0x90 0x98 0xDD 0xDE 0xDF [0x90 0 5 64]]
           [(:code (spc-code "U0")) (:code (spc-code "X")) (:code (spc-code "B")) (:code (spc-code "R")) (:code (spc-code "I"))
            (first (cur-recs (insert-ctrl (new-session) "U0" 0 5 64)))])
    (check "リズム: 空の小節に BD を置く (16 ステップ、表は既定を記録)、新しい小節"
           [[[36 12 1 112] [0 12 0 0]] 16 "Hand Clap" 3]
           (let [s (-> (new-session) (insert-recs [[0xFD 0 0 1]]) (assoc-in [:song :tracks 0 :rhythm?] true))
                 s1 (rhythm-update s 2 #(rhythm-set-hit % 0 15 112 1))
                 s2 (rhythm-new-measure s1)]
             [(vec (take 2 (drop 1 (cur-recs s1)))) (count (:steps (rhythm-pattern s 2)))
              (first (first (get-in s1 [:song :rhythm-assign]))) (:rhy-meas s2)]))
    (check "削除行バッファ: 1行削除して Ctrl-L で復活、エクスクルーシブは続きの行ごと"
           [[[62 48 46 64]] [[60 48 46 64] [62 48 46 64]] [[60 48 46 64]] [[0x98 0 0 0] [0xF7 0 0x41 0xF7] [60 48 46 64]]]
           (let [s (-> (new-session) (insert-notes "C4 D") (goto-pos 0) (delete-recs 1))
                 e (-> (new-session) (insert-recs [[0x98 0 0 0] [0xF7 0 0x41 0xF7] [60 48 46 64]]) (goto-pos 0) (delete-recs 1))]
             [(cur-recs s) (cur-recs (restore-line s)) (cur-recs e) (cur-recs (restore-line e))]))
    (check "コード入力: ノートの行、END の上なら前のノート、展開してカーソルは最後の音"
           [0 1 [[62 0 46 64] [65 0 46 64] [69 48 46 64]] 2]
           (let [s (-> (new-session) (insert-notes "D4"))
                 s2 (insert-chord s 0 [62 65 69])]
             [(chord-target (goto-pos s 0)) (:pos s) (cur-recs s2) (:pos s2)]))
    (check "リズム割り当て: Acou BD (楽器16) を 35 に変えると RHY トラックの 36 の音も 35 に (RHY でないトラックはそのまま)"
           [["Kick" 35 2] [35 48 1 100] [36 48 1 100] true]
           (let [s (-> (new-session) (insert-recs [[36 48 1 100]]) (assoc-in [:song :tracks 0 :rhythm?] true)
                       (add-track 2) (insert-recs [[36 48 1 100]]) (select-track 0))
                 s1 (set-rhythm-assign s 15 "35 2 Kick")]
             [(get-in s1 [:song :rhythm-assign 15]) (first (:recs (get-in s1 [:song :tracks 0])))
              (first (:recs (get-in s1 [:song :tracks 1]))) (some? (:msg (set-rhythm-assign s 15 "Q 1 x")))]))
    (check "EDN: セッションの曲を保存形式に通しても同じ"
           (:song (-> (new-session) (insert-mml "o5 c8") (insert-notes "E G")))
           (-> (new-session) (insert-mml "o5 c8") (insert-notes "E G")
               :song polysted.edn/song->edn-str polysted.edn/edn-str->song))))

