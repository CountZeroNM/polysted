;; polysted_ctrl.clj — コントローラ・エクスクルーシブ・トラック設定の値の解釈と表示 (セッション状態を使わない純粋関数)
;;
;; polysted_session.clj から分けたもの (2026-11-18)。polysted-session は :refer :all で取り込む。
;;   - コントローラの記号 (EDITS.C spc_code)、入力の欄、値の検査、表示 (trk_dis・ctrl_type)
;;   - MIDI チャンネル A1〜B16 (chstr / strch)、調の表示と入力 (scale_str)、ST+ / K#+ / RHY の表示
;;   - エクスクルーシブの入力の解釈と表示 (gt ve ch cs ss)、リズム割り当ての入力
;; 読み込み: (load-file "polysted_ctrl.clj")  ※ polysted_notename.clj の後。ロード時に run-checks が走る
(ns polysted-ctrl
  (:require [clojure.string :as str]
            [polysted-rcp]
            [polysted-notename]))

(def ^:private cc-names
  {0 "BANK MSB" 1 "MODULAT" 2 "BREATH" 4 "FOOT C." 5 "PORTA.TM" 6 "DATA MSB" 7 "VOLUME"
   8 "BALANCE" 10 "PANPOT" 11 "EXPRESS" 32 "BANK LSB" 33 "MODULA.L" 34 "BREATH.L"
   36 "FOOT C.L" 37 "PORTA.TL" 38 "DATA LSB" 39 "VOLUME.L" 40 "BALANC.L" 42 "PANPOT.L"
   43 "EXPRES.L" 64 "HOLD1" 65 "PORTAMEN" 66 "SOSTENUT" 67 "SOFT" 69 "HOLD2" 71 "Resonanc"
   72 "Rel.Time" 73 "Att.Time" 74 "Cutoff F" 84 "POR.CONT" 89 "VariSend" 90 "DryLevel"
   91 "REVERB" 92 "EFFECT2" 93 "CHORUS" 94 "DELAY" 95 "EFFECT5" 96 "DATA INC" 97 "DATA DEC"
   98 "NRPN LSB" 99 "NRPN MSB" 100 "RPN  LSB" 101 "RPN  MSB" 120 "SOND OFF" 121 "RES.ALL"
   122 "LOCAL C." 123 "NOTE OFF" 124 "OMNI OFF" 125 "OMNI ON" 126 "MONO Mod" 127 "POLY Mod"})

(defn ctrl-name [cc] (get cc-names (bit-and cc 127) "CONTROL"))

(defn ch-str "MIDI チャンネル 1〜32 → A1〜B16、それ以外は OFF (chstr)" [ch]
  (if (<= 1 ch 32) (str (if (<= ch 16) "A" "B") (inc (mod (dec ch) 16))) "OFF"))

(defn parse-ch "A1〜B16 か 0〜32 (0 は OFF) → 0〜32。不正なら nil (strch を厳しめにしたもの)" [text]
  (let [t (str/upper-case (str/trim text))]
    (if-let [[_ ab n] (re-matches #"([AB])(\d{1,2})" t)]
      (let [n (Long/parseLong n)] (when (<= 1 n 16) (+ n (if (= ab "B") 16 0))))
      (when (re-matches #"\d{1,2}" t) (let [n (Long/parseLong t)] (when (<= n 32) n))))))

(def supported-ctrls (into #{0xEB 0xEC 0xE2 0xE6 0xE7 0xEA 0xED 0xEE 0x98 0xDD 0xDE 0xDF} (range 0x90 0x98)))

(defn spc-code
  "コントローラの記号 → {:code コード :ctl 番号 (無ければ -1)}。未知・未対応なら nil (spc_code)。
   V 音量 / E エクスプレッション / H ホールド / D モジュレーション / N パン / A リセットオール /
   F1〜F5 エフェクト / 数字・Ln: CC 番号 / @ バンク MSB / @L バンク LSB / P ベンド / G[n] 音色 /
   G@[n] バンク付き音色 / T テンポ / M チャンネル / C アフタータッチ / K キープレッシャー など"
  [text]
  (let [s  (str/upper-case (str/trim text))
        s  (if (str/starts-with? s "/") (subs s 1) s)
        a  (first s)
        b  (second s)
        num (fn [t] (when (re-matches #"\d{1,3}" t) (Long/parseLong t)))
        ;; 2文字の記号を先に見る (原作は後から上書きするので同じ結果になる)
        c2 (cond
             (and (= a \@) (#{\@ \L} b)) 32
             (= a \@) 0
             (and (= a \F) b (<= (int \1) (int b) (int \5))) (+ 91 (- (int b) (int \1)))
             (and (= a \P) (= b \T)) 5 (and (= a \P) (= b \O)) 65 (and (= a \P) (= b \C)) 84
             (and (= a \S) (= b \S)) 66 (and (= a \S) (= b \T)) 67
             (and (= a \E) (= b \M)) 6 (and (= a \E) (= b \L)) 38
             (and (= a \N) (= b \M)) 99 (and (= a \N) (= b \L)) 98
             (and (= a \R) (= b \M)) 101 (and (= a \R) (= b \L)) 100
             (and (= a \B) (= b \R)) 2
             (and (= a \R) (= b \S)) 71 (and (= a \R) (= b \T)) 72
             (and (= a \A) (= b \T)) 73 (and (= a \C) (= b \O)) 74
             (= a \D) 1 (= a \V) 7 (= a \N) 10 (= a \E) 11 (= a \H) 64 (= a \A) 121
             :else -1)]
    (cond
      (nil? a) nil
      (>= c2 0) {:code 0xEB :ctl c2}
      (= a \L) (if (= s "L")
                 {:code 0xEB :ctl -1}                     ; 番号はあとで入力
                 (let [n (num (subs s 1))] (when (and n (<= n 127)) {:code 0xEB :ctl n})))
      (Character/isDigit (char a)) (let [n (num s)] (when (and n (<= n 127)) {:code 0xEB :ctl n}))
      (= a \G) (if (#{\@ \M} b)
                 {:code 0xE2 :ctl (or (num (subs s 2)) -1)}
                 {:code 0xEC :ctl (or (num (subs s 1)) -1)})
      (= a \T) {:code 0xE7 :ctl -1}
      (= a \C) {:code 0xEA :ctl -1}
      (= a \K) {:code 0xED :ctl -1}
      (= a \P) {:code 0xEE :ctl -1}
      (= a \M) {:code 0xE6 :ctl -1}
      (and (= a \U) b (<= (int \0) (int b) (int \7))) {:code (+ 0x90 (- (int b) (int \0))) :ctl -1}   ; ユーザーエクスクルーシブ
      (= a \X) {:code 0x98 :ctl -1}                                     ; トラックエクスクルーシブ
      (= a \B) {:code 0xDD :ctl -1}                                     ; Roland ベースアドレス
      (= a \R) {:code 0xDE :ctl -1}                                     ; Roland アドレス・パラメータ
      (= a \I) {:code 0xDF :ctl -1}                                     ; Roland デバイス番号・モデル ID
      :else nil)))

(defn ctrl-insert-fields
  "挿入時に順に入力する欄 [[cx 種類] ...] (spcon)。
   キープレッシャー 0xED は値 (byte3) も入力する【設計判断。原作は byte3 を聞かない】"
  [{:keys [code ctl]}]
  (let [ctl? (>= ctl 0)]
    (case (int code)
      0xEB (if ctl? [[1 :st] [3 :val]] [[1 :st] [2 :cc] [3 :val]])
      0xEC (if ctl? [[1 :st]] [[1 :st] [2 :prog]])
      0xE2 (if ctl? [[1 :st] [3 :val]] [[1 :st] [2 :prog] [3 :val]])
      0xE6 [[1 :st] [2 :ch]]
      0xE7 [[1 :st] [2 :tempo] [3 :byte]]
      0xEE [[1 :st] [2 :bend]]
      (0x90 0x91 0x92 0x93 0x94 0x95 0x96 0x97 0xDD 0xDE 0xDF) [[1 :st] [2 :val] [3 :val]]
      [[1 :st] [2 :val] [3 :val]])))

(defn ctrl-over-field
  "既存のコントローラ行で欄 cx を数字で上書きするときの [cx 種類]。対象外なら nil (trk_ed の 0〜9)"
  [rec cx]
  (let [c (rec 0)]
    (when (and (<= 0x80 c 0xEF) (not= c 0x98) (pos? cx))
      (cond
        (= cx 1)   [1 :st]
        (= c 0xEE) [2 :bend]
        (= c 0xEC) [2 :prog]
        (and (= c 0xE2) (= cx 2)) [2 :prog]
        (and (= c 0xE6) (= cx 2)) [2 :ch]
        (= c 0xE7) [cx :byte]
        (and (= c 0xEB) (= cx 2)) [2 :cc]
        :else      [cx :val]))))

(def ctrl-field-labels
  {:st "ST" :cc "CC番号" :val "値" :byte "値" :tempo "テンポ(64=100%)" :prog "音色(1-128)"
   :ch "MIDI CH(A1-B16, 0=OFF)" :bend "ベンド(-8192〜8191)"})

(defn ctrl-field-text "欄の今の値の表示" [rec cx type]
  (case type
    :prog (str (inc (rec 2)))
    :ch   (ch-str (rec 2))
    :bend (str (- (+ (rec 2) (* 128 (rec 3))) 8192))
    :cc   (if (> (rec 2) 127) "" (str (rec 2)))
    (str (rec cx))))

(defn set-ctrl-field
  "欄 cx (種類 type) に入力文字列を入れたレコード。空なら今の値のまま。不正なら nil"
  [rec cx type text]
  (let [t   (str/trim text)
        n   (when (re-matches #"-?\d{1,5}" t) (Long/parseLong t))
        clamp (fn [lo hi v] (max lo (min hi v)))]
    (cond
      (and (str/blank? t) (not (and (= type :cc) (> (rec 2) 127)))) rec
      (= type :ch)   (when-let [ch (parse-ch t)] (assoc rec 2 ch))
      (nil? n)       nil
      (= type :bend) (when (<= -8192 n 8191)
                       (let [v (+ n 8192)] (assoc rec 2 (bit-and v 127) 3 (bit-shift-right v 7))))
      (neg? n)       nil
      :else
      (case type
        :st    (assoc rec 1 (clamp 0 255 n))
        :cc    (when (<= n 127) (assoc rec 2 n))
        :val   (assoc rec cx (clamp 0 127 n))
        :byte  (assoc rec cx (clamp 0 255 n))
        :tempo (assoc rec 2 (clamp 1 255 n))
        :prog  (when (<= 1 n 128) (assoc rec 2 (dec n)))))))

(defn ctrl-cells
  "表示用の [名前 \"\" ST byte2 byte3]。対応していないコードなら nil (trk_dis の表記)"
  [[c b1 b2 b3]]
  (case (int c)
    0xEB [(ctrl-name b2) "" (str b1) (str (bit-and b2 127)) (str b3)]
    0xEC ["PROGRAM" "" (str b1) (str (inc b2)) ""]
    0xE2 ["BankProg" "" (str b1) (str (inc b2)) (str b3)]
    0xE6 ["MIDI CH." "" (str b1) (ch-str b2) ""]
    0xE7 ["TEMPO" "" (str b1) (str b2) (str b3)]
    0xEA ["AFTER C." "" (str b1) (str b2) ""]
    0xED ["AFTER K." "" (str b1) (str b2) (str b3)]
    0xEE ["PITCH" "" (str b1) (str (- (+ b2 (* 128 b3)) 8192)) ""]
    (0x90 0x91 0x92 0x93 0x94 0x95 0x96 0x97) [(str "UserExc" (- c 0x90)) "" (str b1) (str b2) (str b3)]
    0xDD ["Rol.Base" "" (str b1) (str b2) (str b3)]
    0xDE ["Rol.Para" "" (str b1) (str b2) (str b3)]
    0xDF ["Rol.Dev#" "" (str b1) (str b2) (str b3)]
    nil))

(defn parse-exc-data
  "エクスクルーシブの入力 → データのバイト列 (F7 で終わる)。16進 (41 / 41H / $41) と
   gt ve ch cs ss (0x80〜0x84)、F7 (終わり、省略可)。不正なら nil"
  [text]
  (let [toks (remove str/blank? (str/split (str/trim (str text)) #"[\s,]+"))
        kw {"GT" 0x80 "VE" 0x81 "CH" 0x82 "CS" 0x83 "SS" 0x84}
        bs (mapv (fn [t] (let [u (str/upper-case t)]
                           (or (kw u)
                               (when-let [[_ h] (re-matches #"\$?([0-9A-F]{1,2})H?" u)]
                                 (let [v (Long/parseLong h 16)] (when (or (<= v 0x7F) (= v 0xF7)) v))))))
                 toks)
        bs (vec (take-while #(not= % 0xF7) bs))]
    (when (and (seq toks) (every? some? bs) (not-any? nil? (map #(let [u (str/upper-case %)] (or (kw u) (re-matches #"\$?[0-9A-F]{1,2}H?" u))) toks)))
      (conj bs 0xF7))))

(defn parse-rhythm-assign
  "割り当ての入力 \"キー GT 名前\" → [キー GT 名前] か nil。キーは音名か 24〜127、0 か OFF は無し"
  [text]
  (let [[k g & nm] (str/split (str/trim (str text)) #"\s+")
        key (cond (nil? k) nil
                  (#{"0" "OFF" "off"} k) 0
                  :else (let [v (polysted-notename/parse-token k {:last-key 60 :key-sig 0})] (when (and v (<= 24 v 127)) v)))
        gt  (when (and g (re-matches #"\d{1,3}" g)) (Long/parseLong g))]
    (when (and key gt (<= gt 255))
      [key gt (str/join " " nm)])))

(def ^:private key-names
  ["C" "G" "D" "A" "E" "B" "F#" "C#" "C" "F" "Bb" "Eb" "Ab" "Db" "Gb" "Cb"
   "A" "E" "B" "F#" "C#" "G#" "D#" "A#" "A" "D" "G" "C" "F" "Bb" "Eb" "Ab"])

(defn key-str "調の1バイト → 表示 (scale_str): \"G MAJOR(#1)\" \"D minor(b1)\"" [k]
  (let [k (bit-and k 31) n (bit-and k 7)]
    (str (key-names k) (if (< k 16) " MAJOR" " minor")
         (cond (zero? n) "" (bit-test k 3) (str "(b" n ")") :else (str "(#" n ")")))))

(defn parse-key
  "調の入力 → 1バイト。0〜31 の数、または音名 (C G D A E B F# C# F Bb Eb Ab Db Gb Cb) に m を付けると短調"
  [text]
  (let [t (str/trim (str text))]
    (cond
      (re-matches #"\d{1,2}" t) (let [n (Long/parseLong t)] (when (<= n 31) n))
      :else (let [minor? (and (> (count t) 1) (str/ends-with? t "m"))
                  nm (str/upper-case (if minor? (subs t 0 (dec (count t))) t))
                  nm (if (> (count nm) 1) (str (subs nm 0 1) (str/lower-case (subs nm 1))) nm)
                  base (if minor? 16 0)
                  idx (some (fn [i] (when (= (key-names (+ base i)) nm) i)) (concat (range 8) (range 9 16)))]
              (when idx (+ base idx))))))

(defn st-shift-val "ST+ の1バイト → 符号付き" [b] (let [b (or b 0)] (if (> b 127) (- b 256) b)))

(defn key-shift-str "K#+ の1バイト → 表示 (128 以上は OFF)" [b]
  (let [b (or b 0)] (cond (>= b 128) "OFF" (> b 63) (str (- b 128)) :else (str b))))

(defn rhy-str [b] (let [b (or b 0)] (cond (< b 0x80) "OFF" (= b 0x80) "ON" :else "ON+")))

(defn track-mode "トラックの MODE 0 Play 1 Mute 2 Mix 3 Rec (原作 trmod の下位 2 ビット。bit0 は鳴らさない、bit1 は録音する)" [t]
  (bit-or (if (:mute? t) 1 0) (bit-and (or (:mode t) 0) 2)))
(defn mode-str [t] (["Play" "Mute" "Mix" "Rec"] (track-mode t)))

(defn exc-hex
  "エクスクルーシブのデータの表示 (特殊コードは gt ve ch cs ss、F7 は終わり)"
  [data]
  (str/join " " (map (fn [x] (case (int x) 0x80 "gt" 0x81 "ve" 0x82 "ch" 0x83 "cs" 0x84 "ss" (format "%02X" x))) data)))

;; ---- ユーザーエクスクルーシブの表の編集 (EXCLU.C user_exc_as・user_exc_dis・excins・excdel・excsum) ----
(defn uex-data
  "表のデータを 24 バイトに (先頭と末尾がともに 0 なら空 = 全部 F7、最初の F7 より後は F7。user_exc_dis)"
  [data]
  (let [d (vec (take 24 (concat data (repeat 0xF7))))]
    (if (and (zero? (d 0)) (zero? (d 23)))
      (vec (repeat 24 0xF7))
      (let [n (or (first (keep-indexed (fn [i v] (when (= v 0xF7) i)) d)) 24)]
        (vec (concat (subvec d 0 n) (repeat (- 24 n) 0xF7)))))))

(defn uex-ins "x に v を入れ、後ろへずらす (24 バイトを超えた分は捨てる。excins)" [d x v]
  (vec (take 24 (concat (subvec d 0 x) [v] (subvec d x)))))

(defn uex-del "x を消して前へ詰め、最後を F7 に (excdel)" [d x]
  (conj (vec (concat (subvec d 0 x) (subvec d (inc x)))) 0xF7))

(defn uex-sum
  "x にチェックサムの値を入れる (原作の F8: excins してから excsum。合計は 5 バイト目から、
   先頭が 0x43 (ヤマハ) なら 4 バイト目から x の手前まで)"
  [d x]
  (let [d (uex-ins d x 0) top (if (= 0x43 (d 0)) 3 4)]
    (assoc d x (bit-and (- 128 (bit-and (reduce + (subvec d top x)) 0x7F)) 0x7F))))

(defn uex-cells "表示の 24 マス (16進、gt ve ch cs ss、最初の F7、その後は ..)" [d]
  (let [n (or (first (keep-indexed (fn [i v] (when (= v 0xF7) i)) d)) 24)]
    (vec (for [i (range 24)]
           (cond (< i n) (let [v (d i)] (case (int v) 0x80 "gt" 0x81 "ve" 0x82 "ch" 0x83 "cs" 0x84 "ss" (format "%02X" v)))
                 (= i n) "F7"
                 :else "..")))))

;; ---- エクスクルーシブのテキストファイル .EXC (EXCLU.C exc_load / exc_save) ----
(defn parse-exc-text
  "EXC ファイルのテキスト → メッセージのデータの列 [[41 10 …] …] (F0・F7 は除く)。
   * から行末までは注釈、空白を飛ばして 2 文字ずつ 16進か gt ve ch cs ss。最初が F0 でなければ空。不正なら nil"
  [text]
  (let [toks (for [l (str/split-lines (str text))
                   :let [u (str/upper-case (str/replace (first (str/split l #"\*" 2)) #"[\x00-\x1F]" " "))]
                   t (loop [i 0 out []]
                       (cond (>= i (count u)) out
                             (= \space (.charAt u i)) (recur (inc i) out)
                             :else (recur (+ i 2) (conj out (str/trim (subs u i (min (count u) (+ i 2))))))))]
               t)
        val (fn [t] (case t "GT" 0x80 "VE" 0x81 "CH" 0x82 "CS" 0x83 "SS" 0x84
                      (when (re-matches #"[0-9A-F]{1,2}" t) (Long/parseLong t 16))))
        bs (mapv val toks)]
    (when (every? some? bs)
      (loop [bs bs msgs []]
        (if (= 0xF0 (first bs))
          (let [[m more] (split-with #(not= % 0xF7) (rest bs))]
            (if (every? #(or (<= % 0x7F) (<= 0x80 % 0x84)) m)
              (recur (rest more) (conj msgs (vec m)))
              nil))
          msgs)))))

(defn exc-file-text
  "メッセージの列 → EXC ファイルのテキスト (exc_save: F0 … F7、1 行 16 個まで、メッセージの間は空行、CRLF)"
  [msgs]
  (str/join "\r\n"
            (for [m msgs]
              (str (str/join "\r\n" (map #(str/join " " %)
                                         (partition-all 16 (concat ["F0"] (map (fn [v] (case (int v) 0x80 "GT" 0x81 "VE" 0x82 "CH" 0x83 "CS" 0x84 "SS" (format "%02X" v))) m) ["F7"]))))
                   "\r\n"))))

;; ---- エクスクルーシブの表示 (EXCLU.C exc_view・exc_dis2、SCDSP.S scpanel_dsp・scinst_dsp) ----
(defn exc-view
  "行 pos のエクスクルーシブの表示。トラックエクスクルーシブ (0x98) かユーザーエクスクルーシブ (0x90〜0x97) の行でなければ nil。
   => {:title 見出し :data [データ … F7]} (F0 は含まない)"
  [recs pos user-exc]
  (let [r (get recs pos)]
    (when (and r (<= 0x90 (r 0) 0x98))
      (if (= 0x98 (r 0))
        {:title "Track Exclusive View"
         :data (conj (vec (take-while #(not= % 0xF7) (first (polysted-rcp/track-exclusive-data recs pos)))) 0xF7)}
        (let [n (- (r 0) 0x90) [nm d] (get user-exc n ["" []])]
          {:title (str "User Exclusive View [" n "] " nm)
           :data (conj (vec (take-while #(not= % 0xF7) (uex-data d))) 0xF7)})))))

(defn exc-view-rows "データを 16 個ずつの行に (gt ve ch cs ss は名前、F7 で終わり)" [data]
  (mapv vec (partition-all 16 (map (fn [v] (case (int v) 0x80 "gt" 0x81 "ve" 0x82 "ch" 0x83 "cs" 0x84 "ss" (format "%02X" v))) data))))

(defn sc55-display
  "Roland SC-55 の画面表示のデータ (41 10 45 12 cs 10 xx 00 …) なら、その中身。
   アドレス 10 00 00 は文字 (16 文字まで) => {:text}、10 01〜 00 はドット => {:dots 16×16 の真偽}。違えば nil"
  [data]
  (let [data (vec data)]
    (when (and (>= (count data) 8) (= [0x41 0x10 0x45 0x12 0x83 0x10] (subvec data 0 6)) (zero? (data 7)))
      (let [body (subvec data 8)]
        (if (pos? (data 6))
          ;; 行 y は、y・y+16・y+32・y+48 番目のバイトの下位 5 ビットをつないで 4 ビット右へずらした 16 ビット
          {:dots (vec (for [y (range 16)]
                        (let [v (bit-shift-right (reduce (fn [acc k] (bit-or (bit-shift-left acc 5) (bit-and (get body (+ y (* 16 k)) 0) 0x1F)))
                                                         0 (range 4))
                                                 4)]
                          (vec (for [x (range 16)] (bit-test v (- 15 x)))))))}
          {:text (apply str (map char (take-while #(< 0 % 0x80) (take 16 body))))})))))

(defn exc-view-lines
  "表示の行 (TUI の画面と GUI で共通)。ドットは dots? のとき ## と .. の文字で"
  [{:keys [title data]} dots?]
  (let [rows (exc-view-rows data)
        sc (sc55-display data)]
    (vec (concat
          [(str " " title) ""
           (str "      " (str/join "  " (map #(format "%X" %) (range 16))))]
          (map-indexed (fn [i cells] (str (format "%02X" i) (if (zero? i) " F0 " "    ") (str/join " " cells))) rows)
          (cond
            (:text sc) ["" (str " SC-55 DISPLAY (文字)  [" (:text sc) "]")]
            (:dots sc) (concat ["" " SC-55 DISPLAY (ドット 16x16)"]
                               (when dots? (map (fn [row] (str "   " (apply str (map #(if % "##" "..") row)))) (:dots sc))))
            :else nil)))))

;; ---- 簡易チェック (詳しいものは polysted_session.clj の run-checks にある) ----
(defn run-checks []
  (let [checks
        [["MIDI チャンネルの表示と入力" [(ch-str 1) (ch-str 18) (ch-str 0) (parse-ch "B2") (parse-ch "0") (parse-ch "C1")]
          ["A1" "B2" "OFF" 18 0 nil]]
         ["コントローラの記号" [(spc-code "V") (spc-code "G5") (spc-code "U3") (spc-code "W")]
          [{:code 0xEB :ctl 7} {:code 0xEC :ctl 5} {:code 0x93 :ctl -1} nil]]
         ["調の表示と入力" [(key-str 1) (key-str 25) (parse-key "Bb") (parse-key "Am")] ["G MAJOR(#1)" "D minor(b1)" 10 16]]
         ["ユーザーエクスクルーシブの表: 空・F7 の後・挿入・削除・Roland のチェックサム・表示"
          (let [d (uex-data [0x41 0x10 0xF7 5 6])
                gs (uex-data [0x41 0x10 0x42 0x12 0x40 0x00 0x7F 0x00])]
            [(= (uex-data (repeat 24 0)) (vec (repeat 24 0xF7))) (subvec d 0 4) (subvec (uex-ins d 1 0x80) 0 4)
             (subvec (uex-del (uex-ins d 1 0x80) 1) 0 4) (subvec (uex-sum gs 8) 0 10) ((uex-sum (assoc gs 0 0x43) 8) 8)
             (subvec (uex-cells (uex-ins d 1 0x81)) 0 5)])
          [true [0x41 0x10 0xF7 0xF7] [0x41 0x80 0x10 0xF7] [0x41 0x10 0xF7 0xF7]
           [0x41 0x10 0x42 0x12 0x40 0x00 0x7F 0x00 0x41 0xF7] 47 ["41" "ve" "10" "F7" ".."]]]
         ["EXC ファイル: 注釈・続けて書いた 2 文字・複数のメッセージ・不正・F0 で始まらない、書き出し (16 個で折り返し)"
          [(parse-exc-text "F0 41 10 42 12 40 00 7F 00 41 F7 * GS reset\r\nF0 43 10 4C 00 00 7E 00 F7")
           (parse-exc-text "f041 10 gt ve f7") (parse-exc-text "F0 ZZ F7") (parse-exc-text "41 10")
           (exc-file-text [[0x41 0x10 0x80]])
           (exc-file-text [(vec (range 16)) [1]])]
          [[[0x41 0x10 0x42 0x12 0x40 0x00 0x7F 0x00 0x41] [0x43 0x10 0x4C 0x00 0x00 0x7E 0x00]]
           [[0x41 0x10 0x80 0x81]] nil []
           "F0 41 10 GT F7\r\n"
           "F0 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E\r\n0F F7\r\n\r\nF0 01 F7\r\n"]]
         ["エクスクルーシブの表示: トラック・ユーザー・ほかの行、16 個で折り返し"
          [(exc-view [[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x81 0xF7]] 0 [])
           (exc-view [[0x92 0 0 0]] 0 [["" []] ["" []] ["GS" [0x41 0x10 0xF7]]])
           (exc-view [[60 48 46 64]] 0 [])
           (exc-view-rows [0x41 0x10 0x81 0xF7]) (mapv count (exc-view-rows (concat (range 19) [0xF7])))]
          [{:title "Track Exclusive View" :data [0x41 0x10 0x81 0xF7]}
           {:title "User Exclusive View [2] GS" :data [0x41 0x10 0xF7]}
           nil [["41" "10" "ve" "F7"]] [16 4]]]
         ["SC-55 の画面: 文字、ドット (4 バイトの 5 ビットずつ)、違うデータ、表示の行"
          (let [hd [0x41 0x10 0x45 0x12 0x83 0x10]
                body (-> (vec (repeat 64 0)) (assoc 0 0x10 48 0x10 17 0x01))
                d (:dots (sc55-display (concat hd [0x01 0x00] body [0x84 0xF7])))]
            [(sc55-display (concat hd [0x00 0x00] (map int "Hello") [0x84 0xF7]))
             (keep-indexed #(when %2 %1) (d 0)) (keep-indexed #(when %2 %1) (d 1)) (count (filter true? (flatten d)))
             (sc55-display [0x41 0x10 0x42 0x12 0x83 0x40 0x00 0x7F 0x00 0x84 0xF7])
             (exc-view-lines {:title "T" :data (vec (concat hd [0x00 0x00] (map int "Hi") [0x84 0xF7]))} false)])
          [{:text "Hello"} [0 15] [9] 3 nil
           [" T" "" "      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F"
            "00 F0 41 10 45 12 cs 10 00 00 48 69 ss F7" "" " SC-55 DISPLAY (文字)  [Hi]"]]]
         ["REX・RES: 書いて読むと同じ、空の表、形式違い"
          (let [ue (assoc (vec (repeat 8 ["" (vec (repeat 24 0xF7))])) 2 ["GS Reset" (uex-data [0x41 0x10 0x42 0x12 0x40 0x00 0x7F 0x00 0x41])])
                rex (polysted-rcp/user-exc->rex ue)
                res (polysted-rcp/entry->res (ue 2))]
            [(alength rex) (= ue (:user-exc (polysted-rcp/rex->user-exc rex))) (alength res) (= (ue 2) (:entry (polysted-rcp/res->entry res)))
             (polysted-rcp/rex->user-exc res) (:error (polysted-rcp/res->entry (byte-array 64)))])
          [410 true 64 true {:error "ファイルが壊れています。"} "違うフォーマットのファイルです。"]]
         ["エクスクルーシブの入力と表示" [(parse-exc-data "41 10 cs 40 ss") (exc-hex [0x41 0x83 0x84 0xF7])]
          [[0x41 0x10 0x83 0x40 0x84 0xF7] "41 cs ss F7"]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted-ctrl: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(run-checks)
