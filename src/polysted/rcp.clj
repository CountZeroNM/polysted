;; src/polysted/rcp.clj  --  RCP/R36 読み込み → src/polysted/mml.clj の song 形式 → .mid (最小版・動作未確認)
;; 使い方:
;;   (require 'polysted.rcp)
;;   (polysted.rcp/rcp->mid! "曲.rcp" "out.mid")
(ns polysted.rcp
  (:require [polysted.mml]
            [clojure.string :as str])
  (:import (java.nio.file Files Paths)
           (java.nio.charset Charset)))

(defn- u8 [^bytes bs i] (bit-and (aget bs (int i)) 0xff))

(defn- sjis
  "Shift-JIS の文字列欄。C の文字列と同じく最初の 0x00 で終わり、前後の空白を除く (sread)"
  [^bytes bs off len]
  (let [n (or (some (fn [i] (when (zero? (aget bs (int (+ off i)))) i)) (range len)) len)]
    (str/trim (String. bs (int off) (int n) (Charset/forName "windows-31j")))))

;; ------------------------------------------------------------
;; 4バイトレコード → イベント (コード表は EDITS.C spc_code などで確認済みの範囲)
;; ------------------------------------------------------------
(defn- decode-record [b0 b1 b2 b3]
  (if (< b0 0x80)
    {:type :note :key b0 :st b1 :gt b2 :vel b3}
    (case (int b0)
      0xE2 {:type :bank-prog :st b1 :prog b2 :bank b3}
      0xE6 {:type :channel :st b1 :ch b2}
      0xE7 {:type :tempo :st b1 :raw b2}
      0xEA {:type :chan-pressure :st b1 :value b2}
      0xEB {:type :cc :st b1 :ctl b2 :value b3}
      0xEC {:type :prog :st b1 :prog b2}
      0xED {:type :key-pressure :st b1 :key b2 :value b3}
      0xEE {:type :bend :st b1 :value (- (+ b2 (* 128 b3)) 8192)}
      0xF5 {:type :key-sig :value b1}
      0xF8 {:type :loop-end :count b1}
      0xF9 {:type :loop-start}
      0xFC {:type :same :bar (+ b1 (* 256 (bit-and b2 3)))}
      0xFD {:type :bar}
      0xFE {:type :end}
      ;; 0x90-0x98(エクスクルーシブ), 0xDD-0xDF(Roland) などは未対応: 時間だけ進める
      (if (< b0 0xF0)
        {:type :raw :code b0 :st b1}
        {:type :raw :code b0}))))

(defn- track-size [b0 b1]
  (+ (bit-and b0 0xFC) (* (+ (* (bit-and b0 3) 256) b1) 256)))

(defn- read-track [^bytes bs pos len]
  (let [size (track-size (u8 bs pos) (u8 bs (+ pos 1)))
        end  (min (+ pos size) len)
        ch   (let [c (inc (u8 bs (+ pos 4)))] (when (<= 1 c 32) c))
        mode (u8 bs (+ pos 7))
        recs (loop [p (+ pos 44) out []]
               (if (> (+ p 4) end)
                 out
                 (let [e (decode-record (u8 bs p) (u8 bs (+ p 1)) (u8 bs (+ p 2)) (u8 bs (+ p 3)))]
                   (if (= :end (:type e))
                     out
                     (recur (+ p 4) (conj out e))))))
        ;; 4バイトレコードそのもの (終端 0xFE の手前まで)。編集・書き出しはこちらを使う
        raw  (loop [p (+ pos 44) out []]
               (if (> (+ p 4) end)
                 out
                 (let [r [(u8 bs p) (u8 bs (+ p 1)) (u8 bs (+ p 2)) (u8 bs (+ p 3))]]
                   (if (= 0xFE (r 0))
                     out
                     (recur (+ p 4) (conj out r))))))]
    {:size size
     :track {:no (u8 bs (+ pos 2))
             :ch ch
             :rhythm? (bit-test (u8 bs (+ pos 3)) 7)
             :key-shift (u8 bs (+ pos 5))
             :st-shift (u8 bs (+ pos 6))
             :mute? (bit-test mode 0)
             :comment (sjis bs (+ pos 8) 36)
             :rhy-byte (u8 bs (+ pos 3))
             :mode mode
             :events recs
             :recs raw}}))

;; ------------------------------------------------------------
;; RCP v3 (.G36、レコンポーザ 3.0 の形式) の読み込み
;; STED2 は G36 を読まない。形式は公開の変換ツール rcm2smf (shingo45endo 氏、MIT ライセンス) の
;; rcm_converter.js (parseG36・convertEvent6byte) で確認した【資料: rcm2smf】:
;;   見出し "COME ON MUSIC RECOMPOSER RCP3.0" (32 バイトの欄)。曲名 0x20 (64)、著作権 0x60 (64)、
;;   メモ 0xA0 + 30×i (12 行)、トラック数 0x208 (u16)、タイムベース 0x20A (u16)、テンポ 0x20C (u16)、
;;   拍子 0x20E・0x20F、調 0x210、PLAYBIAS 0x211、CM6 0x298 (16)、GSD 0x2A8 (16)、GSD2 0x2B8 (16)、
;;   リズム割り当て 0x318 + 16×i (128 個、名前 14・キー・GT)、ユーザーエクスクルーシブ 0xB18 + 48×i
;;   (8 個、名前 23・データ 25)、最初のトラック 0xC98。
;;   トラック: 大きさ u32 (見出し 46 バイトを含む)、+4 番号、+5 種類、+6 チャンネル (i8: ポート×16 + ch、
;;   負なら OFF)、+7 K#+、+8 ST+ (i8)、+9 モード (bit0 ミュート)、+10 メモ (36)、+46 からレコード。
;;   レコードは 6 バイト [cmd vel ST(u16) GT(u16)] (v2 の [cmd ST GT vel] と並びが違い、ST・GT が 16 ビット)。
;;   SAME は小節番号が ST の欄 (u16)。リピートの回数も ST の欄。コメント (0xF6) と続きの行 (0xF7) は
;;   1 行に 5 バイト (バイト 1〜5)。エクスクルーシブ (0x98) の続きの行も 5 バイトずつ。
;; STED2 の 4 バイトの形への変換【設計判断】: ST が 255 を超える行は ST 240 にして残りを休符で足す、
;;   GT が 255 を超える音は 255 に丸める (警告)、コメント・エクスクルーシブは 2 バイトずつの行に詰め直す、
;;   SAME の番号が 1023 を超えたら 1023 (警告)、リピートの回数は 255 まで、チャンネルはポート 0・1 を
;;   A1〜B16 にし、それより上は警告して下位 5 ビット、リズム割り当ては先頭の 32 個、ユーザーエクスクルーシブの
;;   データは 24 バイトまで。トラックの種類 (+5) の bit7 をリズムトラックとみなす (0x80 がリズム ON。
;;   Valley Bell 氏の RCPFormat.txt で確認【資料】。rcp2mid-web-public に同梱)。
;; 2 つ目の資料 RCPFormat.txt (Header v3 [G36]・Track Header [G36]・Sequence Data [G36]) と、行の並び
;; [cmd vel ST GT]・チャンネル (0xFF が OFF、0x00〜0x1F がポート A・B)・コメントの 5 バイト・調の値が一致する。
;; 合成サンプル samples/sample_g36.g36 (rcp2mid-web-public) で、見出し・メモ・音符・SMF・RCP の往復を確認 (2026-11-19)。
;; ------------------------------------------------------------
(def g36-id "COME ON MUSIC RECOMPOSER RCP3.0")

(defn g36-bytes? "G36 (RCP v3) のバイト列か (見出しで判定)" [^bytes bs]
  (and (>= (alength bs) 518) (= g36-id (String. bs 0 (int (count g36-id)) (Charset/forName "windows-31j")))))

(defn- u16 [^bytes bs i] (+ (u8 bs i) (* 256 (u8 bs (inc i)))))
(defn- u32 [^bytes bs i] (+ (u16 bs i) (* 65536 (u16 bs (+ i 2)))))
(defn- s8 [^bytes bs i] (let [v (u8 bs i)] (if (> v 127) (- v 256) v)))

(defn- split-st "ST が 255 を超える行は ST 240 にして、残りを休符で足す" [[a st c d] warn!]
  (if (<= st 255)
    [[a st c d]]
    (let [rest-st (- st 240)]
      (when (> st 0xFFFF) (warn! :st))
      (vec (concat [[a 240 c d]] (repeat (quot rest-st 240) [0 240 0 0])
                   (when (pos? (mod rest-st 240)) [[0 (mod rest-st 240) 0 0]]))))))

(defn g36-records
  "G36 の 6 バイトのレコード (バイトの vector の列) → STED2 の 4 バイトのレコード列 (END の手前まで)。
   warn! は (fn [kind]) で、:gt (GT>255) :same (SAME の番号が大きい) を数える"
  [rows warn!]
  (let [rows (vec rows) n (count rows)
        st (fn [r] (+ (r 2) (* 256 (r 3)))) gt (fn [r] (+ (r 4) (* 256 (r 5))))
        tail (fn [i] (take-while #(= 0xF7 (% 0)) (subvec rows (inc i))))   ; 続きの F7 の行
        two-byte-rows (fn [code data pad]                                   ; 2 バイトずつの行に
                        (let [d (if (odd? (count data)) (conj (vec data) pad) (vec data))]
                          (mapv (fn [[x y]] [code 0 x y]) (partition 2 d))))]
    (loop [i 0 out []]
      (if (>= i n)
        out
        (let [r (rows i) a (r 0)]
          (cond
            (= a 0xFE) out
            (= a 0xF7) (recur (inc i) out)                                  ; 0x98・0xF6 に続かない F7 は捨てる
            (= a 0xF6)                                                      ; コメント: 本体と続きの 5 バイトずつ
            (let [t (tail i)
                  text (concat (subvec r 1) (mapcat #(subvec % 1) t))
                  [h & more] (two-byte-rows 0xF7 text 0x20)]
              (recur (+ i 1 (count t)) (into (conj out (assoc h 0 0xF6)) more)))
            (= a 0x98)                                                      ; トラックエクスクルーシブ: 続きの 5 バイトずつ
            (let [t (tail i)
                  data (concat (take-while #(not= % 0xF7) (mapcat #(subvec % 1) t)) [0xF7])]
              (recur (+ i 1 (count t))
                     (-> out (into (split-st [0x98 (st r) (bit-and (gt r) 0xFF) (r 1)] warn!))
                         (into (two-byte-rows 0xF7 data 0xF7)))))
            (= a 0xFC) (let [m (st r)]
                         (when (> m 1023) (warn! :same))
                         (let [m (min m 1023)] (recur (inc i) (conj out [0xFC (bit-and m 0xFF) (bit-shift-right m 8) 0]))))
            (= a 0xF8) (recur (inc i) (conj out [0xF8 (min 255 (st r)) 0 0]))
            (#{0xF9 0xFD} a) (recur (inc i) (conj out [a 0 0 0]))
            (= a 0xF5) (recur (inc i) (conj out [0xF5 (bit-and (st r) 0xFF) 0 0]))
            :else
            (let [g (gt r)
                  g (if (and (< a 0x80) (> g 255)) (do (warn! :gt) 255) (bit-and g 0xFF))]
              (recur (inc i) (into out (split-st [a (st r) g (r 1)] warn!))))))))))

(defn read-g36-bytes
  "G36 のバイト列 → read-rcp と同じ形の曲 (:recs は STED2 の 4 バイト)。:load-warnings に読み込みの注意"
  [^bytes bs]
  (let [len (alength bs)
        warns (atom {})
        warn! (fn [k] (swap! warns update k (fnil inc 0)))
        ntr (u16 bs 0x208)
        tracks (loop [pos 0xC98 k 0 out []]
                 (if (or (>= k ntr) (> (+ pos 46) len))
                   out
                   (let [size (u32 bs pos)]
                     (if (or (< size 46) (> (+ pos size) len))
                       out
                       (let [rows (mapv (fn [p] (mapv #(u8 bs (+ p %)) (range 6)))
                                        (range (+ pos 46) (- (+ pos size) 5) 6))
                             recs (g36-records rows warn!)
                             c (s8 bs (+ pos 6))
                             _ (when (> c 31) (warn! :port))
                             mode (u8 bs (+ pos 9))]
                         (recur (+ pos size) (inc k)
                                (conj out {:no (u8 bs (+ pos 4))
                                           :ch (when (>= c 0) (inc (bit-and c 31)))
                                           :rhythm? (bit-test (u8 bs (+ pos 5)) 7)
                                           :rhy-byte (u8 bs (+ pos 5))
                                           :key-shift (u8 bs (+ pos 7))
                                           :st-shift (u8 bs (+ pos 8))
                                           :mute? (bit-test mode 0)
                                           :mode mode
                                           :comment (sjis bs (+ pos 10) 36)
                                           :events (mapv #(apply decode-record %) recs)
                                           :recs recs})))))))
        tempo (u16 bs 0x20C)
        _ (when (> tempo 255) (warn! :tempo))
        uex (mapv (fn [i] (let [o (+ 0xB18 (* i 48)) d (mapv #(u8 bs (+ o 23 %)) (range 25))]
                            (when (and (not= 0xF7 (d 24)) (not= 0 (d 24)) (not-any? #{0xF7} (take 24 d))) (warn! :uex))
                            [(sjis bs o 23) (subvec d 0 24)]))
                  (range 8))
        w @warns]
    {:title (sjis bs 0x20 64)
     :copyright (sjis bs 0x60 64)
     :timebase (let [t (u16 bs 0x20A)] (if (pos? t) t 48))
     :tempo (min 255 tempo)
     :time-sig [(u8 bs 0x20E) (u8 bs 0x20F)]
     :key (u8 bs 0x210)
     :pbias (u8 bs 0x211)
     :memo (mapv #(sjis bs (+ 0xA0 (* % 30)) 30) (range 12))
     :cm6-file (sjis bs 0x298 16)
     :gsd-file (sjis bs 0x2A8 16)
     :tone-assign (vec (repeat 16 0))
     :rhythm-assign (mapv (fn [i] (let [o (+ 0x318 (* i 16))] [(sjis bs o 14) (u8 bs (+ o 14)) (u8 bs (+ o 15))])) (range 32))
     :user-exc uex
     :tracks tracks
     :load-warnings (cond-> ["RCP v3 (G36) を STED2 の形式に変換して読み込みました (保存は .rcp / .r36)"]
                      (w :gt) (conj (str "GT が 255 を超える音を 255 にしました (" (w :gt) " 個)"))
                      (w :same) (conj (str "SAME の小節番号が 1023 を超えるものを 1023 にしました (" (w :same) " 個)"))
                      (w :port) (conj "ポート 2 以上のチャンネルは A・B に寄せました")
                      (w :tempo) (conj "テンポが 255 を超えるので 255 にしました")
                      (w :uex) (conj "ユーザーエクスクルーシブの 25 バイト目を落としました")
                      (w :st) (conj "ST が非常に長い行があります"))}))

(declare read-rcp-v2)

(defn read-rcp
  "RCP / R36 / G36 (見出しで判定) を読む"
  [path]
  (let [bs  (Files/readAllBytes (Paths/get path (into-array String [])))]
    (if (g36-bytes? bs)
      (read-g36-bytes bs)
      (read-rcp-v2 bs path))))

(defn- read-rcp-v2 [^bytes bs path]
  (let [len (alength bs)
        n   (if (or (= 36 (u8 bs 0x1E6))
                    (str/ends-with? (str/lower-case path) ".r36"))
              36 18)
        tb  (let [t (+ (u8 bs 0x1C0) (if (pos? (u8 bs 0x1E6)) (* 256 (u8 bs 0x1E7)) 0))]
              (if (pos? t) t 48))
        tracks (loop [pos 0x586 k 0 out []]
                 (if (or (>= k n) (> (+ pos 44) len))
                   out
                   (let [{:keys [size track]} (read-track bs pos len)]
                     (if (< size 44)
                       out
                       (recur (+ pos size) (inc k) (conj out track))))))]
    {:title (sjis bs 0x20 64)
     :timebase tb
     :tempo (u8 bs 0x1C1)
     :time-sig [(u8 bs 0x1C2) (u8 bs 0x1C3)]
     :key (u8 bs 0x1C4)
     ;; 書き出しで戻すための項目 (FILE.C hedread)
     :pbias (u8 bs 0x1C5)
     :memo (mapv #(sjis bs (+ 0x60 (* % 28)) 28) (range 12))
     :cm6-file (sjis bs 0x1C6 16)
     :gsd-file (sjis bs 0x1D6 16)
     :tone-assign (mapv #(u8 bs (+ 0x1F6 %)) (range 16))
     :rhythm-assign (mapv (fn [i] (let [o (+ 0x206 (* i 16))]
                                    [(sjis bs o 14) (u8 bs (+ o 14)) (u8 bs (+ o 15))]))
                          (range 32))
     :user-exc (mapv (fn [i] (let [o (+ 0x406 (* i 48))]
                               [(sjis bs o 24) (mapv #(u8 bs (+ o 24 %)) (range 24))]))
                     (range 8))
     :tracks tracks}))

;; ------------------------------------------------------------
;; RCP/R36 書き出し (FILE.C dsave / hedset / thedset、保存前の dclr / same_cluc)
;; ------------------------------------------------------------
(def rcp-id "RCM-PC98V2.0(C)COME ON MUSIC\r\n")       ; 30 バイト
(def ^:private sjis-cs (Charset/forName "windows-31j"))

(defn- sjis-fit
  "Shift-JIS で len バイト以内に収まるよう末尾を削った文字列のバイト列 (全角を途中で切らない)"
  ^bytes [s len]
  (loop [s (str s)]
    (let [b (.getBytes ^String s sjis-cs)]
      (if (<= (alength b) len) b (recur (subs s 0 (dec (count s))))))))

(defn- put-str!
  "sset: len バイトを空白で埋めてから文字列を書く"
  [^bytes buf off len s]
  (java.util.Arrays/fill buf (int off) (int (+ off len)) (byte 32))
  (let [b (sjis-fit s len)]
    (System/arraycopy b 0 buf (int off) (alength b))))

(defn- put-u8! [^bytes buf i v] (aset buf (int i) (unchecked-byte (int v))))

(defn- measure-starts
  "add_set: 小節の先頭のレコード番号の表 (終端 0xFE を含めた列で数える)"
  [recs]
  (into [0] (keep (fn [i] (when (or (>= ((recs i) 0) 0xFC) (= (get-in recs [(inc i) 0]) 0xFC)) (inc i)))
                  (range (count recs)))))

(defn normalize-track-records
  "保存前の正規化 (dclr と same_cluc)。recs は終端 0xFE を含まない列。=> 終端 [FE FE FE FE] 付き
   - 0xE7 の値 0 は 1、0xE6/0xEA/0xEC の byte3 は 0、0xF6/0xF7 の byte1 は 0
   - 0xF8 の byte2・3 は 0、0xF9・0xFD は [x 1 0 0] (小節番号はファイルには入らない)
   - SAME 0xFC: 指す先が SAME ならたどって番号を直し、byte2 の上位6ビットと byte3 に
     複写元の小節のバイト位置 (トラックヘッダ 44 を含む) を入れる"
  [recs]
  (let [recs (conj (mapv (fn [[a b c d :as r]]
                           (case (int a)
                             0xE7 (if (zero? c) [a b 1 d] r)
                             (0xE6 0xEA 0xEC) [a b c 0]
                             (0xF6 0xF7) [a 0 c d]
                             0xF8 [a b 0 0]
                             (0xF9 0xFD) [a 1 0 0]
                             r))
                         recs)
                   [0xFE 0xFE 0xFE 0xFE])
        starts (measure-starts recs)
        mx     (dec (count starts))                 ; add_set の戻り値 (区切りの数)
        no-of  (fn [r] (+ (r 1) (* 256 (bit-and (r 2) 3))))]
    (vec (map-indexed
          (fn [j r]
            (if (not= (r 0) 0xFC)
              r
              (let [[no ok?] (loop [no (no-of r) depth 0]
                               (if (>= no mx)
                                 [no false]
                                 (let [ad (starts no)]
                                   (if (and (not= j ad) (= ((recs ad) 0) 0xFC) (< depth 16))
                                     (recur (no-of (recs ad)) (inc depth))
                                     [no true]))))
                    r   (if ok? [(r 0) (bit-and no 0xFF) (+ (bit-and (r 2) 0xFC) (bit-and (bit-shift-right no 8) 3)) (r 3)] r)
                    ad  (+ 44 (if ok? (* 4 (starts no)) 0))]
                [(r 0) (r 1) (+ (bit-and (r 2) 3) (bit-and ad 0xFC)) (bit-and (bit-shift-right ad 8) 0xFF)])))
          recs))))

(defn- default-track
  "dinit の初期値 (i は 0 始まり)。10 トラック目はリズム"
  [i]
  {:ch (when (< i 16) (inc i)) :recs [] :mute? false :rhythm? (= i 9) :comment ""
   :key-shift (if (= i 9) 128 0) :st-shift 0 :rhy-byte (if (= i 9) 128 0) :mode 0})

(defn song->rcp-bytes
  "曲 (トラックが :recs を持つ形) → RCP/R36 のバイト列。r36?: 36 トラックで書く
   (r36? を省略したら、18 トラック目より後ろに中身があるとき 36)"
  ([song] (song->rcp-bytes song nil))
  ([song r36?]
   (let [tracks (vec (:tracks song))
         used36 (some (fn [[i t]] (and (>= i 18) (or (seq (:recs t)) (seq (:comment t)))))
                      (map-indexed vector tracks))
         mx     (if (or r36? (and (nil? r36?) used36) (> (count tracks) 36)) 36 18)
         _      (when (> (count tracks) mx)
                  (throw (ex-info (str "トラックが多すぎます (" (count tracks) " > " mx ")") {})))
         tb     (or (:timebase song) 48)
         hed    (byte-array 0x586)
         out    (java.io.ByteArrayOutputStream.)]
     ;; ---- ヘッダ (hedset) ----
     (put-str! hed 0 30 rcp-id) (put-u8! hed 30 0) (put-u8! hed 31 0)
     (put-str! hed 0x20 64 (or (:title song) ""))
     (dotimes [i 12] (put-str! hed (+ 0x60 (* i 28)) 28 (get (:memo song) i "")))
     (doseq [i (range 0x1B0 0x1C0)] (put-u8! hed i 0x40))
     (put-u8! hed 0x1C0 (bit-and tb 0xFF))
     (put-u8! hed 0x1C1 (or (:tempo song) 120))
     (let [[b1 b2] (or (:time-sig song) [4 4])] (put-u8! hed 0x1C2 b1) (put-u8! hed 0x1C3 b2))
     (put-u8! hed 0x1C4 (or (:key song) 0))
     (put-u8! hed 0x1C5 (or (:pbias song) 0))
     (when (seq (:cm6-file song)) (put-str! hed 0x1C6 12 (:cm6-file song)))
     (when (seq (:gsd-file song)) (put-str! hed 0x1D6 12 (:gsd-file song)))
     (put-u8! hed 0x1E6 mx)
     (put-u8! hed 0x1E7 (bit-shift-right tb 8))
     (dotimes [i 16] (put-u8! hed (+ 0x1F6 i) (get (:tone-assign song) i 0)))
     (dotimes [i 32]
       (let [[nm k g] (get (:rhythm-assign song) i ["" 0 0]) o (+ 0x206 (* i 16))]
         (put-str! hed o 14 nm) (put-u8! hed (+ o 14) k) (put-u8! hed (+ o 15) g)))
     (dotimes [i 8]
       (let [[nm data] (get (:user-exc song) i ["" []]) o (+ 0x406 (* i 48))]
         (put-str! hed o 24 nm)
         (dotimes [j 24] (put-u8! hed (+ o 24 j) (get data j 0)))))
     (.write out hed)
     ;; ---- トラック (thedset + データ) ----
     (dotimes [i mx]
       (let [t    (merge (default-track i) (get tracks i))
             recs (normalize-track-records (vec (:recs t)))
             sz   (+ 44 (* 4 (count recs)))
             th   (byte-array 44)
             mode (bit-or (bit-and (or (:mode t) 0) 0xFE) (if (:mute? t) 1 0))]
         (put-u8! th 0 (+ (bit-and sz 0xFC) (bit-and (bit-shift-right sz 16) 3)))
         (put-u8! th 1 (bit-and (bit-shift-right sz 8) 0xFF))
         (put-u8! th 2 (inc i))                       ; トラック番号は位置+1【設計判断】
         (put-u8! th 3 (bit-or (bit-and (or (:rhy-byte t) 0) 0x7F) (if (:rhythm? t) 0x80 0)))
         (put-u8! th 4 (dec (or (:ch t) 0)))           ; OFF は 0xFF
         (put-u8! th 5 (or (:key-shift t) 0))
         (put-u8! th 6 (or (:st-shift t) 0))
         (put-u8! th 7 (if (= mode 3) 4 mode))
         (put-str! th 8 36 (or (:comment t) ""))
         (.write out th)
         (doseq [r recs] (doseq [v r] (.write out (int v))))))
     (.toByteArray out))))

(defn write-rcp!
  "曲を RCP (.rcp) / R36 (.r36) で書き出す。拡張子 .r36 なら 36 トラック"
  [song path]
  (let [r36? (when (str/ends-with? (str/lower-case path) ".r36") true)]
    (Files/write (Paths/get path (into-array String []))
                 ^bytes (song->rcp-bytes song r36?)
                 (into-array java.nio.file.OpenOption []))
    path))

;; ------------------------------------------------------------
;; テンポ: E7 の値 = 設定したいテンポ ÷ 初期設定テンポ × 64 (マニュアル STED2.MAN《スペシャルコントローラ》で確認)。
;; byte3 は GRADATION (変化の速さ、0 で瞬時) だが、再生では瞬時に変える【設計判断】。
;; 実ファイルで鳴らしてテンポが合わなければ、この1関数だけを直す。
;; ------------------------------------------------------------
(defn tempo-bpm [base raw]
  (max 1 (Math/round (double (/ (* base raw) 64)))))

;; ------------------------------------------------------------
;; エクスクルーシブ (マニュアル《USER EXC》、EXCLU.C、《スペシャルコントローラ》の Roland 系)
;; ------------------------------------------------------------
(defn exclusive-bytes
  "エクスクルーシブのデータ (F0 を含まない。F7 で終わる) → 送る SysEx (F0 … F7)。特殊コード:
   0x80 gt = その行の byte2、0x81 ve = byte3、0x82 ch = トラックのチャンネル (0〜15)、
   0x83 cs = チェックサムを 0 から数え直す、0x84 ss = cs 以降の Roland 形式チェックサム、0xF7 = 終わり"
  [data {:keys [gt ve ch] :or {gt 0 ve 0}}]
  (loop [ds (seq data) out [0xF0] sum 0]
    (let [d (first ds)]
      (cond
        (or (nil? d) (= d 0xF7)) (conj out 0xF7)
        (= d 0x83) (recur (next ds) out 0)
        (= d 0x84) (recur (next ds) (conj out (bit-and (- 128 (bit-and sum 127)) 127)) sum)
        :else (let [v (bit-and (case (int d) 0x80 gt 0x81 ve 0x82 (mod (dec (or ch 1)) 16) d) 0x7F)]
                (recur (next ds) (conj out v) (+ sum v)))))))

(defn track-exclusive-data
  "0x98 の行 i に続く 0xF7 の行から、データのバイト列 (F7 まで) と、次の行番号を取り出す"
  [recs i]
  (loop [j (inc i) acc []]
    (if (and (< j (count recs)) (= ((recs j) 0) 0xF7))
      (let [[_ _ x y] (recs j)]
        (cond (= x 0xF7) [(conj acc 0xF7) (inc j)]
              (= y 0xF7) [(conj acc x 0xF7) (inc j)]
              :else (recur (inc j) (conj acc x y))))
      [acc j])))

(defn user-exclusive-data
  "ユーザーエクスクルーシブ n (0〜7) のデータ。空 (先頭と末尾が 0) なら nil (uex_read)"
  [user-exc n]
  (let [data (second (get user-exc n))]
    (when (and (seq data) (not (and (zero? (first data)) (zero? (peek data)))) (not= (first data) 0xF7))
      data)))

(defn records->events-ex
  "4バイトレコード → イベント。エクスクルーシブは :sysex {:st :bytes} にする
   (0x98 + 続く 0xF7、0x90〜0x97 はヘッダのユーザーエクスクルーシブ、0xDE は Roland DT1 を作る)。
   0xDF (デバイス番号・モデル ID、既定 16・22) と 0xDD (アドレス上位・中位) は状態として覚える。
   ctx: {:user-exc ヘッダの表 :ch トラックのチャンネル}"
  [recs {:keys [user-exc ch]}]
  (let [recs (vec recs) n (count recs)]
    (loop [i 0 out [] ch ch dev 0x10 model 0x16 hi 0 mid 0]
      (if (>= i n)
        out
        (let [[a b c d] (recs i)]
          (cond
            (= a 0x98) (let [[data j] (track-exclusive-data recs i)]
                         (recur j (conj out {:type :sysex :st b :bytes (exclusive-bytes data {:gt c :ve d :ch ch})})
                                ch dev model hi mid))
            (<= 0x90 a 0x97) (let [data (user-exclusive-data user-exc (- a 0x90))]
                               (recur (inc i) (conj out (if data
                                                          {:type :sysex :st b :bytes (exclusive-bytes data {:gt c :ve d :ch ch})}
                                                          {:type :raw :code a :st b}))
                                      ch dev model hi mid))
            (= a 0xDF) (recur (inc i) (conj out {:type :raw :code a :st b}) ch c d hi mid)
            (= a 0xDD) (recur (inc i) (conj out {:type :raw :code a :st b}) ch dev model c d)
            (= a 0xDE) (recur (inc i) (conj out {:type :sysex :st b
                                                 :bytes (exclusive-bytes [0x41 dev model 0x12 0x83 hi mid c d 0x84 0xF7] {})})
                              ch dev model hi mid)
            (= a 0xF7) (recur (inc i) out ch dev model hi mid)             ; 0x98 から外れた続きの行は無視
            :else (recur (inc i) (conj out (decode-record a b c d))
                         (if (= a 0xE6) c ch) dev model hi mid)))))))

(defn rcp->song [path]
  (let [{:keys [tempo tracks] :as song} (read-rcp path)
        tracks (mapv #(assoc % :events (records->events-ex (:recs %) {:user-exc (:user-exc song) :ch (:ch %)})) tracks)
        fix (fn [e] (if (= :tempo (:type e))
                      (assoc e :value (tempo-bpm tempo (:raw e)))
                      e))
        tracks (mapv #(update % :events (fn [es] (mapv fix es))) tracks)
        tracks (if (seq tracks)
                 (update tracks 0 update :events
                         #(into [{:type :tempo :st 0 :value tempo}] %))
                 tracks)]
    (assoc song :tracks tracks)))

(defn rcp->mid!
  "ミュートのトラックを除いて .mid に書き出す。リズムトラック (RHY) も普通に鳴らす
   (原作の RHY はパターン編集の可否だけで、演奏には影響しない)。K#+ と ST+ は反映する"
  [in out]
  (let [song (rcp->song in)
        skip? (fn [t] (:mute? t))
        used (remove skip? (:tracks song))]
    (println "title:" (:title song)
             "| timebase:" (:timebase song)
             "| tempo:" (:tempo song)
             "| tracks:" (count (:tracks song)))
    (doseq [[i t] (map-indexed vector (:tracks song))
            :when (seq (:events t))]
      (println (format "  TR%02d ch=%s records=%d%s"
                       (inc i) (or (:ch t) "OFF") (count (:events t))
                       (cond (:mute? t)   "  [ミュート:スキップ]"
                             (:rhythm? t) "  [リズム]"
                             :else ""))))
    (polysted.mml/write-mid! (assoc song :tracks (vec used)) out)
    (println "wrote" out)))

;; ------------------------------------------------------------
;; ユーザーエクスクルーシブのファイル (FILE.C rex_load / rex_save / uex_read / uex_set)
;;   .REX: "RCM-PC98 USER EXCLUSIVE\r\n" (25 バイト) + 0x1A + 8 個 × (名前 24 + データ 24) = 410 バイト
;;   .RES: "STedV2.0U_EXC\r\n" (15 バイト) + 0x1A + 1 個 = 64 バイト
;;   データの先頭と末尾がともに 0 なら空 (uex_read)
;; ------------------------------------------------------------
(def rex-id "RCM-PC98 USER EXCLUSIVE\r\n")
(def res-id "STedV2.0U_EXC\r\n")

(defn- uex-entry-at [^bytes bs off]
  (let [d (mapv #(u8 bs (+ off 24 %)) (range 24))]
    [(sjis bs off 24) (if (and (zero? (d 0)) (zero? (d 23))) (vec (repeat 24 0xF7)) d)]))

(defn- put-uex-entry! [^bytes buf off [nm data]]
  (put-str! buf off 24 (or nm ""))
  (dotimes [j 24] (put-u8! buf (+ off 24 j) (get data j 0xF7))))

(defn- id-ok? [^bytes bs ^String id]
  (and (>= (alength bs) (count id)) (= id (String. bs 0 (int (count id)) sjis-cs))))

(defn user-exc->rex "ユーザーエクスクルーシブの表 (8 × [名前 データ]) → .REX のバイト列" ^bytes [ue]
  (let [buf (byte-array 410)]
    (System/arraycopy (.getBytes ^String rex-id sjis-cs) 0 buf 0 25)
    (put-u8! buf 25 0x1A)
    (dotimes [i 8] (put-uex-entry! buf (+ 26 (* 48 i)) (get ue i ["" []])))
    buf))

(defn rex->user-exc ".REX のバイト列 → {:user-exc 8 × [名前 データ]} か {:error}" [^bytes bs]
  (cond
    (< (alength bs) 410) {:error "ファイルが壊れています。"}
    (not (id-ok? bs rex-id)) {:error "違うフォーマットのファイルです。"}
    :else {:user-exc (mapv #(uex-entry-at bs (+ 26 (* 48 %))) (range 8))}))

(defn entry->res "1 個の [名前 データ] → .RES のバイト列" ^bytes [entry]
  (let [buf (byte-array 64)]
    (System/arraycopy (.getBytes ^String res-id sjis-cs) 0 buf 0 15)
    (put-u8! buf 15 0x1A)
    (put-uex-entry! buf 16 entry)
    buf))

(defn res->entry ".RES のバイト列 → {:entry [名前 データ]} か {:error}" [^bytes bs]
  (cond
    (< (alength bs) 64) {:error "ファイルが壊れています。"}
    (not (id-ok? bs res-id)) {:error "違うフォーマットのファイルです。"}
    :else {:entry (uex-entry-at bs 16)}))
