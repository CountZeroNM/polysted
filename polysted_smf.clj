;; polysted_smf.clj — SMF (.mid) の読み込み → レコード形式の曲
;;
;; 方針【設計判断】(原作 STED2 には SMF 読み込みが無いので、仕様は本実装で決めたもの):
;;   - javax.sound.midi で読む (フォーマット 0/1、ランニングステータス対応)。SMPTE 時間は未対応
;;   - 時間は SMF の分解能 (PPQ) から曲のタイムベース (既定 48) へ換算して丸める
;;   - 1つの SMF トラックに複数チャンネルがあれば、チャンネルごとに polysted のトラックに分ける
;;   - ノートオンとノートオフ (ベロシティ0 のノートオンを含む) を組にして [キー ST GT vel]。
;;     同じ時刻のイベントは ST=0 で並べる (コントローラが先、ノートが後)
;;   - 間隔が 255 を超えるときは休符 [0 n 0 0] で埋める。GT が 255 を超える音は 255 で切る (警告)
;;   - CC → 0xEB、音色 → 0xEC、ベンド → 0xEE、チャンネルアフタータッチ → 0xEA、ポリ → 0xED
;;   - SysEx (F0 …) → トラックエクスクルーシブ [98 ST 0 0] + [F7 0 d d] の行 (F0 を除き F7 で終わるデータ。
;;     奇数なら F7 で埋める。EXCLU.C の書き込みと同じ形)。チャンネルを持たないので、そのトラックの
;;     最初のチャンネルの polysted のトラックに入れる (チャンネルが無ければ ch1 のトラック)
;;   - 最初のテンポを曲のテンポにし、途中のテンポ変更は 0xE7 (値 = テンポ ÷ 曲のテンポ × 64、マニュアルで確認) を
;;     最初のトラックに入れる。拍子・調号・トラック名 (→ コメント) も取り込む
;;   - 最後のイベントの ST は、トラック終端 (End of Track) までの時間 (無ければ音の長さ)
;;
;; 読み込み: (load-file "polysted_smf.clj")  ※ロード時に run-checks が走る
;; 使い方:   (polysted-smf/read-smf "x.mid") => {:song 曲 :warnings [...]}
(ns polysted-smf
  (:require [clojure.string :as str])
  (:import (javax.sound.midi MidiSystem Sequence Track MidiEvent ShortMessage MetaMessage SysexMessage)
           (java.io File)
           (java.nio.charset Charset)))

(defn- u8 [b] (bit-and (int b) 0xFF))

(defn- meta-text [^MetaMessage m]
  (let [d (.getData m)]
    ;; 日本語の SMF は Shift-JIS が多い
    (str/trim (String. d (Charset/forName "windows-31j")))))

(defn- tempo-bpm [^MetaMessage m]
  (let [d (.getData m)
        mpq (+ (bit-shift-left (u8 (aget d 0)) 16) (bit-shift-left (u8 (aget d 1)) 8) (u8 (aget d 2)))]
    (/ 60000000.0 mpq)))

(defn key-sig->byte
  "SMF の調号 (sf: -7〜7, minor?) → STED2 の調の1バイト (bit0-2 数、bit3 ♭、bit4 短調)"
  [sf minor?]
  (+ (Math/abs (int sf)) (if (neg? sf) 8 0) (if minor? 16 0)))

(defn- scan-track
  "1トラック → {:name 名前 :metas [...] :chans {ch [イベント...]}}。時間は SMF の tick のまま"
  [^Track tr]
  (let [n (.size tr)]
    (loop [i 0 name nil metas [] chans {} open {} eot 0]
      (if (>= i n)
        {:name name :metas metas :eot eot
         ;; 閉じていないノートは終端まで
         :chans (reduce (fn [cs [[ch k] q]]
                          (reduce (fn [cs [t v ord]] (update cs ch (fnil conj []) {:t t :kind :note :key k :vel v :len (max 1 (- eot t)) :ord ord}))
                                  cs q))
                        chans open)}
        (let [ev  ^MidiEvent (.get tr i)
              t   (.getTick ev)
              msg (.getMessage ev)]
          (cond
            (and (instance? SysexMessage msg) (= 0xF0 (.getStatus ^SysexMessage msg)))
            (let [data (mapv #(bit-and % 0xFF) (.getData ^SysexMessage msg))]
              (recur (inc i) name metas (update chans :sysex (fnil conj []) {:t t :kind :sysex :data data :ord i}) open (max eot t)))

            (instance? MetaMessage msg)
            (let [m ^MetaMessage msg typ (.getType m)]
              (case (int typ)
                0x03 (recur (inc i) (or name (meta-text m)) metas chans open (max eot t))
                0x2F (recur (inc i) name metas chans open (max eot t))
                (recur (inc i) name (conj metas {:t t :type typ :msg m}) chans open (max eot t))))

            (instance? ShortMessage msg)
            (let [m ^ShortMessage msg cmd (.getCommand m) ch (.getChannel m)
                  d1 (.getData1 m) d2 (.getData2 m)
                  add (fn [e] (update chans ch (fnil conj []) (assoc e :t t :ord i)))]
              (cond
                (and (= cmd ShortMessage/NOTE_ON) (pos? d2))
                (recur (inc i) name metas chans (update open [ch d1] (fnil conj []) [t d2 i]) (max eot t))

                (or (= cmd ShortMessage/NOTE_OFF) (= cmd ShortMessage/NOTE_ON))
                (let [q (get open [ch d1])]
                  (if (seq q)
                    (let [[t0 v ord] (first q)]
                      (recur (inc i) name metas
                             (update chans ch (fnil conj []) {:t t0 :kind :note :key d1 :vel v :len (- t t0) :ord ord})
                             (assoc open [ch d1] (subvec q 1)) (max eot t)))
                    (recur (inc i) name metas chans open (max eot t))))

                (= cmd ShortMessage/CONTROL_CHANGE)   (recur (inc i) name metas (add {:kind :cc :ctl d1 :value d2}) open (max eot t))
                (= cmd ShortMessage/PROGRAM_CHANGE)   (recur (inc i) name metas (add {:kind :prog :prog d1}) open (max eot t))
                (= cmd ShortMessage/PITCH_BEND)       (recur (inc i) name metas (add {:kind :bend :lsb d1 :msb d2}) open (max eot t))
                (= cmd ShortMessage/CHANNEL_PRESSURE) (recur (inc i) name metas (add {:kind :chan-pressure :value d1}) open (max eot t))
                (= cmd ShortMessage/POLY_PRESSURE)    (recur (inc i) name metas (add {:kind :key-pressure :key d1 :value d2}) open (max eot t))
                :else (recur (inc i) name metas chans open (max eot t))))

            :else (recur (inc i) name metas chans open (max eot t))))))))

(defn exclusive-records
  "SysEx のデータ (F0 を除く) → [98 ST 0 0] と [F7 0 d d] の行。データは F7 で終わり、奇数なら F7 で埋める"
  [data st]
  (let [d (vec (map #(if (= % 0xF7) 0xF7 (bit-and % 0x7F)) data))
        d (if (= (peek d) 0xF7) d (conj d 0xF7))
        d (if (odd? (count d)) (conj d 0xF7) d)]
    (into [[0x98 st 0 0]] (map (fn [[x y]] [0xF7 0 x y]) (partition 2 d)))))

(defn- ev->recs [e st]
  (case (:kind e)
    :note          [[(:key e) st (:gt e) (:vel e)]]
    :cc            [[0xEB st (:ctl e) (:value e)]]
    :prog          [[0xEC st (:prog e) 0]]
    :bend          [[0xEE st (:lsb e) (:msb e)]]
    :chan-pressure [[0xEA st (:value e) 0]]
    :key-pressure  [[0xED st (:key e) (:value e)]]
    :tempo         [[0xE7 st (:raw e) 0]]
    :sysex         (exclusive-records (:data e) st)))

(defn abs->records
  "絶対時刻 (換算済み) のイベント列 → レコード列。end: トラックの終わりの時刻 (nil なら最後の音の長さ)
   同じ時刻はノート以外を先に。間隔 > 255 は休符で埋める"
  [evs end]
  (let [evs (vec (sort-by (juxt :t #(if (= :note (:kind %)) 1 0) :ord) evs))
        n   (count evs)]
    (loop [i 0 out []]
      (if (>= i n)
        out
        (let [e     (evs i)
              nxt-t (if (< (inc i) n)
                      (:t (evs (inc i)))
                      (max (+ (:t e) (if (= :note (:kind e)) (:gt e) 0)) (or end 0)))
              delta (max 0 (- nxt-t (:t e)))
              out   (into out (ev->recs e (min 255 delta)))
              out   (loop [rest (- delta 255) out out]
                      (if (pos? rest)
                        (recur (- rest 255) (conj out [0 (min 255 rest) 0 0]))
                        out))]
          (recur (inc i) out))))))

(defn sequence->song
  "javax.sound.midi の Sequence → {:song 曲 :warnings [...]}。opts: {:timebase 48}"
  ([sq] (sequence->song sq {}))
  ([^Sequence sq {:keys [timebase] :or {timebase 48}}]
   (when-not (= (.getDivisionType sq) Sequence/PPQ)
     (throw (ex-info "SMPTE 時間の SMF には対応していません" {})))
   (let [ppq    (.getResolution sq)
         conv   (fn [t] (Math/round (double (/ (* t timebase) ppq))))
         scans  (mapv scan-track (.getTracks sq))
         metas  (sort-by :t (mapcat :metas scans))
         tempos (filter #(= 0x51 (:type %)) metas)
         base   (if-let [t0 (first tempos)] (Math/round (tempo-bpm (:msg t0))) 120)
         base   (max 1 (min 255 base))
         tsig   (when-let [m (first (filter #(= 0x58 (:type %)) metas))]
                  (let [d (.getData ^MetaMessage (:msg m))] [(u8 (aget d 0)) (bit-shift-left 1 (u8 (aget d 1)))]))
         ksig   (when-let [m (first (filter #(= 0x59 (:type %)) metas))]
                  (let [d (.getData ^MetaMessage (:msg m))] (key-sig->byte (.byteValue (aget d 0)) (= 1 (u8 (aget d 1))))))
         tempo-evs (for [m (rest tempos)]
                     {:t (conv (:t m)) :kind :tempo :ord -1
                      :raw (-> (* 64 (/ (tempo-bpm (:msg m)) base)) Math/round (max 1) (min 255))})
         ;; SysEx はチャンネルを持たないので、そのトラックの最初のチャンネルへ (無ければ ch1 として)
         scans  (mapv (fn [sc] (if-let [sx (get-in sc [:chans :sysex])]
                                 (let [chs (sort (filter number? (keys (:chans sc))))
                                       to (or (first chs) 0)]
                                   (-> sc (update :chans dissoc :sysex) (update-in [:chans to] (fnil into []) sx)))
                                 sc))
                      scans)
         groups (for [[ti sc] (map-indexed vector scans)
                      ch (sort (keys (:chans sc)))]
                  {:ti ti :ch ch :name (:name sc) :eot (:eot sc) :evs ((:chans sc) ch)})
         title  (let [s0 (first scans)]
                  (if (and (:name s0) (empty? (:chans s0)) (> (count scans) 1)) (:name s0) ""))
         warn   (volatile! [])
         tracks (vec (map-indexed
                      (fn [gi {:keys [ch name eot evs]}]
                        (let [evs (mapv (fn [e]
                                          (let [e (assoc e :t (conv (:t e)))]
                                            (if (= :note (:kind e))
                                              (let [gt (max 1 (conv (:len e)))]
                                                (when (> gt 255) (vswap! warn conj (str "GT " gt " を 255 にしました (ch" (inc ch) ")")))
                                                (assoc e :gt (min 255 gt)))
                                              e)))
                                        evs)
                              evs (if (zero? gi) (into evs tempo-evs) evs)]
                          {:no (inc gi) :ch (inc ch) :mute? false :rhythm? false
                           :comment (or name "") :key-shift 0 :st-shift 0
                           :recs (abs->records evs (conv eot))}))
                      groups))
         tracks (if (empty? tracks)
                  [{:no 1 :ch 1 :recs [] :mute? false :rhythm? false :comment ""}]
                  tracks)]
     (when (> (count tracks) 36)
       (vswap! warn conj (str "トラックが " (count tracks) " 本あるので 36 本までにしました")))
     {:song {:title title :timebase timebase :tempo base :key (or ksig 0)
             :time-sig (or tsig [4 4]) :tracks (vec (take 36 tracks))}
      :warnings @warn})))

(defn read-smf
  "SMF (.mid) を読む => {:song 曲 :warnings [...]}"
  ([path] (read-smf path {}))
  ([path opts] (sequence->song (MidiSystem/getSequence (File. ^String path)) opts)))

;; ---- 簡易チェック (期待値は手で求めた) ----
(defn run-checks []
  (let [mk   (fn [ppq tracks]                         ; tracks: [[[tick msg] ...] ...]
               (let [sq (Sequence. Sequence/PPQ (int ppq))]
                 (doseq [evs tracks]
                   (let [tr (.createTrack sq)]
                     (doseq [[t m] evs] (.add tr (MidiEvent. m (long t))))))
                 sq))
        sm   (fn [cmd ch d1 d2] (ShortMessage. (int cmd) (int ch) (int d1) (int d2)))
        on   (fn [ch k v] (sm ShortMessage/NOTE_ON ch k v))
        off  (fn [ch k] (sm ShortMessage/NOTE_OFF ch k 0))
        meta (fn [typ bs] (MetaMessage. (int typ) (byte-array (map unchecked-byte bs)) (count bs)))
        tempo (fn [bpm] (let [mpq (quot 60000000 bpm)] (meta 0x51 [(bit-shift-right mpq 16) (bit-shift-right mpq 8) mpq])))
        name (fn [s] (let [b (.getBytes ^String s "US-ASCII")] (MetaMessage. 0x03 b (alength b))))
        recs (fn [r i] (get-in r [:song :tracks i :recs]))
        r1 (sequence->song (mk 96 [[[0 (tempo 100)] [0 (meta 0x58 [3 2 24 8])] [0 (name "Piano")]
                                    [0 (sm ShortMessage/CONTROL_CHANGE 0 7 90)]
                                    [0 (on 0 60 100)] [96 (off 0 60)] [96 (on 0 62 80)] [144 (off 0 62)]]]))
        r2 (sequence->song (mk 48 [[[0 (on 0 60 100)] [0 (on 0 64 100)] [48 (off 0 60)] [48 (on 0 60 0)]
                                    [48 (off 0 64)]]]))
        r3 (sequence->song (mk 48 [[[0 (on 0 60 100)] [10 (off 0 60)] [600 (on 0 62 90)] [648 (off 0 62)]]]))
        r4 (sequence->song (mk 48 [[[0 (on 0 60 100)] [400 (off 0 60)]]]))
        r5 (sequence->song (mk 48 [[[0 (on 0 60 100)] [0 (on 9 36 110)] [48 (off 0 60)] [48 (off 9 36)]]]))
        r6 (sequence->song (mk 48 [[[0 (tempo 120)] [0 (name "Song")]]
                                   [[0 (on 0 60 100)] [48 (off 0 60)] [48 (tempo 60)] [96 (meta 0x2F [])]]]))
        checks
        [["テンポ・拍子・トラック名・CC・ノート (PPQ96 → 48)"
          [(get-in r1 [:song :tempo]) (get-in r1 [:song :time-sig]) (get-in r1 [:song :tracks 0 :comment]) (recs r1 0)]
          [100 [3 4] "Piano" [[0xEB 0 7 90] [60 48 48 100] [62 24 24 80]]]]
         ["和音は ST=0、ベロシティ0 のノートオンもノートオフ"
          (recs r2 0)  [[60 0 48 100] [64 48 48 100]]]
         ["間隔が 255 を超えたら休符で埋める"
          (recs r3 0)  [[60 255 10 100] [0 255 0 0] [0 90 0 0] [62 48 48 90]]]
         ["GT が 255 を超える音は 255 にして警告 (トラック終端までは休符)"
          [(recs r4 0) (count (:warnings r4))]  [[[60 255 255 100] [0 145 0 0]] 1]]
         ["チャンネルごとにトラックを分ける"
          [(mapv :ch (get-in r5 [:song :tracks])) (recs r5 0) (recs r5 1)]
          [[1 10] [[60 48 48 100]] [[36 48 48 110]]]]
         ["フォーマット1: 先頭トラックの名前は曲名、途中のテンポは 0xE7 (64=100%)、終端までの時間"
          [(get-in r6 [:song :title]) (recs r6 0)]
          ["Song" [[60 48 48 100] [0xE7 48 32 0]]]]
         ["SysEx → トラックエクスクルーシブの行 (奇数は F7 で埋める)"
          (let [r (sequence->song (mk 48 [[[0 (SysexMessage. (byte-array (map unchecked-byte [0xF0 0x41 0x10 0x42 0x12 0xF7])) 6)]
                                           [0 (on 0 60 100)] [48 (off 0 60)]]]))]
            (recs r 0))
          [[0x98 0 0 0] [0xF7 0 0x41 0x10] [0xF7 0 0x42 0x12] [0xF7 0 0xF7 0xF7] [60 48 48 100]]]
         ["調号: ニ長調 (#2) → 2、変ロ短調 (♭5) → 5+8+16"
          [(key-sig->byte 2 false) (key-sig->byte -5 true)]  [2 29]]]
        bad (remove (fn [[_ a e]] (= a e)) checks)]
    (if (empty? bad)
      (println (str "polysted-smf: OK " (count checks) "件"))
      (doseq [[l a e] bad] (println "NG" l "期待" e "実際" a)))
    (empty? bad)))

(run-checks)
