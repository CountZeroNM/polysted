;; src/polysted/mml.clj  --  STED2風 MML -> イベント列 -> SMF(.mid) 書き出し (最小版・動作未確認)
;; 使い方:  (require 'polysted.mml)   (polysted.mml/write-demo! "test.mid") でデモを書き出す
(ns polysted.mml
  (:require [clojure.string :as str])
  (:import (javax.sound.midi Sequence MidiSystem ShortMessage MetaMessage MidiEvent SysexMessage)
           (java.io File)))

;; ------------------------------------------------------------
;; 調号 (STED2: bit0-2=個数, bit3=♭, bit4=短調(無視))
;; ------------------------------------------------------------
(def base-semi {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11})
(def sharp-order [\F \C \G \D \A \E \B])
(def flat-order  [\B \E \A \D \G \C \F])

(defn key-sig-shifts [ks]
  (let [n (bit-and ks 7)
        flat? (bit-test ks 3)]
    (zipmap (take n (if flat? flat-order sharp-order))
            (repeat (if flat? -1 1)))))

;; ------------------------------------------------------------
;; MML パーサ (KEY_SUB.C mml_inp の仕様どおり)
;; ------------------------------------------------------------
(defn- read-int [s i]
  (let [d (re-find #"^\d*" (subs s i))]
    [(when (seq d) (Long/parseLong d)) (+ i (count d))]))

(defn parse-mml
  [text {:keys [st gt vel key-sig rhythm?] :or {st 48 gt 46 vel 64 key-sig 0}}]
  (let [s (str/upper-case text)
        n (count s)
        shifts (if rhythm? {} (key-sig-shifts key-sig))]
    (loop [i 0, oct 4, st st, gt gt, vel vel, chord nil, out []]
      (if (>= i n)
        out
        (let [c (get s i), i (inc i)]
          (case c
            \[ (recur i oct st gt vel chord (conj out {:type :loop-start}))
            \] (let [[v j] (read-int s i)]
                 (recur j oct st gt vel chord
                        (if (and v (<= 1 v 256))
                          (conj out {:type :loop-end :count v})
                          out)))
            (\' \`)
            (if (nil? chord)
              (recur i oct st gt vel [] out)
              (let [[a j]  (read-int s i)
                    ss     (if (and a (<= 1 a 255)) a st)
                    [g j2] (if (= (get s j) \,) (read-int s (inc j)) [nil j])
                    gg     (if (and g (<= 1 g 255)) g gt)
                    notes  (mapv (fn [k] {:type :note :key k :st 0 :gt gg :vel vel}) chord)
                    notes  (if (seq notes)
                             (assoc-in notes [(dec (count notes)) :st] ss)
                             notes)]
                (recur j2 oct st gt vel nil (into out notes))))
            \O (let [d (get s i)
                     [o i] (if (and d (Character/isDigit (char d)))
                             [(Character/digit (char d) 10) (inc i)]
                             [oct i])
                     [o i] (if (= (get s i) \.) [-1 (inc i)] [o i])]
                 (recur i o st gt vel chord out))
            \L (let [[a j] (read-int s i)]
                 (if (and a (<= 1 a 64))
                   (let [x (quot 192 a)] (recur j oct x (- x 2) vel chord out))
                   (recur j oct st gt vel chord out)))
            \V (let [[a j] (read-int s i)]
                 (recur j oct st gt (if (and a (<= 0 a 127)) a vel) chord out))
            \< (recur i (if (< oct 9) (inc oct) oct) st gt vel chord out)
            \> (recur i (if (pos? oct) (dec oct) oct) st gt vel chord out)
            (\A \B \C \D \E \F \G \R)
            (let [acc (get s i)
                  explicit? (contains? #{\N \= \# \+ \-} acc)
                  i (if explicit? (inc i) i)
                  [a i] (read-int s i)
                  [ss gg] (if (and a (<= 1 a 64))
                            (let [x (quot 192 a)] [x (- x 2)])
                            [st gt])
                  base (base-semi c)
                  adj (cond (nil? base) 0
                            explicit? (get {\# 1 \+ 1 \- -1} acc 0)
                            :else (get shifts c 0))
                  k (when base (+ (* 12 (inc oct)) base adj))]
              (cond
                (nil? base)
                (recur i oct st gt vel chord
                       (if chord out (conj out {:type :rest :st ss})))

                (not (<= 0 k 127))
                (recur i oct st gt vel chord out)

                (and chord (< (count chord) 16))
                (recur i oct st gt vel (conj chord k) out)

                :else
                (recur i oct st gt vel chord
                       (conj out {:type :note :key k :st ss :gt gg :vel vel}))))
            ;; 読めない文字は黙って無視 (原実装と同じ)
            (recur i oct st gt vel chord out)))))))

;; ------------------------------------------------------------
;; 展開: SAME(0xFC) と ループ
;; ------------------------------------------------------------
(defn expand-same
  "SAME → 参照先の小節の中身 + 小節線。小節番号は0始まり。"
  [events]
  (loop [es events cur [] bars [] out []]
    (if-let [e (first es)]
      (case (:type e)
        :bar  (recur (rest es) [] (conj bars cur) (conj out e))
        ;; SAMEの直前に小節線が無ければ、直前の内容は暗黙に1小節として閉じる
        :same (let [bars (if (seq cur) (conj bars cur) bars)
                    out  (if (seq cur) (conj out {:type :bar}) out)
                    t    (vec (get bars (:bar e) []))]   ; 参照先が無ければ空小節
                (recur (rest es) []
                       (conj bars t)
                       (conj (into out t) {:type :bar})))
        (recur (rest es) (conj cur e) bars (conj out e)))
      out)))

(defn expand-loops
  "回数1〜254のループを展開(合計n回)。255以上は展開しない。入れ子は内側から。"
  [events]
  (let [stack
        (reduce
         (fn [stack e]
           (case (:type e)
             :loop-start (conj stack [e])
             :loop-end
             (if (< (count stack) 2)
               stack
               (let [with-marker (peek stack)
                     parent (peek (pop stack))
                     body (subvec with-marker 1)
                     c (:count e)
                     added (if (<= 1 c 254)
                             (vec (apply concat (repeat c body)))
                             (-> [(first with-marker)] (into body) (conj e)))]
                 (conj (pop (pop stack)) (into parent added))))
             (conj (pop stack) (conj (peek stack) e))))
         [[]] events)]
    (into (first stack) (apply concat (rest stack)))))

(defn expand-track [tr]
  (update tr :events (comp expand-loops expand-same)))

;; ------------------------------------------------------------
;; レコード列 → 絶対tickのイベント列
;; ------------------------------------------------------------
(defn key-shift-val
  "トラックの K#+ (1バイト) → 半音数。-64〜63 は 7 ビットの2の補数、128 以上 (OFF) と nil は 0 (TRKSET.C)"
  [b]
  (let [b (or b 0)] (cond (>= b 128) 0 (> b 63) (- b 128) :else b)))

(defn st-shift-val
  "トラックの ST+ (1バイト) → tick 数 (-99〜99、2の補数)"
  [b]
  (let [b (or b 0)] (if (> b 127) (- b 256) b)))

(defn track->abs
  "トラックのイベント列 → 絶対 tick のイベント列。K#+ (:key-shift) をノートのキーに足す (0〜127 を外れる音は鳴らさない)"
  [{:keys [events ch key-shift]}]
  (let [ks (key-shift-val key-shift)]
  (loop [es events t 0 ch ch out []]
    (if-let [e (first es)]
      (let [out (case (:type e)
                  :note  (let [k (+ (:key e) ks)]
                           (if (and (pos? (:gt e)) (pos? (:vel e)) (<= 0 k 127))
                             (conj out {:tick t :ch ch :kind :note :key k
                                        :len (:gt e) :vel (:vel e)})
                             out))
                  :tempo (conj out {:tick t :kind :tempo :bpm (:value e)})
                  :sysex (conj out {:tick t :kind :sysex :bytes (:bytes e)})
                  (:cc :bend :prog :bank-prog :chan-pressure :key-pressure)
                  (conj out (assoc e :tick t :ch ch :kind (:type e)))
                  out)
            ch (if (= :channel (:type e)) (:ch e) ch)]
        (recur (rest es) (+ t (:st e 0)) ch out))
      out))))

;; ------------------------------------------------------------
;; SMF 書き出し
;; ------------------------------------------------------------
;; データバイトは下位7ビットにする (範囲外の値、例えば音色 0x81 で例外にならないように)
(defn- sm [cmd ch d1 d2]
  (ShortMessage. (int cmd) (int (mod (dec ch) 16)) (int (bit-and d1 0x7F)) (int (bit-and d2 0x7F))))

(defn ->sequence [{:keys [timebase tracks]}]
  (let [sq (Sequence. Sequence/PPQ (int timebase))]
    (doseq [tr tracks
            :let [mt (.createTrack sq)
                  add! (fn [msg tick] (.add mt (MidiEvent. msg (long tick))))]
            ev (track->abs (expand-track tr))
            ;; ST+ (:st-shift) でトラックの演奏タイミングをずらす (テンポは動かさない)
            :let [ch (:ch ev)
                  tick (if (= :tempo (:kind ev)) (:tick ev) (max 0 (+ (:tick ev) (st-shift-val (:st-shift tr)))))]
            :when (or (#{:tempo :sysex} (:kind ev)) (and ch (<= 1 ch 32)))]
      (case (:kind ev)
        :note (do (add! (sm ShortMessage/NOTE_ON ch (:key ev) (:vel ev)) tick)
                  (add! (sm ShortMessage/NOTE_OFF ch (:key ev) 0) (+ tick (:len ev))))
        :tempo (let [mpq (quot 60000000 (:bpm ev))
                     b (byte-array [(unchecked-byte (bit-shift-right mpq 16))
                                    (unchecked-byte (bit-shift-right mpq 8))
                                    (unchecked-byte mpq)])]
                 (add! (MetaMessage. 0x51 b 3) tick))
        :cc   (add! (sm ShortMessage/CONTROL_CHANGE ch (:ctl ev) (:value ev)) tick)
        :bend (let [v (+ 8192 (:value ev))]
                (add! (sm ShortMessage/PITCH_BEND ch
                          (bit-and v 0x7f) (bit-and (bit-shift-right v 7) 0x7f)) tick))
        :prog (add! (sm ShortMessage/PROGRAM_CHANGE ch (:prog ev) 0) tick)
        :bank-prog (do (add! (sm ShortMessage/CONTROL_CHANGE ch 0 (:bank ev)) tick)
                       (add! (sm ShortMessage/PROGRAM_CHANGE ch (:prog ev) 0) tick))
        :sysex (let [bs (byte-array (map unchecked-byte (:bytes ev)))]
                 (add! (SysexMessage. bs (alength bs)) tick))
        :chan-pressure (add! (sm ShortMessage/CHANNEL_PRESSURE ch (:value ev) 0) tick)
        :key-pressure  (add! (sm ShortMessage/POLY_PRESSURE ch (:key ev) (:value ev)) tick)))
    sq))

(defn write-mid! [song path]
  (MidiSystem/write (->sequence song) 1 (File. ^String path)))

;; ------------------------------------------------------------
;; デモ: test.mid を書き出す
;; ------------------------------------------------------------
(def demo
  {:timebase 48
   :tracks [{:no 1 :ch 1
             :events (into [{:type :tempo :st 0 :value 120}
                            {:type :prog :st 0 :prog 0}
                            {:type :cc :st 0 :ctl 7 :value 100}]
                           (parse-mml "l8 o4 [cdefgab<c]2" {}))}]})

(defn write-demo!
  "デモの曲を SMF に書き出す (以前はロード時に test.mid を書き出していた)"
  ([] (write-demo! "test.mid"))
  ([path]
   (write-mid! demo path)
   (println "wrote" path " (note events:"
            (count (filter #(= :note (:kind %)) (track->abs (expand-track (first (:tracks demo))))))
            ")")))
