# サンプル曲

polysted で開いて、聴いて、いじって試すための曲です。どれも EDN（polysted の保存形式）で、テキストエディタでも読めます。

```bash
clojure -M:tui examples/01_kirakira.edn    # 端末で開く (p で再生、q で終了)
clojure -M:gui examples/02_minuet_g.edn    # ウィンドウで開く
```

REPL からは `(s/load! "examples/03_eight_beat.edn")` のあと `(s/play!)`。

| ファイル | 曲 | 見どころ |
|---|---|---|
| `01_kirakira.edn` | きらきら星（フランス民謡）。メロディ・和音・ベースの 3 トラック、4/4、12 小節 | **SAME**（小節複写）: 7〜12 小節目は前の小節を指す 1 行だけ。MML の和音（`'ceg'96,92`）、音色・音量・パンのコントローラ |
| `02_minuet_g.edn` | メヌエット ト長調 BWV Anh. 114（クリスティアン・ペツォールト作とされる）。右手・左手の 2 トラック、3/4、16 小節 | **調号**（ト長調）: MML の `f` が F♯ になる。**リピート** `[` `]2` で 16 小節を 2 回。付点 2 分音符（ST 144）はレコードで直接 |
| `03_eight_beat.edn` | Eight Beat（polysted のオリジナル）。ドラム・ベース・エレピの 3 トラック、4/4、8 小節 | **リズムトラック**（RHY ON、ch 10）: TUI の `Ctrl-G` でリズムパターン編集が開く。4 小節目はフィルイン、ほかは SAME |

## 権利

- きらきら星（フランス民謡）とメヌエット ト長調（18 世紀の曲）は、著作権の保護期間を過ぎた曲です。
- 3 曲とも、編曲と打ち込みは polysted のサンプルとして作ったもので、リポジトリの [MIT License](../LICENSE) で自由に使えます。

## 作り直す

曲は [`make_examples.clj`](make_examples.clj) で組み立てています（`polysted.session` の純粋関数を使う。REPL の `s/mml` や `s/bar` と同じ関数）。

```bash
clojure -M examples/make_examples.clj      # プロジェクトのルートから。examples/*.edn を書き直す
```
