# CLAUDE.md — polysted (STED2 に学んだ MIDI エディタ、Clojure)

## 最初に読むもの
- `README.md`(概要・REPL コマンド)
- `実装状況と仕様.md`(仕様の根拠・実装状況・検証ログ・次の作業)
- `doc/機能目録.md`(機能ごとの実装の場所・原作の根拠・学習ポイント。機能を足したら該当の項目も更新する)

## 動作確認
ソースは各ファイルを `load-file` で読む(`deps.edn` は REPL 起動用のエイリアス `:rebel` `:polysted` だけ。`polysted_all.clj` が全部を順に読む)。ロード時に各ファイルの `run-checks` が走る。新しいファイルを足したら、下の読み込み順・`polysted_all.clj`・`tui.clj`・`gui.clj` の並びも直す。

```bash
clojure -M -e '(load-file "polysted.clj") (load-file "polysted_rcp.clj") (load-file "polysted_edit.clj") (load-file "polysted_notename.clj") (load-file "polysted_edn.clj") (load-file "polysted_smf.clj") (load-file "polysted_replace.clj") (load-file "polysted_rhythm.clj") (load-file "polysted_ctrl.clj") (load-file "polysted_roll.clj") (load-file "polysted_visual.clj") (load-file "polysted_score.clj") (load-file "polysted_record.clj") (load-file "polysted_session.clj") (load-file "polysted_tui_base.clj") (load-file "polysted_tui_screens.clj") (load-file "polysted_tui.clj") (load-file "polysted_gui.clj")'
```
- 読み込み順は上のとおり(後のファイルが前の名前空間を require する)。
- `polysted.clj` はロード時にデモとして `test.mid` を書き出す。
- TUI の対話確認は実端末がないので、Python の `pty.fork()` で `clojure -M tui.clj` を起動し、キーを書き込んで画面出力を確認する(一時スクリプトは scratchpad に置く)。
- 静的解析: `clojure -Sdeps '{:deps {clj-kondo/clj-kondo {:mvn/version "2024.08.01"}}}' -M -m clj-kondo.main --lint polysted*.clj`(本体は警告 0 を保つ。起動スクリプト `tui.clj` は対象外)。
- `polysted_gui.clj` のチェックは AWT を使うので、Claude Code の環境では `DISPLAY=:0` を付けて実行する(既定の `DISPLAY=wayland-0` では X に繋がらない)。GUI の対話確認は、同じ JVM で `polysted-gui/run!` を開き、パネルに合成した `KeyEvent`/`MouseEvent` を送って、フレームを画像に描いて見る。
- 出力に `NG` が無いことを確認する。音は Claude Code からは確認できないので、再生確認はユーザーに頼む。

## 方針
- 返答は日本語で、結論から簡潔に。
- 仕様は STED2 のソース(https://github.com/cat2151/STed2.07m)や資料で確認できたものだけを事実とし、推測・設計判断は md に【推測】【設計判断】と明記する。
- コアは純粋関数 `(state, args...) -> state`。新しい編集コマンドは `polysted_session.clj` の `commands` 表に登録し、REPL 層に短い名前の関数を足す。
- 新しい関数には、期待値を手で追って求めた簡易チェックを `run-checks` に追加する。
- 機能を追加したら README・`実装状況と仕様.md`・`doc/機能目録.md` を更新する。
- 原作を指す「STED2」(と原作のファイル名・関数名)は置き換えない。本作の名前は polysted。
- `S` は `defonce`。REPL で古い状態を消すには再起動する。
