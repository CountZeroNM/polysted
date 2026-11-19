# CLAUDE.md — polysted (STED2 に学んだ MIDI エディタ、Clojure)

## 最初に読むもの
- `README.md`(概要・REPL コマンド)
- `実装状況と仕様.md`(仕様の根拠・実装状況・検証ログ・次の作業)
- `doc/機能目録.md`(機能ごとの実装の場所・原作の根拠・学習ポイント。機能を足したら該当の項目も更新する)

## 構成
- ソースは `src/polysted/*.clj`(名前空間 `polysted.xxx`、`tui-base` などハイフンの名前はファイル名では `_`)。各名前空間は使う名前空間を `:require` で読む。
- `dev/user.clj` は REPL の user 名前空間(`s` が `polysted.session`)。`deps.edn` のエイリアス: `:tui` `:gui` `:check` `:dev` `:repl`。
- 新しい名前空間を足したら、`src/polysted/check.clj` の `check-nss` と、README の「ファイル構成」・`doc/機能目録.md` の 1.2 を直す。

## 動作確認
```bash
DISPLAY=:0 clojure -M:check     # 全部の run-checks。NG が 1 つでもあれば終了コード 1
```
- 各名前空間の `run-checks` は読み込んだだけでは走らない。REPL では `(polysted.check/run-all)` か `(polysted.edit/run-checks)` など。
- TUI の対話確認は実端末がないので、Python の `pty.fork()` で `clojure -M:tui x.edn` を起動し、キーを書き込んで画面出力を確認する(一時スクリプトは scratchpad に置く)。
- 静的解析: `clojure -Sdeps '{:deps {clj-kondo/clj-kondo {:mvn/version "2024.08.01"}}}' -M -m clj-kondo.main --lint src dev samples`(警告 0 を保つ)。
- `polysted.gui` は AWT を使うので、Claude Code の環境では `DISPLAY=:0` を付けて実行する(既定の `DISPLAY=wayland-0` では X に繋がらない。付けないと `:check` は GUI を飛ばす)。GUI の対話確認は、同じ JVM で `polysted.gui/run!` を開き、パネルに合成した `KeyEvent`/`MouseEvent` を送って、フレームを画像に描いて見る。
- 出力に `NG` が無いことを確認する。音は Claude Code からは確認できないので、再生確認はユーザーに頼む。

## 方針
- 返答は日本語で、結論から簡潔に。
- 仕様は STED2 のソース(https://github.com/cat2151/STed2.07m)や資料で確認できたものだけを事実とし、推測・設計判断は md に【推測】【設計判断】と明記する。
- コアは純粋関数 `(state, args...) -> state`。新しい編集コマンドは `src/polysted/session.clj` の `commands` 表に登録し、REPL 層に短い名前の関数を足す。
- 新しい関数には、期待値を手で追って求めた簡易チェックを `run-checks` に追加する。
- 機能を追加したら README・`実装状況と仕様.md`・`doc/機能目録.md` を更新する。
- 原作を指す「STED2」(と原作のファイル名・関数名)は置き換えない。本作の名前は polysted。
- `S` は `defonce`。REPL で古い状態を消すには再起動する。
