> こうなってくると、一体erlangモジュールの、どの関数が自動インポートBIFで、どの関数が自動インポートBIFでないのかが気になってきます。
> しかしドキュメントを探しても、その一覧というのが見つかりませんでした。

一覧ではないですが http://erlang.org/doc/man/erlang.html の冒頭には「Auto-imported BIFs are listed without module prefix. BIFs listed with module prefix are not auto-imported.」との記載があり、`byte_size(Bitstring) -> integer() >= 0`のように接頭辞無しで表記されている関数は自動インポートで、`erlang:cancel_timer(TimerRef) -> Result`のように`erlang:`接頭辞がつくものはそうではない、とマニュアル上から判別することは一応可能となっていたりします。
