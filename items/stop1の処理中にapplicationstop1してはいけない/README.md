アプリケーションを終了させる時に呼ばれるコールバック関数 `stop/1` で `application:stop/1` を呼び出してはいけません。

## 動機

自身のアプリケーションを終了させる時、他のアプリケーションも終了させてから終わったほうが綺麗にシャットダウンできるかなと思って、以下のようなコードを書きました。

```erlang:myapp_app.erl
-module(myapp_app).

-behaviour(application).

...

-spec stop(_) -> ok.
stop(_State) ->
    application:stop(cowboy),
    application:stop(hackney),
    %% その他起動してるアプリケーションを終了する
    ok.
```

## 何が起きるか

**終了しない**

## 何が起きているか

```erlang:application.erl
stop(Application) ->
    application_controller:stop_application(Application).
```

`application:stop/1` は `application_controller:stop_application/1` を呼んでいるだけです。

```erlang:application_controller.erl
stop_application(AppName) ->
    gen_server:call(?AC, {stop_application, AppName}, infinity).
```

複数のアプリケーションを纏めているアプリケーションコントローラに `gen_server:call/3`、しかも `infinity` で同期呼び出しをしています。
この時点で大分嫌な予感がしますね。

```erlang:application_controller.erl
handle_call({stop_application, AppName}, _From, S) ->
    ...
    stop_appl(AppName, Id, Type),
    ...
```

`stop_appl/3` を呼び出し…

```erlang:application_controller.erl
stop_appl(AppName, Id, Type) when is_pid(Id) ->
    ...
    application_master:stop(Id),
    ...
```

アプリケーションごとに存在するアプリケーションマスターの `application_master:stop/1` を呼び出し…

```erlang:application_master.erl
stop(AppMaster) -> call(AppMaster, stop).

call(AppMaster, Req) ->
    Tag = make_ref(),
    Ref = erlang:monitor(process, AppMaster),
    AppMaster ! {Req, Tag, self()},
    receive 
        {'DOWN', Ref, process, _, _Info} ->
            ok;
        {Tag, Res} ->
            erlang:demonitor(Ref, [flush]),
            Res
    end.
```

`AppMaster`にメッセージパッシングしていますが、その直後に `receive` しているのでこれは同期処理です。

```erlang:application_master.erl
handle_msg({stop, Tag, From}, State) ->
    catch terminate(normal, State),
    ...

terminate(Reason, State = #state{child=Child, children=Children, req=Reqs}) ->
    ...
    terminate_child(Child, State),
    ...

terminate_child(Child, State) ->
    terminate_child_i(Child, State).

terminate_child_i(Child, State) ->
    Child ! {self(), terminate},
    terminate_loop(Child, State).

terminate_loop(Child, State) ->
    receive
        IoReq when element(1, IoReq) =:= io_request ->
            State#state.gleader ! IoReq,
            terminate_loop(Child, State);
        {'EXIT', Child, _} ->
            ok;
        Other ->
            NewState = handle_msg(Other, State),
            terminate_loop(Child, NewState)
    end.
```

`terminate_child_i/2` で `Child ! {self(), terminate}` していますが、その直後 `terminate_loop/2` で `receive` しているため、これも同期処理です。

```erlang:application_master.erl
loop_it(Parent, Child, Mod, AppState) ->
    receive
        ...
        {Parent, terminate} ->
            ...
            catch Mod:stop(NewAppState),
            exit(normal);
        ...
    end.
```

そしてここで　`Mod:stop/1` を呼び出しています。
これが最初に書いた `application` ビヘイビアの `stop/1` のコールバックになります。

どれも全部同期処理であり、アプリケーションコントローラは `Mod:stop/1` の処理が終わるのを待っています。
で、その中で更に `application:stop/1` を呼び出すと、アプリケーションコントローラは既に `Mod:stop/1` を待っている状態なので、**デッドロック** が発生し、永遠に待ち続けることになります。

## 教訓

`stop/1` の中では自分のアプリケーションを終わらせることに注力しましょう。

## 参考

- [otp/lib/kernel/src at maint · erlang/otp](https://github.com/erlang/otp/tree/maint/lib/kernel/src)
