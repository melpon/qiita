Erlangには自動インポートBIF（auto-imported BIF）と呼ばれる関数が存在しています。
その関数の一覧を調べてみました。

## 自動インポートBIFについて

ErlangのBIF（Built-In Function）は、大体`erlang`モジュールに入っていて、これらの一部の関数は、モジュール名の修飾を書かなくても呼び出すことができます。
例えば`abs/1`の場合、`erlang:abs(-100)`と書かなくても、単に`abs(-100)`と書くだけで構いません。
このモジュール名の修飾が不要な関数を **自動インポートBIF** と呼びます。

当然、自動インポートされていないBIFもあります。
例えば `crc32/1` は自動インポートBIFではありません。
そのため `crc32(<<1, 2, 3>>)` という呼び出しはエラーになります。
必ずモジュール名の修飾を付けて `erlang:crc32(<<1, 2, 3>>)` と書く必要があるのです。

こうなってくると、一体`erlang`モジュールの、どの関数が自動インポートBIFで、どの関数が自動インポートBIFでないのかが気になってきます。
しかしドキュメントを探しても、その一覧というのが見つかりませんでした。

仕方ないのでソースから調べました。以下の通りです。

## 自動インポートBIF一覧

以下は、`erlang`モジュールに入っている関数を全て列挙しています。
自動インポートBIFの場合は「自動インポートBIF」と記述しています。

| 関数 | 自動インポートBIFかどうか |
|:-:|:-:|
| abs/1 | 自動インポートBIF |
| adler32/1 | |
| adler32/2 | |
| adler32_combine/3 | |
| append_element/2 | |
| apply/2 | 自動インポートBIF |
| apply/3 | 自動インポートBIF |
| atom_to_binary/2 | 自動インポートBIF |
| atom_to_list/1 | 自動インポートBIF |
| binary_part/2 | 自動インポートBIF |
| binary_part/3 | 自動インポートBIF |
| binary_to_atom/2 | 自動インポートBIF |
| binary_to_existing_atom/2 | 自動インポートBIF |
| binary_to_float/1 | 自動インポートBIF |
| binary_to_integer/1 | 自動インポートBIF |
| binary_to_integer/2 | 自動インポートBIF |
| binary_to_list/1 | 自動インポートBIF |
| binary_to_list/3 | 自動インポートBIF |
| binary_to_term/1 | 自動インポートBIF |
| binary_to_term/2 | 自動インポートBIF |
| bit_size/1 | 自動インポートBIF |
| bitsize/1 | 自動インポートBIF |
| bitstring_to_list/1 | |
| bump_reductions/1 | |
| byte_size/1 | 自動インポートBIF |
| cancel_timer/1 | |
| cancel_timer/2 | |
| check_old_code/1 | 自動インポートBIF |
| check_process_code/2 | 自動インポートBIF |
| check_process_code/3 | 自動インポートBIF |
| convert_time_unit/3 | |
| crc32/1 | |
| crc32/2 | |
| crc32_combine/3 | |
| date/0 | 自動インポートBIF |
| decode_packet/3 | |
| delete_element/2 | |
| delete_module/1 | 自動インポートBIF |
| demonitor/1 | 自動インポートBIF |
| demonitor/2 | 自動インポートBIF |
| disconnect_node/1 | 自動インポートBIF |
| display/1 | |
| element/2 | 自動インポートBIF |
| erase/0 | 自動インポートBIF |
| erase/1 | 自動インポートBIF |
| error/1 | 自動インポートBIF |
| error/2 | 自動インポートBIF |
| exit/1 | 自動インポートBIF |
| exit/2 | 自動インポートBIF |
| external_size/1 | |
| external_size/2 | |
| float/1 | 自動インポートBIF |
| float_to_binary/1 | 自動インポートBIF |
| float_to_binary/2 | 自動インポートBIF |
| float_to_list/1 | 自動インポートBIF |
| float_to_list/2 | 自動インポートBIF |
| fun_info/1 | |
| fun_info/2 | |
| fun_to_list/1 | |
| function_exported/3 | |
| garbage_collect/0 | 自動インポートBIF |
| garbage_collect/1 | 自動インポートBIF |
| garbage_collect/2 | 自動インポートBIF |
| get/0 | 自動インポートBIF |
| get/1 | 自動インポートBIF |
| get_cookie/0 | |
| get_keys/0 | 自動インポートBIF |
| get_keys/1 | 自動インポートBIF |
| get_stacktrace/0 | |
| group_leader/0 | 自動インポートBIF |
| group_leader/2 | 自動インポートBIF |
| halt/0 | 自動インポートBIF |
| halt/1 | 自動インポートBIF |
| halt/2 | 自動インポートBIF |
| hash/2 | |
| hd/1 | 自動インポートBIF |
| hibernate/3 | |
| insert_element/3 | |
| integer_to_binary/1 | 自動インポートBIF |
| integer_to_binary/2 | 自動インポートBIF |
| integer_to_list/1 | 自動インポートBIF |
| integer_to_list/2 | 自動インポートBIF |
| iolist_size/1 | 自動インポートBIF |
| iolist_to_binary/1 | 自動インポートBIF |
| is_alive/0 | 自動インポートBIF |
| is_atom/1 | 自動インポートBIF |
| is_binary/1 | 自動インポートBIF |
| is_bitstr/1 | 自動インポートBIF |
| is_bitstring/1 | |
| is_boolean/1 | 自動インポートBIF |
| is_builtin/3 | |
| is_float/1 | 自動インポートBIF |
| is_function/1 | 自動インポートBIF |
| is_function/2 | 自動インポートBIF |
| is_integer/1 | 自動インポートBIF |
| is_list/1 | 自動インポートBIF |
| is_map/1 | 自動インポートBIF |
| is_number/1 | 自動インポートBIF |
| is_pid/1 | 自動インポートBIF |
| is_port/1 | 自動インポートBIF |
| is_process_alive/1 | 自動インポートBIF |
| is_record/2 | 自動インポートBIF |
| is_record/3 | 自動インポートBIF |
| is_reference/1 | 自動インポートBIF |
| is_tuple/1 | 自動インポートBIF |
| length/1 | 自動インポートBIF |
| link/1 | 自動インポートBIF |
| list_to_atom/1 | 自動インポートBIF |
| list_to_binary/1 | 自動インポートBIF |
| list_to_bitstring/1 | 自動インポートBIF |
| list_to_existing_atom/1 | 自動インポートBIF |
| list_to_float/1 | 自動インポートBIF |
| list_to_integer/1 | 自動インポートBIF |
| list_to_integer/2 | 自動インポートBIF |
| list_to_pid/1 | 自動インポートBIF |
| list_to_tuple/1 | 自動インポートBIF |
| load_module/2 | 自動インポートBIF |
| load_nif/2 | |
| loaded/0 | |
| localtime/0 | |
| localtime_to_universaltime/1 | |
| localtime_to_universaltime/2 | |
| make_ref/0 | 自動インポートBIF |
| make_tuple/2 | |
| make_tuple/3 | |
| map_size/1 | 自動インポートBIF |
| match_spec_test/3 | |
| max/2 | 自動インポートBIF |
| md5/1 | |
| md5_final/1 | |
| md5_init/0 | |
| md5_update/2 | |
| memory/0 | |
| memory/1 | |
| min/2 | 自動インポートBIF |
| module_loaded/1 | 自動インポートBIF |
| monitor/2 | 自動インポートBIF |
| monitor/3 | 自動インポートBIF |
| monitor_node/2 | |
| monitor_node/3 | |
| monotonic_time/0 | |
| monotonic_time/1 | |
| nif_error/1 | |
| nif_error/2 | |
| node/0 | 自動インポートBIF |
| node/1 | 自動インポートBIF |
| nodes/0 | 自動インポートBIF |
| nodes/1 | 自動インポートBIF |
| now/0 | 自動インポートBIF |
| open_port/2 | 自動インポートBIF |
| phash/2 | |
| phash2/1 | |
| phash2/2 | |
| pid_to_list/1 | 自動インポートBIF |
| port_call/3 | |
| port_close/1 | 自動インポートBIF |
| port_command/2 | 自動インポートBIF |
| port_command/3 | 自動インポートBIF |
| port_connect/2 | 自動インポートBIF |
| port_control/3 | 自動インポートBIF |
| port_info/1 | |
| port_info/2 | |
| port_to_list/1 | |
| ports/0 | |
| pre_loaded/0 | 自動インポートBIF |
| process_display/2 | |
| process_flag/2 | 自動インポートBIF |
| process_flag/3 | 自動インポートBIF |
| process_info/1 | 自動インポートBIF |
| process_info/2 | 自動インポートBIF |
| processes/0 | 自動インポートBIF |
| purge_module/1 | 自動インポートBIF |
| put/2 | 自動インポートBIF |
| raise/3 | |
| read_timer/1 | |
| read_timer/2 | |
| ref_to_list/1 | |
| register/2 | 自動インポートBIF |
| registered/0 | 自動インポートBIF |
| resume_process/1 | |
| round/1 | 自動インポートBIF |
| self/0 | 自動インポートBIF |
| send/2 | |
| send/3 | |
| send_after/3 | |
| send_after/4 | |
| send_nosuspend/2 | |
| send_nosuspend/3 | |
| set_cookie/2 | |
| setelement/3 | 自動インポートBIF |
| size/1 | 自動インポートBIF |
| spawn/1 | 自動インポートBIF |
| spawn/2 | 自動インポートBIF |
| spawn/3 | 自動インポートBIF |
| spawn/4 | 自動インポートBIF |
| spawn_link/1 | 自動インポートBIF |
| spawn_link/2 | 自動インポートBIF |
| spawn_link/3 | 自動インポートBIF |
| spawn_link/4 | 自動インポートBIF |
| spawn_monitor/1 | 自動インポートBIF |
| spawn_monitor/3 | 自動インポートBIF |
| spawn_opt/2 | 自動インポートBIF |
| spawn_opt/3 | 自動インポートBIF |
| spawn_opt/4 | 自動インポートBIF |
| spawn_opt/5 | 自動インポートBIF |
| split_binary/2 | 自動インポートBIF |
| start_timer/3 | |
| start_timer/4 | |
| statistics/1 | 自動インポートBIF |
| suspend_process/1 | |
| suspend_process/2 | |
| system_flag/2 | |
| system_info/1 | |
| system_monitor/0 | |
| system_monitor/1 | |
| system_monitor/2 | |
| system_profile/0 | |
| system_profile/2 | |
| system_time/0 | |
| system_time/1 | |
| term_to_binary/1 | 自動インポートBIF |
| term_to_binary/2 | 自動インポートBIF |
| throw/1 | 自動インポートBIF |
| time/0 | 自動インポートBIF |
| time_offset/0 | |
| time_offset/1 | |
| timestamp/0 | |
| tl/1 | 自動インポートBIF |
| trace/3 | |
| trace_delivered/1 | |
| trace_info/2 | |
| trace_pattern/2 | |
| trace_pattern/3 | |
| trunc/1 | 自動インポートBIF |
| tuple_size/1 | 自動インポートBIF |
| tuple_to_list/1 | 自動インポートBIF |
| unique_integer/0 | |
| unique_integer/1 | |
| universaltime/0 | |
| universaltime_to_localtime/1 | |
| unlink/1 | 自動インポートBIF |
| unregister/1 | 自動インポートBIF |
| whereis/1 | 自動インポートBIF |
| yield/0 | |

## 感想

`erlang`モジュールに入っている241関数中、152関数がBIFでした。
さすがにこの量を覚えれる気がしないので、

- とりあえず`erlang:`を書かずに記述してみて、エラーが出たら直す
- 全部に`erlang:`を付けて呼び出す

のどちらかの方針で良さそうです。

## 参考

- [otp/erl_internal.erl at cc25f4cd184ca41ba935e1d2d14eb250e68f11d2 · erlang/otp](https://github.com/erlang/otp/blob/cc25f4cd184ca41ba935e1d2d14eb250e68f11d2/lib/stdlib/src/erl_internal.erl#L254)
