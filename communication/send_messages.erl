-module(send_messages).
-export([go/0, stop/0]).

send_hi(_Port, 0) -> ok;
send_hi(Port, I) ->
    Port ! {self(), {command, term_to_binary({init_state, {heyo, "hi",9}})}},
    timer:sleep(10),
    send_hi(Port, I - 1).

go() ->
    Port = open_port({spawn, "python3 -u python_listener.py"},[binary,{packet,4}]),
    send_hi(Port, 500).

stop() -> erlang:halt().
