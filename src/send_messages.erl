-module(send_messages).
-export([go/0, stop/0]).

send_hi(_Port, 0) -> ok;
send_hi(Port, I) ->
    %%             everything in term_to_binary is encoded and sent
    Port ! {self(), {command, term_to_binary({update, {heyo, "hi",9}})}},
    read_mailbox(Port),
    send_hi(Port, I - 1).

read_mailbox(Port) ->
    receive
        {Port, {data, Bin}} -> 
            io:format("got message: ~w~w~n", [Port, Bin])
    after 10 -> io:format("no messages this time~n")
    end.


go() ->
    %%                          -u limits buffering             encoding spec
    Port = open_port({spawn, "python3 -u python_listener.py"},[binary,{packet,4}]),
    send_hi(Port, 500).



stop() -> erlang:halt().
