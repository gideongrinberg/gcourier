-module(port).
-export([find_port/0]).

find_port() ->
    case gen_tcp:listen(0, [{ip, {127,0,0,1}}, binary, {active, false}]) of
        {ok, Socket} ->
            case inet:port(Socket) of
                {ok, Port} ->
                    gen_tcp:close(Socket),
                    Port;
                Error ->
                    gen_tcp:close(Socket),
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.
