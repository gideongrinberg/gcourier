-module(runcmd).
% -behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {os_pid :: pid()}).

-spec start_link(binary() | string() | [string()], [string() | binary()]) ->
          {ok, pid()} | {error, any()}.
start_link(Cmd, Args) ->
    gen_server:start_link(?MODULE, {Cmd, Args}, []).

-spec stop(pid()) -> ok.
stop(GenServerPid) ->
    gen_server:cast(GenServerPid, shutdown).

init({Cmd, Args}) ->
    process_flag(trap_exit, true),

    %% Start the erlexec manager if needed; ignore "already_started"
    case exec:start_link([]) of
        {ok, ExecMgr} ->
            logger:debug("erlexec manager started: ~p", [ExecMgr]);
        {error, {already_started, ExecMgr}} ->
            logger:debug("erlexec manager already running: ~p", [ExecMgr])
    end,

    %% Now spawn & link the OS process
    OsPid = exec:run_link(
              Cmd,
              [stdout, stderr, link, {args, Args}]
            ),

    {ok, #state{os_pid=OsPid}}.

handle_info({stdout, OsPid, Data}, OsPid) ->
    io:put_chars(Data),
    {noreply, OsPid};
handle_info({stderr, OsPid, Data}, OsPid) ->
    io:put_chars(stderr, Data),
    {noreply, OsPid};

handle_info({'EXIT', OsPid, Reason}, OsPid) ->
    exit({shutdown, Reason});

handle_info(shutdown, OsPid) ->
    exec:stop(OsPid),
    {stop, normal, OsPid};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, OsPid) ->
    catch exec:stop(OsPid),
    ok.
