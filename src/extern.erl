-module(extern).
-export([arch/0, untar/2]).

arch() ->
    list_to_binary(erlang:system_info(system_architecture)).

untar(PathBin, DestBin) ->
    %% Convert Gleam String (binary) to Erlang charlist
    Path = binary_to_list(PathBin),
    Dest = binary_to_list(DestBin),

    %% Ensure destination directory exists
    case filelib:is_dir(Dest) of
        false ->
            {error, <<"Destination directory does not exist">>};
        true ->
            %% Perform extraction
            case erl_tar:extract(Path, [{cwd, Dest}, compressed]) of
                ok ->
                    %% Return unit value as undefined
                    {ok, undefined};
                {error, Reason} ->
                    %% Format the tar error and return as binary
                    ErrStr = erl_tar:format_error(Reason),
                    {error, list_to_binary(ErrStr)}
            end
    end.