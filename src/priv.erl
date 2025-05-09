-module(priv).
-export([find_bin/1]).

-include_lib("kernel/include/file.hrl").

find_bin(Name) ->
    NameStr = normalize_name(Name),
    % {ok, App} = application:get_application(?MODULE),
    PrivDir    = code:priv_dir(gcourier),
    OS   = detect_os(),
    Arch = detect_arch(),
    Ext  = case OS of "windows" -> ".exe"; _ -> "" end,
    Path = filename:join([PrivDir, OS, Arch, NameStr ++ Ext]),
    list_to_binary(Path).

%% normalize input to a charlist
-spec normalize_name(atom() | binary() | list()) -> list().
normalize_name(Name) when is_atom(Name)   -> atom_to_list(Name);
normalize_name(Name) when is_binary(Name) -> binary_to_list(Name);
normalize_name(Name) when is_list(Name)   -> Name.

%% detect OS via os:type/0
-spec detect_os() -> string().
detect_os() ->
    case os:type() of
        {win32, _}      -> "windows";
        {unix, darwin}  -> "darwin";
        {unix, linux}   -> "linux";
        {unix, freebsd} -> "freebsd";
        {unix, openbsd} -> "openbsd";
        {unix, sunos}   -> "sunos";
        Other           -> io_lib:format("~p", [Other]) 
    end.

-spec detect_arch() -> string().
detect_arch() ->
    RawArchFull = string:to_lower(erlang:system_info(system_architecture)),
    [RawArch | _] = string:tokens(RawArchFull, "-"),
    normalize_arch(RawArch).

-spec normalize_arch(string()) -> string().
normalize_arch("amd64")   -> "x86_64";
normalize_arch("x86-64")  -> "x86_64";
normalize_arch("x86_64")  -> "x86_64";
normalize_arch("i386")    -> "x86";
normalize_arch("i686")    -> "x86";
normalize_arch("arm64")   -> "aarch64";
normalize_arch("aarch64") -> "aarch64";
normalize_arch("armv7l")  -> "armv7";
normalize_arch(Other)     -> Other.
