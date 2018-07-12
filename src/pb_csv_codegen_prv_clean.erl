-module(pb_csv_codegen_prv_clean).

-export([init/1,
    do/1,
    format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}, {protobuf, ?PROVIDER}]).
-define(DESC, "Remove compiled Protocol Buffers rpc from apps.").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, pb_csv},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 pb_csv clean"},
        {short_desc, ?DESC},
        {desc, ""},
        {opts, []}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined -> rebar_state:project_apps(State);
            AppInfo -> [AppInfo]
        end,
    lists:foreach(fun clean/1, Apps),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-define(DEFAULT_MODULE_SUFFIX, "").

clean(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),

    {ok, PbCsvOpts} = dict:find(pb_csv_opts, Opts),
    TargetErlDir = filename:join([AppDir, proplists:get_value(o_erl, PbCsvOpts)]),
    TargetHrlDir = filename:join([AppDir, proplists:get_value(o_hrl, PbCsvOpts)]),

    RouterFile = filename:rootname(filename:basename(proplists:get_value(router, PbCsvOpts))),
    RouterFile0 = filename:basename(RouterFile, ".csv"),
    ErlTarget = filename:join([TargetErlDir, RouterFile0 ++ ".erl"]),
    HrlTarget = filename:join([TargetHrlDir, RouterFile0 ++ ".hrl"]),
    DeleteFiles = [ErlTarget, HrlTarget],
    rebar_api:debug("deleting ~p", [DeleteFiles]),
    rebar_file_utils:delete_each(DeleteFiles).
