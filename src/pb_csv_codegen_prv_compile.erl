-module(pb_csv_codegen_prv_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'compile').
-define(DEPS, [{default, app_discovery}, {protobuf, ?PROVIDER}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, pb_csv},
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 pb_csv_codegen"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "A rebar plugin"},
        {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:current_app(State) of
        undefined ->
            Apps = rebar_state:project_apps(State),
            NewApps = lists:map(fun compile/1, Apps),
            {ok, rebar_state:project_apps(State, NewApps)};
        AppInfo ->
            CurrentApp = compile(AppInfo),
            {ok, rebar_state:current_app(State, CurrentApp)}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-define(DEFAULT_MODULE_SUFFIX, "").

compile(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts, ?DEFAULT_MODULE_SUFFIX),

    {ok, PbCsvOpts} = dict:find(pb_csv_opts, Opts),
    RouterFile = filename:join(AppDir, proplists:get_value(router, PbCsvOpts)),
    RouterFile0 = filename:basename(RouterFile, ".csv"),
    TargetErlDir = filename:join(AppDir, proplists:get_value(o_erl, PbCsvOpts)),
    ErlTarget = filename:join([TargetErlDir, RouterFile0 ++ ".erl"]),

    case filelib:last_modified(RouterFile) > filelib:last_modified(ErlTarget) of
        true ->
            gen_router(AppDir, RouterFile, RouterFile0, ErlTarget, TargetErlDir, ModuleNameSuffix, PbCsvOpts);
        _ -> skip
    end,

    update_erl_first_files(TargetErlDir, AppDir, AppInfo).

gen_router(AppDir, RouterFile, RouterFile0, ErlTarget, TargetErlDir, ModuleNameSuffix, PbCsvOpts) ->
    {ok, Binary} = file:read_file(RouterFile),
    ModPrefix = proplists:get_value(mod_prefix, PbCsvOpts),
    #{input_list := InputList0, output_list := OutputList0, protos := Protos} =
        parse_csv(binary_to_list(Binary), ModuleNameSuffix, ModPrefix),

    InputList = lists:reverse(InputList0),
    OutputList = lists:reverse(OutputList0),

    RouterErlTplFile0 = proplists:get_value(router_erl_tpl, PbCsvOpts),
    RouterErlTplFile = bbmustache:parse_file(filename:join([AppDir, RouterErlTplFile0])),

    CmdList = lists:sort(InputList ++ OutputList),

    ErlRenderData = [
        {file, RouterFile0},
        {file_upper, string:to_upper(RouterFile0)},
        {input_list, add_is_last(InputList)},
        {output_list, add_is_last(OutputList)},
        {cmd_list, add_is_last(CmdList)}
    ],
    compile_tpl(ErlTarget, RouterErlTplFile, ErlRenderData),

    HrlRenderData = [
        {file, RouterFile0},
        {file_upper, string:to_upper(RouterFile0)},
        {cmd_list, CmdList}
    ],

    TargetHrlDir = filename:join(AppDir, proplists:get_value(o_hrl, PbCsvOpts)),
    HrlTarget = filename:join([TargetHrlDir, RouterFile0 ++ ".hrl"]),
    RouterHrlTplFile0 = proplists:get_value(router_hrl_tpl, PbCsvOpts),
    RouterHrlTplFile = bbmustache:parse_file(filename:join([AppDir, RouterHrlTplFile0])),
    compile_tpl(HrlTarget, RouterHrlTplFile, HrlRenderData),

    gen_mods(AppDir, TargetErlDir, PbCsvOpts, maps:to_list(Protos)).

gen_mods(AppDir, TargetErlDir, PbCsvOpts, Protos) ->
    MsgPrefix = proplists:get_value(msg_prefix, PbCsvOpts),
    ErlTpl = bbmustache:parse_file(filename:join(AppDir, proplists:get_value(erl_tpl, PbCsvOpts))),
    [begin
         FileName = MsgPrefix ++ ProtoName,
         ErlRenderData = [
             {proto_name, FileName},
             {callback_list, CallbackList}
         ],

         ErlTarget = filename:join([TargetErlDir, FileName ++ ".erl"]),
         compile_tpl(ErlTarget, ErlTpl, ErlRenderData)
     end || {ProtoName, CallbackList} <- Protos].

parse_csv(String, ModuleNameSuffix, ModPrefix) ->
    Lines = string:tokens(String, [$\r, $\n]),
    lists:foldl(
        fun(Line, Acc) ->
            parse_csv_line(Line, ModuleNameSuffix, ModPrefix, Acc)
        end, #{input_list => [], output_list => [], protos => #{}}, Lines).

parse_csv_line(Line, ModuleNameSuffix, ModPrefix,
    #{input_list := InputList, output_list := OutputList, protos := Protos0} = Acc) ->
    [Cmd0, MsgName, ProtoName] = string:tokens(Line, ","),
    GpbProto = ProtoName ++ ModuleNameSuffix,
    HandleMod = ModPrefix ++ ProtoName,
    Cmd = list_to_integer(Cmd0),
    BaseData = [
        {cmd, Cmd},
        {proto_name, ProtoName},
        {proto_name_upper, string:to_upper(ProtoName)},
        {gpb_proto, GpbProto},
        {handle_mod, HandleMod},
        {msg, MsgName},
        {msg_upper, string:to_upper(camelhump_to_underline(MsgName))}
    ],
    Len = length(MsgName),
    case lists:nthtail(Len - 4, MsgName) of
        "Resp" ->
            OutputData = [{type, output} | BaseData],
            Acc#{output_list => [OutputData | OutputList]};
        "Push" ->
            OutputData = [{type, output} | BaseData],
            Acc#{output_list => [OutputData | OutputList]};
        [_, $R, $e, $q] ->
            HandleFunc = camelhump_to_underline(lists:sublist(MsgName, Len - 3)),
            InputData = [{type, input}, {handle_func, HandleFunc} | BaseData],
            Callback = [
                {gpb_proto, GpbProto},
                {callback, HandleFunc},
                {req, MsgName}
            ],
            Protos = maps:update_with(ProtoName, fun(List) -> [Callback | List] end, [], Protos0),
            Acc#{input_list => [InputData | InputList], protos =>  Protos}
    end.

camelhump_to_underline([H | T]) when H >= $A andalso H =< $Z ->
    [$_, H + 32 | camelhump_to_underline(T)];
camelhump_to_underline([H | T]) ->
    [H | camelhump_to_underline(T)];
camelhump_to_underline([]) -> [].

-define(AUTO_GEN_HEAD,
    <<"%% -*- coding: utf-8 -*-\n%% Automatically generated, do not edit\n%% Generated by pb_csv_codegen\n">>).

compile_tpl(Target, Tpl, RenderData) ->
    IoData = bbmustache:compile(Tpl, RenderData, [{key_type, atom}]),
    ok = file:write_file(Target, [?AUTO_GEN_HEAD, IoData]).

add_is_last([]) -> [];
add_is_last(List) ->
    [H | T] = lists:reverse(List),
    lists:reverse([[{is_last, true} | H] | T]).

update_erl_first_files(TargetErlDir, AppDir, AppInfo) ->
    case filelib:wildcard(filename:join(TargetErlDir, "*.erl")) of
        [] -> AppInfo;
        ErlFirstFiles0 ->
            PrefixLen = length(AppDir),
            ErlFirstFiles = [lists:nthtail(PrefixLen, ErlFirstFile) || ErlFirstFile <- ErlFirstFiles0],
            OldOpts = rebar_app_info:opts(AppInfo),
            NewOpts =
                case dict:find(erl_first_files, OldOpts) of
                    {ok, OldErlFirstFiles} ->
                        dict:store(erl_first_files, OldErlFirstFiles ++ ErlFirstFiles, OldOpts);
                    error ->
                        dict:store(erl_first_files, ErlFirstFiles, OldOpts)
                end,
            rebar_app_info:opts(AppInfo, NewOpts)
    end.