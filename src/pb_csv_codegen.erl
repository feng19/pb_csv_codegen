-module(pb_csv_codegen).

-export([init/1, test/1, test/2]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar3_gpb_prv_compile:init(State0),
    {ok, State2} = rebar3_gpb_prv_clean:init(State1),
    {ok, State3} = pb_csv_codegen_prv_compile:init(State2),
    {ok, State4} = pb_csv_codegen_prv_clean:init(State3),
    {ok, State4}.

test(Type) ->
    {ok, CWD} = file:get_cwd(),
    test(Type, filename:join(CWD, "test")).
test(compile, Dir) ->
    State = get_state(Dir),
    pb_csv_codegen_prv_compile:do(State);
test(clean, Dir) ->
    State = get_state(Dir),
    pb_csv_codegen_prv_clean:do(State).

get_state(Dir) ->
    AppInfo0 = rebar_app_info:new(),
    Opts0 = dict:new(),
    Opts1 = dict:store(gpb_opts, [{module_name_suffix, "_pb"}], Opts0),
    Opts = dict:store(pb_csv_opts, [
        {router, "proto/msg.csv"},
        {msg_prefix, "msg_"},
        {mod_prefix, "mod_"},
        {o_erl, "src/msg"},
        {o_hrl, "include/msg"},
        {erl_tpl, "templates/gpb_rpc/gpb_rpc.erl.tpl"},
%%        {hrl_tpl, "templates/gpb_rpc/gpb_rpc.hrl.tpl"},
        {router_erl_tpl, "templates/gpb_rpc/gpb_rpc_router.erl.tpl"},
        {router_hrl_tpl, "templates/gpb_rpc/gpb_rpc_router.hrl.tpl"}
    ], Opts1),
    AppInfo1 = rebar_app_info:opts(AppInfo0, Opts),
    AppInfo = rebar_app_info:dir(AppInfo1, Dir),
    rebar_state:current_app(rebar_state:new(), AppInfo).