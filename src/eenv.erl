-module(eenv).
%% API
-export([load/1, unload/1]).
-export([get/2, get/3]).
-export([set/2, set/3, set/4]).
-export([unset/2, unset/3]).

-compile([{inline, [
    {load, 1}, {unload, 1},
    {set, 2}, {set, 3}, {unset, 2},
    {get, 2}, {get, 3}]}]).

-define(ROUTER_MOD, eenv_router).
-define(GET, get).
-define(SET, set).
-define(GET_LOADED_APPS, get_loaded_apps).
-define(NONE, none).
-define(UNDEFINED, undefined).
-define(UNLOADED, unloaded).
-define(COMPILE_OPT, [verbose, report_errors, report_warnings]).

%% -module(Module).
-define(make_module(MOD_), erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(MOD_)])).
%% -export([Function/N]).
-define(make_export(FUNC_, N_), erl_syntax:attribute(
    erl_syntax:atom(export),
    [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(FUNC_), erl_syntax:integer(N_))])]
)).
-define(make_app_mod(APP_), list_to_atom("eenv_" ++ atom_to_list(APP_))).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Loads the application configuration for an application into the beam.
-spec load(Applications) -> 'ok' when
    Applications :: atom() | [atom()].
load(App) when is_atom(App) ->
    load([App]);
load(Apps) when is_list(Apps) ->
    load_app_module(Apps),
    update_router_module(load, Apps).

%% @doc Deletes the application configuration beam file.
unload(App) when is_atom(App) ->
    unload([App]);
unload(Apps) when is_list(Apps) ->
    update_router_module(unload, Apps),
    unload_app_module(Apps).

%% @doc Returns the value of configuration parameter Par for Application.
%% Returns undefined if the configuration parameter does not exist:
%% Returns unloaded if application is not loaded by eenv.
%%
-spec get(Application, Par) -> 'undefined' | 'unloaded' | {'ok', Val} when
    Application :: atom(),
    Par :: atom(),
    Val :: term().
get(Application, Par) ->
    ?ROUTER_MOD:get(Application, Par).

%% @doc Works like {@link get/2} but
%% Returns value Default when configuration parameter Par does not exist.
%% Returns <a href="http://erlang.org/doc/apps/kernel/application.html#get_env-3"><tt>application:get_env(App, Par, Default)</tt></a> when application is not loaded by eenv.
-spec get(Application, Par, Default) -> Val when
    Application :: atom(),
    Par :: atom(),
    Default :: term(),
    Val :: term().
get(Application, Par, Default) ->
    case ?ROUTER_MOD:get(Application, Par) of
        {ok, Value} -> Value;
        ?UNDEFINED -> Default;
        ?UNLOADED -> application:get_env(Application, Par, Default)
    end.

%% @doc Sets the value of configuration parameter Par for Application. Same as <a href="http://erlang.org/doc/apps/kernel/application.html#get_env-3"><tt>application:get_env/3</tt></a>.
%% Use this function only if you know what you are doing, that is, on your own applications.
%% It is very application-dependent and configuration parameter-dependent when and how often the value is read by the application.
%% Careless use of this function can put the application in a weird, inconsistent, and malfunctioning state.
-spec set(Application, Par, Val) -> 'ok' when
    Application :: atom(),
    Par :: atom(),
    Val :: term().
set(Application, Par, Val) ->
    ok = application:set_env(Application, Par, Val),
    load_app_module(Application),
    ok.

%% @doc Sets the value of configuration parameter Par for Application. Same as <a href="http://erlang.org/doc/apps/kernel/application.html#set_env-4"><tt>application:set_env/4</tt></a>.
-spec set(Application, Par, Val, Opt) -> 'ok' when
    Application :: atom(),
    Par :: atom(),
    Opt :: [{timeout, timeout()} | {persistent, boolean()}],
    Val :: term().
set(Application, Par, Val, Opt) ->
    ok = application:set_env(Application, Par, Val, Opt),
    load_app_module(Application),
    ok.

%% @doc Works like {@link set/3}. with setting in batches.
-spec set(Application, List) -> 'ok' when
    Application :: atom(),
    List :: [{atom(), term()}].
set(Application, List) ->
    [begin ok = application:set_env(Application, Par, Value) end || {Par, Value} <- List],
    load_app_module(Application),
    ok.

%% @doc Removes the configuration parameter Par and its value for Application.
%% Same as <a href="http://erlang.org/doc/apps/kernel/application.html#unset_env-2"><tt> application:unset_env(Application, Par)</tt></a>.
-spec unset(Application, Pars) -> 'ok' when
    Application :: atom(),
    Pars :: atom() | [atom()].
unset(Application, Par) when is_atom(Par) ->
    ok = application:unset_env(Application, Par),
    load_app_module(Application),
    ok;
unset(Application, Pars) when is_list(Pars) ->
    [begin ok = application:unset_env(Application, Par) end || Par <- Pars],
    load_app_module(Application),
    ok.

%% @doc Removes the configuration parameter Par and its value for Application.
%% Same as <a href="http://erlang.org/doc/apps/kernel/application.html#unset_env-3"><tt> application:unset_env(Application, Par, Opt)</tt></a>.
-spec unset(Application, Pars, Opt) -> 'ok' when
    Application :: atom(),
    Opt :: [{timeout, timeout()} | {persistent, boolean()}],
    Pars :: atom() | [atom()].
unset(Application, Par, Opt) when is_atom(Par) ->
    ok = application:unset_env(Application, Par, Opt),
    load_app_module(Application),
    ok;
unset(Application, Pars, Opt) when is_list(Pars) ->
    [begin ok = application:unset_env(Application, Par, Opt) end || Par <- Pars],
    load_app_module(Application),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
update_router_module(Type, UpdateApps) ->
    Apps = get_loaded_apps(),
    LoadApps = case Type of load -> Apps ++ UpdateApps; unload -> Apps -- UpdateApps end,
    SortApps = lists:usort(LoadApps),
    Forms = router_forms(SortApps),
    {ok, ?ROUTER_MOD, Bin} = compile:forms(Forms, ?COMPILE_OPT),
    code:purge(?ROUTER_MOD),
    {module, ?ROUTER_MOD} = code:load_binary(?ROUTER_MOD, "eenv_router.beam", Bin),
    ok.

get_loaded_apps() ->
    case erlang:function_exported(?ROUTER_MOD, ?GET_LOADED_APPS, 0) of
        true -> ?ROUTER_MOD:?GET_LOADED_APPS();
        false -> []
    end.

%% eenv_router.beam
%%-module(eenv_router).
%%-export([get/2]).
%% get('load_app1', Par) -> eenv_'load_app1':get(Par);
%% get('load_app2', Par) -> eenv_'load_app2':get(Par);
%% get(_, _) -> unloaded.
router_forms(LoadApps) ->
    [begin erl_syntax:revert(X) end || X <- [
        ?make_module(?ROUTER_MOD),
        ?make_export(?GET, 2),
        ?make_export(?GET_LOADED_APPS, 0),
        make_router_func(LoadApps),
        make_loaded_apps_func(LoadApps)
    ]].

make_router_func(LoadApps) ->
    DefaultClause = [
        erl_syntax:clause(
            [erl_syntax:variable("_"), erl_syntax:variable("_")],
            ?NONE,
            [erl_syntax:atom(?UNLOADED)]
        )
    ],
    Clause = make_router_clause(LoadApps, DefaultClause),
    erl_syntax:function(erl_syntax:atom(?GET), Clause).

make_loaded_apps_func(LoadApps) ->
    Clause =
        [erl_syntax:clause([], none,
            [erl_syntax:abstract(LoadApps)])],
    erl_syntax:function(erl_syntax:atom(?GET_LOADED_APPS), Clause).

make_router_clause([], Acc) -> Acc;
make_router_clause([LoadApp | TailApps], Acc) ->
    Clause =
        erl_syntax:clause(
            [erl_syntax:abstract(LoadApp), erl_syntax:variable("Par")],
            ?NONE,
            [erl_syntax:application(erl_syntax:atom(?make_app_mod(LoadApp)),
                erl_syntax:atom(?GET),
                [erl_syntax:variable("Par")])]
        ),
    make_router_clause(TailApps, [Clause | Acc]).

%%eenv_'$load_app'.beam
%%-module(eenv_'$load_app').
%%-export([get/1]).
%% get('par_1') -> {ok, term_val_1};
%% get('par_2') -> {ok, term_val_2};
%% get(_) -> undefined.
load_app_module([]) -> ok;
load_app_module([LoadApp | LoadApps]) ->
    load_app_module(LoadApp),
    load_app_module(LoadApps);
load_app_module(App) when is_atom(App) ->
    AppMod = ?make_app_mod(App),
    Kvs = application:get_all_env(App),
    SortKvs = lists:keysort(1, Kvs),
    Forms = app_forms(AppMod, SortKvs),
    {ok, AppMod, Bin} = compile:forms(Forms, ?COMPILE_OPT),
    code:purge(AppMod),
    BeamFile = atom_to_list(AppMod) ++ ".beam",
    {module, AppMod} = code:load_binary(AppMod, BeamFile, Bin),
    ok.

app_forms(AppName, Kvs) ->
    [begin erl_syntax:revert(X) end || X
        <- [
            ?make_module(AppName),
            ?make_export(?GET, 1),
            make_app_func(Kvs)
        ]
    ].

make_app_func(Kvs) ->
    DefaultClause = [
        erl_syntax:clause(
            [erl_syntax:variable("_")],
            ?NONE,
            [erl_syntax:atom(?UNDEFINED)]
        )
    ],
    Clause = make_app_clause(Kvs, DefaultClause),
    erl_syntax:function(erl_syntax:atom(?GET), Clause).

make_app_clause([], Acc) -> Acc;
make_app_clause([{Par, Val} | Kvs], Acc) ->
    Clause =
        erl_syntax:clause([erl_syntax:abstract(Par)],
            ?NONE,
            [erl_syntax:abstract({ok, Val})]),
    make_app_clause(Kvs, [Clause | Acc]).

%% delete eenv_'$load_app'.beam
unload_app_module([]) -> ok;
unload_app_module([App | UnloadApps]) ->
    unload_app_module(App),
    unload_app_module(UnloadApps);
unload_app_module(UnloadApp) ->
    UnloadAppMod = ?make_app_mod(UnloadApp),
    code:purge(UnloadAppMod),
    code:delete(UnloadAppMod).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_test() ->
    App = crypto,
    Par = test1_par,
    Val = test1_val,
    DefaultVal = test1_default_val,
    ok = eenv:load(crypto),
    ?assertEqual(ok, eenv:set(App, Par, Val)),
    ?assertEqual({ok, Val}, eenv:get(App, Par)),
    ?assertEqual(Val, eenv:get(App, Par, DefaultVal)),
    ?assertEqual(undefined, eenv:get(App, no_exist_par)),
    ?assertEqual(DefaultVal, eenv:get(App, no_exist_par, DefaultVal)),
    ?assertEqual(DefaultVal, eenv:get(no_exist_app, Par, DefaultVal)),
    ok = eenv:unload(App),
    ok.

set_test() ->
    App = crypto,
    Par = test2_par,
    Val = test2_val,
    Par1 = test2_par1,
    Val1 = test2_val1,
    ok = eenv:load(crypto),
    ?assertEqual(ok, eenv:set(App, Par, Val)),
    ?assertEqual(ok, eenv:set(App, Par, Val, [{timeout, 5000}])),
    ?assertEqual({ok, Val}, eenv:get(App, Par)),
    ?assertEqual(ok, eenv:set(App, [{Par1, Val1}, {Par, Val}])),
    ?assertEqual({ok, Val1}, eenv:get(App, Par1)),
    ?assertEqual({ok, Val}, eenv:get(App, Par)),
    ok = eenv:unload(App).

unset_test() ->
    App = crypto,
    Par = test3_par,
    Val = test3_val,
    Par1 = test3_par1,
    Val1 = test3_val1,
    ok = eenv:load(crypto),
    ?assertEqual(ok, eenv:set(crypto, Par, Val)),
    ?assertEqual({ok, Val}, eenv:get(crypto, Par)),
    ?assertEqual(ok, eenv:unset(App, Par)),
    ?assertEqual(ok, eenv:unset(App, Par1, [{timeout, 5000}])),
    ?assertEqual(undefined, eenv:get(crypto, Par)),
    ?assertEqual(undefined, eenv:get(crypto, Par1)),
    ?assertEqual(ok, eenv:set(crypto, [{Par, Val}, {Par1, Val1}])),
    ?assertEqual({ok, Val}, eenv:get(crypto, Par)),
    ?assertEqual({ok, Val1}, eenv:get(crypto, Par1)),
    ?assertEqual(ok, eenv:unset(App, [Par, Par1], [{timeout, 5000}])),
    ?assertEqual(undefined, eenv:get(crypto, Par)),
    ?assertEqual(undefined, eenv:get(crypto, Par1)),
    ok = eenv:unload(App),
    ok.

load_test() ->
    App = crypto,
    AppBeam = list_to_atom("eenv_" ++ atom_to_list(App)),
    ?assertException(error, undef, AppBeam:module_info(exports)),
    ok = eenv:load(App),
    ?assertEqual([{get, 1}, {module_info, 0}, {module_info, 1}], lists:usort(AppBeam:module_info(exports))),
    ?assertEqual({ok, []}, eenv:get(App, included_applications)),
    ?assertEqual(undefined, eenv:get(App, no_exist)),
    ?assertEqual(?UNLOADED, eenv:get(no_load_app, no_exist)),
    ok = eenv:unload(App),
    ?assertException(error, undef, AppBeam:module_info(exports)),
    ?assertEqual(?UNLOADED, eenv:get(App, no_exist)),
    ok.

-endif.
