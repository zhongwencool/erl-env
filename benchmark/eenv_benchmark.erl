-module(eenv_benchmark).

-export([test/1]).
-export([test/2]).
-export([test_code/0]).

-include("eenv_benchmark.hrl").

-define(INCR, 20).
-define(CAPACITY, 200).


-define(PAR, test_par).
-define(PAR_1, test_par1).
-define(PAR_20, test_par20).
-define(PAR_40, test_par40).
-define(PAR_60, test_par60).
-define(PAR_80, test_par80).
-define(PAR_100, test_par100).
-define(PAR_120, test_par120).
-define(PAR_140, test_par140).
-define(PAR_160, test_par160).
-define(PAR_180, test_par180).
-define(PAR_5, test_par1).
-define(PAR_25, test_par20).
-define(PAR_45, test_par40).
-define(PAR_65, test_par60).
-define(PAR_85, test_par80).
-define(PAR_105, test_par100).
-define(PAR_125, test_par120).
-define(PAR_145, test_par140).
-define(PAR_165, test_par160).
-define(PAR_185, test_par180).
-define(VAL, {ok, test_val}).

test(Count) ->
    test(1, Count),
    test(10, Count),
    test(20, Count),
    test(30, Count),
    test(40, Count),
    test(50, Count),
    test(1000, Count),
    test(2000, Count),
    test(3000, Count),
    test(5000, Count),
    test(10000, Count),
    test(15000, Count),
    test(20000, Count),
    ok.

%% @doc ProcNum: Process number running  task at the same time.
%%      Count  : Each process running function times.
-spec test(ProcNum, Count) -> 'ok' when
    ProcNum :: integer(),
    Count :: integer().
test(ProcNum, Count) ->
    setup(),
    
    LoopFunc = fun() -> run_loop(Count) end,
    {LoopTime, _LoopCount} = benchmark(ProcNum, LoopFunc),
    timer:sleep(400),
    EenvFunc = fun() -> run_eenv(Count) end,
    {EenvTime, EenvCount} = benchmark(ProcNum, EenvFunc),
    timer:sleep(400),
    CodeFunc = fun() -> run_code(Count) end,
    {CodeTime, CodeCount} = benchmark(ProcNum, CodeFunc),
    timer:sleep(400),
    AppFunc = fun() -> run_application(Count) end,
    {AppTime, AppCount} = benchmark(ProcNum, AppFunc),
    timer:sleep(400),
    DictFunc = fun() -> init_dict(), run_dict(Count) end,
    {DictTime, DictCount} = benchmark(ProcNum, DictFunc),
    timer:sleep(400),
    RecFunc = fun() -> run_record(Count, #rec{}) end,
    {RecTime, RecCount} = benchmark(ProcNum, RecFunc),
    timer:sleep(400),
    MapFunc = fun() -> run_map(Count, init_map()) end,
    {MapTime, MapCount} = benchmark(ProcNum, MapFunc),
    
    Times = [
        {eenv, EenvTime - LoopTime, EenvCount},
        {code, CodeTime - LoopTime, CodeCount},
        {app, AppTime - LoopTime, AppCount},
        {dict, DictTime - LoopTime, DictCount},
        {rec, RecTime - LoopTime, RecCount},
        {map, MapTime - LoopTime, MapCount}],
    printf(ProcNum, Count, LoopTime, Times),
    cleanup().

init_dict() ->
    [begin
         Par = list_to_atom(atom_to_list(?PAR) ++ integer_to_list(Seq)),
         erlang:put(Par, ?VAL)
     end || Seq <- lists:seq(1, ?CAPACITY)].

init_map() ->
    lists:foldl(fun(Seq, Acc) ->
        Par = list_to_atom(atom_to_list(?PAR) ++ integer_to_list(Seq)),
        maps:put(Par, ?VAL, Acc)
                end, maps:new(), lists:seq(1, ?CAPACITY)).

setup() ->
    application:load(ssl),
    ok = eenv:load(ssl),
    List = [begin {list_to_atom(atom_to_list(?PAR) ++ integer_to_list(S)), test_val}
            end || S <- lists:seq(1, ?CAPACITY)],
    ok = eenv:set(ssl, List).

printf(ProcNum, Count, LoopTime, Times) ->
    [{MinType, MinTime, MinCount}|_] = ListTime = lists:keysort(2, Times),
    io:format("Run ProcNum=~p processes Count=~p count/process. EmptyLoop: ~6.3fns/loop~n",
        [ProcNum, Count - MinCount, LoopTime/((Count - MinCount)*ProcNum/?INCR)]),
    io:format("~s~n", [lists:duplicate(104, $-)]),
    io:format("~4s|~25s|~22s|~22s|~26s|~n", ['Type', 'Time/(ProcNum*Count) ns',
        'Time/ProcNum  us', 'Time/MinTime', 'Time/PrevMinTime']),
    
    lists:foldl(fun({Type, Time, ExCount}, {LastType, LastTime}) ->
        RunCount = Count - ExCount,
        case MinTime == 0 orelse LastTime == 0 of
            true ->
                io:format("~4s|~25.3f|~22.3f|~15.2p  ~4s |~15.2p ~4s=~4s |~n",
                    [Type, Time/(RunCount*ProcNum), Time/(ProcNum*1000), Time, MinType, Time, Type, LastType]);
            false when Type =:= app orelse Type =:= rec orelse Type =:= map ->
                io:format("~4s|~25.3f|~22.3f|~15.2fx ~4s |~11.2f x ~4s = ~3s  |~n",
                    [Type, Time/(RunCount*ProcNum), Time/(ProcNum*1000), Time/MinTime, MinType, Time/LastTime, LastType, Type]);
            false ->
                io:format("~4s|~25.3f|~22.3f|~15.2fx ~4s |~11.2f x ~4s = ~4s |~n",
                    [Type, Time/(RunCount*ProcNum), Time/(ProcNum*1000), Time/MinTime, MinType, Time/LastTime, LastType, Type])
        end,
        {Type, Time}
                end,
        {MinType, MinTime}, ListTime),
    io:format("~s~n", [lists:duplicate(104, $-)]).

cleanup() ->
    ok = eenv:unload(ssl),
    application:unload(ssl),
    ok.

benchmark(ProcNum, Func) ->
    erlang:garbage_collect(),
    Pids = [begin spawn(fun() -> worker(Func) end) end|| _<- lists:seq(1, ProcNum)],
    StartMsg = {start, self()},
    collect(Pids, StartMsg, ProcNum).

worker(Func) ->
    receive {start, Pid} ->
        Start = erlang:monotonic_time(),
        ExCount = Func(),
        End = erlang:monotonic_time(),
        Time = erlang:convert_time_unit(End - Start, native, nano_seconds),
        erlang:send(Pid, {Time, ExCount})
    end.

collect([], _Msg, ProcNum) -> finish(ProcNum, 0, 0);
collect([Pid|Pids], Msg, ProcNum) ->
    erlang:send(Pid, Msg),
    collect(Pids, Msg, ProcNum).

finish(0, TimeAcc, CountAcc) -> {TimeAcc, CountAcc};
finish(ProcNum, TimeAcc, CountAcc) ->
    receive {Time, Count} ->
        finish(ProcNum - 1, Time + TimeAcc, CountAcc + Count)
    end.

run_loop(Count)when Count =< 0 -> Count;
run_loop(Count) ->
    run_loop(Count - ?INCR).

run_eenv(Count)when Count =< 0 -> Count;
run_eenv(Count) ->
    ?VAL = eenv:get(ssl, ?PAR_1),
    ?VAL = eenv:get(ssl, ?PAR_20),
    ?VAL = eenv:get(ssl, ?PAR_40),
    ?VAL = eenv:get(ssl, ?PAR_60),
    ?VAL = eenv:get(ssl, ?PAR_80),
    ?VAL = eenv:get(ssl, ?PAR_100),
    ?VAL = eenv:get(ssl, ?PAR_120),
    ?VAL = eenv:get(ssl, ?PAR_140),
    ?VAL = eenv:get(ssl, ?PAR_160),
    ?VAL = eenv:get(ssl, ?PAR_180),
    ?VAL = eenv:get(ssl, ?PAR_5),
    ?VAL = eenv:get(ssl, ?PAR_25),
    ?VAL = eenv:get(ssl, ?PAR_45),
    ?VAL = eenv:get(ssl, ?PAR_65),
    ?VAL = eenv:get(ssl, ?PAR_85),
    ?VAL = eenv:get(ssl, ?PAR_105),
    ?VAL = eenv:get(ssl, ?PAR_125),
    ?VAL = eenv:get(ssl, ?PAR_145),
    ?VAL = eenv:get(ssl, ?PAR_165),
    ?VAL = eenv:get(ssl, ?PAR_185),
    run_eenv(Count - ?INCR).

run_application(Count)when Count =< 0 -> Count;
run_application(Count) ->
    ?VAL = application:get_env(ssl, ?PAR_1),
    ?VAL = application:get_env(ssl, ?PAR_20),
    ?VAL = application:get_env(ssl, ?PAR_40),
    ?VAL = application:get_env(ssl, ?PAR_60),
    ?VAL = application:get_env(ssl, ?PAR_80),
    ?VAL = application:get_env(ssl, ?PAR_100),
    ?VAL = application:get_env(ssl, ?PAR_120),
    ?VAL = application:get_env(ssl, ?PAR_140),
    ?VAL = application:get_env(ssl, ?PAR_160),
    ?VAL = application:get_env(ssl, ?PAR_180),
    ?VAL = application:get_env(ssl, ?PAR_5),
    ?VAL = application:get_env(ssl, ?PAR_25),
    ?VAL = application:get_env(ssl, ?PAR_45),
    ?VAL = application:get_env(ssl, ?PAR_65),
    ?VAL = application:get_env(ssl, ?PAR_85),
    ?VAL = application:get_env(ssl, ?PAR_105),
    ?VAL = application:get_env(ssl, ?PAR_125),
    ?VAL = application:get_env(ssl, ?PAR_145),
    ?VAL = application:get_env(ssl, ?PAR_165),
    ?VAL = application:get_env(ssl, ?PAR_185),
    run_application(Count - ?INCR).

run_dict(Count)when Count =< 0 -> Count;
run_dict(Count) ->
    ?VAL = erlang:get(?PAR_1),
    ?VAL = erlang:get(?PAR_20),
    ?VAL = erlang:get(?PAR_40),
    ?VAL = erlang:get(?PAR_60),
    ?VAL = erlang:get(?PAR_80),
    ?VAL = erlang:get(?PAR_100),
    ?VAL = erlang:get(?PAR_120),
    ?VAL = erlang:get(?PAR_140),
    ?VAL = erlang:get(?PAR_160),
    ?VAL = erlang:get(?PAR_180),
    ?VAL = erlang:get(?PAR_5),
    ?VAL = erlang:get(?PAR_25),
    ?VAL = erlang:get(?PAR_45),
    ?VAL = erlang:get(?PAR_65),
    ?VAL = erlang:get(?PAR_85),
    ?VAL = erlang:get(?PAR_105),
    ?VAL = erlang:get(?PAR_125),
    ?VAL = erlang:get(?PAR_145),
    ?VAL = erlang:get(?PAR_165),
    ?VAL = erlang:get(?PAR_185),
    run_dict(Count - ?INCR).

run_code(Count)when Count =< 0 -> Count;
run_code(Count) ->
    ?VAL = eenv_mock:get(ssl, ?PAR_1),
    ?VAL = eenv_mock:get(ssl, ?PAR_20),
    ?VAL = eenv_mock:get(ssl, ?PAR_40),
    ?VAL = eenv_mock:get(ssl, ?PAR_60),
    ?VAL = eenv_mock:get(ssl, ?PAR_80),
    ?VAL = eenv_mock:get(ssl, ?PAR_100),
    ?VAL = eenv_mock:get(ssl, ?PAR_120),
    ?VAL = eenv_mock:get(ssl, ?PAR_140),
    ?VAL = eenv_mock:get(ssl, ?PAR_160),
    ?VAL = eenv_mock:get(ssl, ?PAR_180),
    ?VAL = eenv_mock:get(ssl, ?PAR_5),
    ?VAL = eenv_mock:get(ssl, ?PAR_25),
    ?VAL = eenv_mock:get(ssl, ?PAR_45),
    ?VAL = eenv_mock:get(ssl, ?PAR_65),
    ?VAL = eenv_mock:get(ssl, ?PAR_85),
    ?VAL = eenv_mock:get(ssl, ?PAR_105),
    ?VAL = eenv_mock:get(ssl, ?PAR_125),
    ?VAL = eenv_mock:get(ssl, ?PAR_145),
    ?VAL = eenv_mock:get(ssl, ?PAR_165),
    ?VAL = eenv_mock:get(ssl, ?PAR_185),
    run_code(Count - ?INCR).


run_record(Count, _Rec)when Count =< 0 -> Count;
run_record(Count, Rec) ->
    #rec{?PAR_1 = {ok, test_val}} = Rec,
    #rec{?PAR_20 = {ok, test_val}} = Rec,
    #rec{?PAR_40 = {ok, test_val}} = Rec,
    #rec{?PAR_60 = {ok, test_val}} = Rec,
    #rec{?PAR_80 = {ok, test_val}} = Rec,
    #rec{?PAR_100 = {ok, test_val}} = Rec,
    #rec{?PAR_120 = {ok, test_val}} = Rec,
    #rec{?PAR_140 = {ok, test_val}} = Rec,
    #rec{?PAR_160 = {ok, test_val}} = Rec,
    #rec{?PAR_180 = {ok, test_val}} = Rec,
    #rec{?PAR_5 = {ok, test_val}} = Rec,
    #rec{?PAR_25 = {ok, test_val}} = Rec,
    #rec{?PAR_45 = {ok, test_val}} = Rec,
    #rec{?PAR_65 = {ok, test_val}} = Rec,
    #rec{?PAR_85 = {ok, test_val}} = Rec,
    #rec{?PAR_105 = {ok, test_val}} = Rec,
    #rec{?PAR_125 = {ok, test_val}} = Rec,
    #rec{?PAR_145 = {ok, test_val}} = Rec,
    #rec{?PAR_165 = {ok, test_val}} = Rec,
    #rec{?PAR_185 = {ok, test_val}} = Rec,
    run_record(Count - ?INCR, Rec).

run_map(Count, _Map)when Count =< 0 -> Count;
run_map(Count, Map) ->
    #{?PAR_1 := {ok, test_val}} = Map,
    #{?PAR_20 := {ok, test_val}} = Map,
    #{?PAR_40 := {ok, test_val}} = Map,
    #{?PAR_60 := {ok, test_val}} = Map,
    #{?PAR_80 := {ok, test_val}} = Map,
    #{?PAR_100 := {ok, test_val}} = Map,
    #{?PAR_120 := {ok, test_val}} = Map,
    #{?PAR_140 := {ok, test_val}} = Map,
    #{?PAR_160 := {ok, test_val}} = Map,
    #{?PAR_180 := {ok, test_val}} = Map,
    #{?PAR_5 := {ok, test_val}} = Map,
    #{?PAR_25 := {ok, test_val}} = Map,
    #{?PAR_45 := {ok, test_val}} = Map,
    #{?PAR_65 := {ok, test_val}} = Map,
    #{?PAR_85 := {ok, test_val}} = Map,
    #{?PAR_105 := {ok, test_val}} = Map,
    #{?PAR_125 := {ok, test_val}} = Map,
    #{?PAR_145 := {ok, test_val}} = Map,
    #{?PAR_165 := {ok, test_val}} = Map,
    #{?PAR_185 := {ok, test_val}} = Map,
    run_map(Count - ?INCR, Map).

test_code() ->
    Count = 10000,
    MinProcNum = 100,
    MaxProcNum = 1000,
    io:format("~s~n", [lists:duplicate(30, $-)]),
    io:format("|Process|Time/(Count*ProcNum)|~n"),
    Func = fun() -> run_code(Count) end,
    lists:foreach(fun(ProcNum) ->
        Time = benchmark(ProcNum, Func),
        io:format("|~6w | ~17.4fns| ~n", [ProcNum, Time/(Count*ProcNum)])
                  end, lists:seq(MinProcNum, MaxProcNum, 100)),
    io:format("~s~n", [lists:duplicate(30, $-)]),
    ok.
