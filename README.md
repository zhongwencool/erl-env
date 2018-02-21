erlenv
=====

## Goal
`eenv` makes retrieving configuration parameters used by application faster and more stable then *application:get_env*.
In a general way, the value of a configuration parameter is retrieved by calling *application:get_env* which caches params in the `ac_tab` ETS table, but the speed of reading ets table grows more and more slowly as concurrent volumes grows. 
`eenv` translates the ETS data into a dynamically constructed and loaded Erlang module. This can significantly reduce latency.
PS: It is only for the situation which needs high reading concurrency and low writing needs.
  
## Rationale
Application stores configuration in `ac_tab` ETS with `{{env, AppName, Key}, Value}`,
`application:get_env/2-3` must lookup `ac_tab` by `{env, AppName, Key}` every time.

1. Configurations in `sys.config`.
```erlang
[
{appname_1, [{ip, "127.0.0.1"}, {port, 8080}]},
{appname_2, [{log_level, debug}, {log_path, "log/debug.log"}]}
].
```
2. These configurations are present in the `ac_tab` table as the release starts.
```erlang
[{{env, appname_1, ip}, "127.0.0.1"}, {{env, appname_1, port}, 8080},
 {{env, appname_2, log_level}, debug}},  {{env, appname_2, log_path}, "log/debug.log"}].
```
3. `eenv` dynamic compiles configurations into beam by calling `eenv:load(AppName)`.
It generates two beam files:
```erlang
% eenv_router.beam
-module(eenv_router).
-export([get/2]).
get(appname_1, Key) -> eenv_appname_1:get(Key);
get(appname_2, Key) -> eenv_appname_2:get(Key);
get(_, _) -> unload.

% eenv_appname_1.beam
-module(eenv_appname_1).
-export([get/1]).
get(ip) -> {ok, "127.0.0.1"};
get(port) -> {ok, 8080};
get(_) -> undefined.

% eenv_appname_2.beam
-module(eenv_appname_2).
-export([get/1]).
get(log_level) -> {ok, debug};
get(log_path) -> {ok, "log/debug.log"};
get(_) -> undefined.
```
we can retrieve parameters by `eenv:get/2-3.`, such as:
```erlang
{ok, IP} = eenv:get(appname_1, ip),
{ok, Port} = eenv:get(appname_1, port),
LogLevel = eenv:get(appname_2, log_level, error),
LogPath = eenv:get(appname_1, log_path, "log/error.log").
```
Each loaded application will generate `eenv_'AppName'.beam` to save own configuration information.
`eenv`'s code is very straightforward(lines < 200) and worth a visit.
  
## Quick Start
1. Load `eenv` when your application starts by `eenv:load(YourAppName)`.
```erlang
% YourApp_app.erl
-behaviour(application).
% Application callbacks
-export([start/2, stop/1]).
start(_StartType, _StartArgs) ->
    eenv:load(your_app),
    your_app_sup:start_link().

stop(_State) ->
    eenv:unload(your_app),
    ok.  
```
2. Replace application's function with eenv's function. 

|      Before           |       After      |
| :--------------------:  |  :-------------:  |
| _application:get/2-3_   |  [eenv:get/2-3](https://github.com/zhongwencool/erl-env/blob/master/src/eenv.erl#L53)   |
| _application:set/2-4_   |  [eenv:set/2-4](https://github.com/zhongwencool/erl-env/blob/master/src/eenv.erl#L79)   |
| _application:unset/2-3_ |  [eenv:unset/2-3](https://github.com/zhongwencool/erl-env/blob/master/src/eenv.erl#L110) |

## Benchmark

### Benchmark Type Specification

- `dict` copy params into process dictionary by `erlang:put/2`, fetch by `erlang:get/1`.
- `rec`  copy params into process state by record(`#state{key = Val}`） and fetch by `#state{key = Val} = State`.
- `map`  copy params into process state by map(`#{key = val}`）and fetch by `#{key => Val} = State`.
- `app`  fetch params by `application:get/2`.
- `eenv` fetch params by `eenv:get/2`.
- `code` fetch params by static beam.

More detailed information see [eenv_benchmark.erl](https://github.com/zhongwencool/erl-env/blob/master/benchmark/eenv_benchmark.erl).
```shell
zsh$ make benchmark
===> Verifying dependencies...
===> Compiling eenv
Starting benchmark...
Erlang/OTP 20 [erts-9.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.0  (abort with ^G)
1> Run ProcNum=1 processes Count=10000 count/process. EmptyLoop:  8.000ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   24.500|               245.000|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   50.800|               508.000|           2.07x  rec |       2.07 x  rec = dict |
eenv|                   73.600|               736.000|           3.00x  rec |       1.45 x dict = eenv |
 map|                   78.400|               784.000|           3.20x  rec |       1.07 x eenv = map  |
code|                  187.600|              1876.000|           7.66x  rec |       2.39 x  map = code |
 app|                  449.200|              4492.000|          18.33x  rec |       2.39 x code = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=10 processes Count=10000 count/process. EmptyLoop:  6.200ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   21.650|               216.500|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   40.780|               407.800|           1.88x  rec |       1.88 x  rec = dict |
 map|                   65.500|               655.000|           3.03x  rec |       1.61 x dict = map  |
code|                  176.260|              1762.600|           8.14x  rec |       2.69 x  map = code |
eenv|                  198.180|              1981.800|           9.15x  rec |       1.12 x code = eenv |
 app|                  990.120|              9901.200|          45.73x  rec |       5.00 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=20 processes Count=10000 count/process. EmptyLoop: 11.600ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   21.600|               216.000|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   37.530|               375.300|           1.74x  rec |       1.74 x  rec = dict |
 map|                   72.705|               727.050|           3.37x  rec |       1.94 x dict = map  |
code|                  243.830|              2438.300|          11.29x  rec |       3.35 x  map = code |
eenv|                  289.220|              2892.200|          13.39x  rec |       1.19 x code = eenv |
 app|                 1288.025|             12880.250|          59.63x  rec |       4.45 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=30 processes Count=10000 count/process. EmptyLoop:  8.800ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   27.173|               271.733|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   56.157|               561.567|           2.07x  rec |       2.07 x  rec = dict |
 map|                   76.130|               761.300|           2.80x  rec |       1.36 x dict = map  |
code|                  274.173|              2741.733|          10.09x  rec |       3.60 x  map = code |
eenv|                  342.797|              3427.967|          12.62x  rec |       1.25 x code = eenv |
 app|                 1800.640|             18006.400|          66.26x  rec |       5.25 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=40 processes Count=10000 count/process. EmptyLoop: 10.050ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   28.973|               289.725|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   52.775|               527.750|           1.82x  rec |       1.82 x  rec = dict |
 map|                   73.282|               732.825|           2.53x  rec |       1.39 x dict = map  |
code|                  336.410|              3364.100|          11.61x  rec |       4.59 x  map = code |
eenv|                  399.125|              3991.250|          13.78x  rec |       1.19 x code = eenv |
 app|                 2241.085|             22410.850|          77.35x  rec |       5.61 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=50 processes Count=10000 count/process. EmptyLoop: 10.400ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   30.304|               303.040|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   51.718|               517.180|           1.71x  rec |       1.71 x  rec = dict |
 map|                   70.316|               703.160|           2.32x  rec |       1.36 x dict = map  |
code|                  384.044|              3840.440|          12.67x  rec |       5.46 x  map = code |
eenv|                  477.252|              4772.520|          15.75x  rec |       1.24 x code = eenv |
 app|                 2726.554|             27265.540|          89.97x  rec |       5.71 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=1000 processes Count=10000 count/process. EmptyLoop: 10.956ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   18.751|               187.506|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   38.307|               383.066|           2.04x  rec |       2.04 x  rec = dict |
 map|                   60.377|               603.768|           3.22x  rec |       1.58 x dict = map  |
code|                 8852.114|             88521.138|         472.10x  rec |     146.61 x  map = code |
eenv|                 9247.929|             92479.286|         493.21x  rec |       1.04 x code = eenv |
 app|                66608.656|            666086.560|        3552.35x  rec |       7.20 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=2000 processes Count=10000 count/process. EmptyLoop: 10.706ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   18.498|               184.981|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   37.629|               376.288|           2.03x  rec |       2.03 x  rec = dict |
 map|                   59.790|               597.904|           3.23x  rec |       1.59 x dict = map  |
code|                15319.678|            153196.775|         828.18x  rec |     256.22 x  map = code |
eenv|                19704.738|            197047.378|        1065.23x  rec |       1.29 x code = eenv |
 app|               165826.738|           1658267.384|        8964.53x  rec |       8.42 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=3000 processes Count=10000 count/process. EmptyLoop: 12.050ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   18.703|               187.035|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   40.587|               405.870|           2.17x  rec |       2.17 x  rec = dict |
 map|                   59.714|               597.141|           3.19x  rec |       1.47 x dict = map  |
code|                24968.905|            249689.051|        1334.99x  rec |     418.14 x  map = code |
eenv|                35843.095|            358430.950|        1916.38x  rec |       1.44 x code = eenv |
 app|               233007.906|           2330079.058|       12457.98x  rec |       6.50 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=5000 processes Count=10000 count/process. EmptyLoop: 11.276ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   18.808|               188.077|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   41.237|               412.371|           2.19x  rec |       2.19 x  rec = dict |
 map|                   58.939|               589.391|           3.13x  rec |       1.43 x dict = map  |
code|                46437.121|            464371.206|        2469.05x  rec |     787.88 x  map = code |
eenv|                59476.405|            594764.053|        3162.35x  rec |       1.28 x code = eenv |
 app|               473787.975|           4737879.751|       25191.22x  rec |       7.97 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=10000 processes Count=10000 count/process. EmptyLoop: 10.829ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   18.884|               188.843|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   39.479|               394.787|           2.09x  rec |       2.09 x  rec = dict |
 map|                   60.383|               603.827|           3.20x  rec |       1.53 x dict = map  |
code|                90985.045|            909850.445|        4818.04x  rec |    1506.81 x  map = code |
eenv|               106457.887|           1064578.867|        5637.39x  rec |       1.17 x code = eenv |
 app|               875741.508|           8757415.083|       46374.15x  rec |       8.23 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=15000 processes Count=10000 count/process. EmptyLoop: 10.773ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   19.000|               189.998|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   42.839|               428.392|           2.25x  rec |       2.25 x  rec = dict |
 map|                   65.579|               655.790|           3.45x  rec |       1.53 x dict = map  |
code|               110231.265|           1102312.651|        5801.70x  rec |    1680.89 x  map = code |
eenv|               154514.285|           1545142.848|        8132.41x  rec |       1.40 x code = eenv |
 app|              1103348.055|          11033480.554|       58071.52x  rec |       7.14 x eenv = app  |
--------------------------------------------------------------------------------------------------------
Run ProcNum=20000 processes Count=10000 count/process. EmptyLoop: 10.800ns/loop
--------------------------------------------------------------------------------------------------------
Type|  Time/(ProcNum*Count) ns|      Time/ProcNum  us|          Time/MinTime|          Time/PrevMinTime|
 rec|                   18.891|               188.906|           1.00x  rec |       1.00 x  rec = rec  |
dict|                   39.378|               393.776|           2.08x  rec |       2.08 x  rec = dict |
 map|                   61.373|               613.734|           3.25x  rec |       1.56 x dict = map  |
code|               145870.193|           1458701.932|        7721.84x  rec |    2376.77 x  map = code |
eenv|               206175.821|           2061758.207|       10914.20x  rec |       1.41 x code = eenv |
 app|              1379935.745|          13799357.453|       73048.79x  rec |       6.69 x eenv = app  |
--------------------------------------------------------------------------------------------------------
```
   
### Conclusion

- `dict`, `rec`, `map` copy params into process memory is faster and consume less resources then `eenv`, `app`, `code`, 
but the problem is that it's very memory consuming(N copy data with N processes running) and difficult to update.
`dict/rec ≈ 2.50`, `map/rec ≈ 3.10`, `map/dict ≈ 1.24`.

- `eenv` and `static code beam` almost the same, depend on the clause order and test sample.

- `eenv/app ≈ 7.20`  and `eenv` cost less CPU resources than `app`.
   
- If CPU resources enough, `eenv` slower then `dict` about 6 times, and cost more CPU.

### Confused
 - Why eenv and code time is increase? it should be constant.
 
   It exhausts CPU resources when lots of processes running in busy circle, so time trend to increase.
   
 - But why dict/map/rec time almost constant?
 
   It only increase not obvious way.
   Because fetch data from memory cost less CPU, and super fast(0.0x us) make it's hard to create lots of processes running at same time. 
   You should see increase when run `make benchmark50000`.  

## ChangeLog

## License
MIT.
          