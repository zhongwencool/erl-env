-module(eenv_mock).

%% API
-export([get/2]).

get(ssl, Key) ->
    eenv_mock_ssl:get(Key);
get(_, _) ->
    unload.
