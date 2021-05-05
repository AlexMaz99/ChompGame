%%%-------------------------------------------------------------------
%% @doc chompApi public API
%% @end
%%%-------------------------------------------------------------------

-module(chompApi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chompApi_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
