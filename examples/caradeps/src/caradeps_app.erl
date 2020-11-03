%%%-------------------------------------------------------------------
%% @doc caradeps public API
%% @end
%%%-------------------------------------------------------------------

-module(caradeps_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    caradeps_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
