%%%-------------------------------------------------------------------
%% @doc caramel_readme public API
%% @end
%%%-------------------------------------------------------------------

-module(caramel_readme_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    caramel_readme_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
