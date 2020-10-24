-module(rebar3_caramel).

-export([init/1]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->

    %% This adds the new compiler module:
    State1 = rebar_state:append_compilers(State, [rebar3_caramel_compiler]),
    rebar_api:debug("State: ~p~n", [State1]),
    %% If needing the new compiler module to take precedence over
    %% other ones (i.e. generating .erl files from another format):
    %% State2 = rebar_state:append_compilers(State1, [translator_mod]),
    {ok, State1}.

