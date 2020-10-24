-module(rebar3_caramel).

-export([init/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, caramel).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->

    Provider = providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 caramel compile"},
            {opts, []},
            {short_desc, "An example rebar compile plugin"},
            {desc, ""}
    ]),

    State1 = rebar_state:add_provider(State, Provider),

    %% This adds the new compiler module:
    State2 = rebar_state:append_compilers(State1, [rebar3_caramel_compiler]),
    rebar_api:debug("State: ~p~n", [State2]),
    %% If needing the new compiler module to take precedence over
    %% other ones (i.e. generating .erl files from another format):
    %% State2 = rebar_state:append_compilers(State1, [translator_mod]),
    {ok, State2}.

