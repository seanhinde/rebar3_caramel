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

    {ok, rebar_state:add_provider(State, Provider)}.


