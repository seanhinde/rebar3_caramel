-module(rebar3_caramel_plugin).

-export([init/1]).

-define(PROVIDER, rebar3_caramel).
-define(NAMESPACE, caramel).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, ?NAMESPACE},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 compile"},  % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin to compile caramel projects"},
            {desc, "A rebar plugin to compile caramel/ocaml .ml files"}
    ]),
  
    %% This adds the new compiler module:
    State1 = rebar_state:append_compilers(State, [rebar3_caramel_compiler]),
    %% If needing the new compiler module to take precedence over
    %% other ones (i.e. generating .erl files from another format):
    %% State2 = rebar_state:append_compilers(State1, [translator_mod]),
    {ok, State1}.

