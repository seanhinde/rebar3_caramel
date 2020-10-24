-module(rebar3_caramel_compiler).

-export([init/1, do/1, format_error/1]).

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

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
                  undefined ->
                      rebar_state:project_apps(State);
                  AppInfo ->
                      [AppInfo]
              end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         OutDir = rebar_app_info:out_dir(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.ml\$"),

         CompileFun = fun(Source, Opts1) ->
                              caramel_compile(Opts1, Source, OutDir)
                      end,

         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

caramel_compile(_Opts, Source, OutDir) ->
    Cmd = "caramelc compile " ++ Source,
    rebar_api:console("Compile: ~s", [Cmd]),
    rebar_api:info("Writing out ~s", [OutFile]).


context(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Mappings = [{".erl", filename:join([Dir, "src"])}],
    #{src_dirs => ["src"],
      include_dirs => [],
      src_ext => ".ml",
      out_mappings => Mappings}.

needed_files(_, FoundFiles, Mappings, AppInfo) ->
    FirstFiles = [],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles,
                           not lists:member(Source, FirstFiles),
                           rebar_compiler:needs_compile(Source, ".erl", Mappings)],

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), caramel_opts, []),
    Opts1 = update_opts(Opts, AppInfo),

    {{FirstFiles, Opts1}, {RestFiles, Opts1}}.

dependencies(File, _Dir, SrcDirs) ->
    SrcFiles = lists:append([src_files(SrcDir) || SrcDir <- SrcDirs]),
    Cmd = "caramelc sort-deps " ++ lists:join(" ", SrcFiles),
    rebar_api:console("Deps: ~s", [Cmd]),
    [].

compile(Source, [{_, _}], _, Opts) ->
    Cmd = "caramelc compile " ++ Source,
    rebar_api:console("Compile: ~s", [Cmd]),
    case os:cmd(Cmd) of
        {ok, _} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            rebar_compiler:error_tuple(Source, Es, Ws, Opts)
    end.

clean(MlFiles, _AppInfo) ->
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(F, "\\.ml$", ".erl", [unicode]))
       || F <- MlFiles]).

update_opts(Opts, _AppInfo) ->
    Opts.

src_files(Dir) ->
    %% .mib extension is assumed to be valid here
    case file:list_dir(Dir) of
        {ok, Files} ->
            [filename:join(Dir, File)
             || File <- Files,
                filename:extension(File) =:= ".ml"];
        _ ->
            []
    end.