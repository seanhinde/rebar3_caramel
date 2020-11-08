-module(rebar3_caramel_compiler).
-behaviour(rebar_compiler).

-define(COMPILER_VSN, "v0.0.14").

-export([context/1,
         needed_files/4,
         dependencies/4,
         compile/4,
         clean/2]).

-export([update_opts/2]).

%% specify what kind of files to find and where to find them. Rebar3 handles
%% doing all the searching from these concepts.
context(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Compiler = find_compiler(Dir),
    Mappings = [{".erl", filename:join([Dir, "src"])}],
    #{src_dirs => ["src"],
      include_dirs => [],
      src_ext => ".ml",
      out_mappings => Mappings,
      dependencies_opts => Compiler}.

%% do your own analysis aided by the graph to specify what needs re-compiling.
%% You can use this to add more or fewer files (i.e. compiler options changed),
%% and specify how to schedule their compilation. One thing we do here for
%% erlang files is look at the digraph to only rebuild files with newer
%% timestamps than their build artifacts (which are also in the DAG after the
%% first build) or those with compiler options that changed (the
%% compile_and_track callback lets you annotate artifacts)
needed_files(G, FoundFiles, Mappings, AppInfo) ->
    %% the returned files to build essentially specify a schedule and priority with special
    %% option sets
     %% Files that _must_ be built first like those in parse transforms, with
     %% different build options
    %%{["/top/priority/files.erl"], CompilerOpts},
     %% {Sequential, Parallel} build order for regular files, with shared
     %% compiler options
    %% {{["/path/to/file.erl", ...], ["other/files/mod.erl", ...]}, CompilerOpts}}.

    FirstFiles = [],

    %% Remove first files from found files
    NeededErlFiles = [Source || Source <- FoundFiles,
                           not lists:member(Source, FirstFiles),
                           rebar_compiler:needs_compile(Source, ".erl", Mappings)],

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), caramel_opts, []),
    Opts1 = update_opts(Opts, AppInfo),

    SubGraph = digraph_utils:subgraph(G, NeededErlFiles), % get all the files and deps of those needing a rebuild
    DepErlsOrdered = lists:reverse(digraph_utils:topsort(SubGraph)),

    %% rebar_api:console("Needed: ~p", [NeededErlFiles]),
    %% rebar_api:console("DepErlsOrdered: ~p", [DepErlsOrdered]),

    {{FirstFiles, Opts1}, {DepErlsOrdered, Opts1}}.

%% Define which files each of the files depends on, including includes and whatnot.
%% This is then used to create a digraph of all existing files to know how to propagate
%% file changes. The Digraph is passed to other callbacks as `G' and annotates all files
%% with their last changed timestamp
%% Prior to 3.14, the `State' argument was not available.
dependencies(File, _Dir, SrcDirs, Compiler) ->
    SrcFiles = lists:append([src_files(SrcDir) || SrcDir <- SrcDirs]),
    Cmd = Compiler ++" sort-deps " ++ lists:join(" ", SrcFiles),
    %% rebar_api:console("Cmd: ~s", [Cmd]),
    {ok, Res} = rebar_utils:sh(Cmd, [abort_on_error]),
    SortedDeps = string:tokens(Res, " \r\n"),
    %% rebar_api:console("File: ~p", [File]),
    %% Deps = tl(lists:dropwhile(fun(D) -> D =/= File end, SortedDeps)),
    Deps = lists:takewhile(fun(D) -> D =/= File end, SortedDeps),
    %% rebar_api:console("Deps: ~p", [Deps]),
    Deps.

compile(Source, [{".erl", SrcDir}], AppInfo, Opts) ->
    TopDir = filename:dirname(dict:fetch(base_dir, AppInfo)),
    case find_compiler(TopDir) of
        false ->
            rebar_api:error("caramelc compiler not found. Make sure you have it installed (https://github.com/AbstractMachinesLab/caramel) and it is in your PATH", []),
            rebar_compiler:error_tuple(Source, [], [], Opts);
        Exec ->
            SourceFile = filename:basename(Source),
            Command = Exec ++ " compile " ++ SourceFile,
            %% rebar_api:console("Compiling: ~p", [Source]),
            Gen_dir = filename:join([filename:dirname(SrcDir), "gen", "src"]),
            %% rebar_api:console("Creating: ~p", [Gen_dir]),
            ok = filelib:ensure_dir(filename:join(Gen_dir, "dummy")),
            {ok, _} = file:copy(Source, filename:join(Gen_dir, SourceFile)),
            %% rebar_api:console("Copying: ~p ~p", [Source, filename:join(Gen_dir, SourceFile)]),
            %% rebar_api:console("Compiling: ~p", [Command]),
            {ok, Res} = rebar_utils:sh(Command, [{cd, Gen_dir}, abort_on_error]),
            rebar_compiler:ok_tuple(Source, Res)
    end.

find_compiler(TopDir) ->
    %% Otherwise if there is a compiler in $PATH use that one
    %% Otherwise install the current version of the compiler
    case os:find_executable("caramelc") of
        false ->
            %% Look for a compiler in the plugins dir
            %%AllDeps = rebar_state:all_plugin_deps(State),
            %%case rebar_app_utils:find(rebar_utils:to_binary(rebar3_caramel), AllDeps) of
            %%    {ok, AppInfo} ->
            %%        rebar_api:console("CaramalApp: ~p", [rebar_app_info:fetch_dir(AppInfo)]),
            %%        PluginsDir = rebar_app_info:fetch_dir(AppInfo),
            %%        Compiler = filename:join([PluginsDir, "bin", "caramelc"]),
            %%        ensure_compiler_version(Compiler);
            %%    _ ->
            %%        false
            %%end;
            PluginsDir = filename:join([TopDir, "_build", "default", "plugins", "rebar3_caramel"]),
            Compiler = filename:join([PluginsDir, "caramel", "bin", "caramelc"]),
            ok = ensure_compiler_version(Compiler, PluginsDir),
            Compiler;
        Exec ->
            Exec
    end.

ensure_compiler_version(Compiler, PluginsDir) ->
    case filelib:is_file(Compiler) of
            true ->
                % Is it the right version?
                case rebar_utils:sh(Compiler ++ " --version", [return_on_error]) of
                    {ok, VersionStr} ->
                        Version = parse_version(VersionStr),
                        if Version == ?COMPILER_VSN ->
                              ok;
                            true ->
                                %% Not the right version, fetch the matching version
                                rebar_api:info("Upgrading caramelc from ~s to ~s", [Version, ?COMPILER_VSN]),
                                ok = install_compiler(PluginsDir)
                        end;
                    _ ->
                        rebar_api:info("Installing caramelc ~s", [?COMPILER_VSN]),
                        ok = install_compiler(PluginsDir)
                end;
            false ->
                rebar_api:info("Installing caramelc ~s", [?COMPILER_VSN]),
                ok = install_compiler(PluginsDir)
        end.

parse_version(VersionStr) ->
    [Vsn |_] = string:tokens(VersionStr, "+"),
    "v" ++ Vsn.

install_compiler(PluginsDir) ->
    URL = arch_specific_url(),
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
         {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
            ok = erl_tar:extract({binary, RespBody}, [{cwd, PluginsDir}, compressed]);

        {ok, {{_, StatusCode, _}, _RespHeaders, _RespBody}} ->
             rebar_api:console("Install: ~p", [StatusCode]),
             rebar_api:error("caramelc compiler failed to download ~p", [StatusCode]),
            {error, StatusCode};
        {error, Reason} ->
             {error, Reason}
    end.

arch_specific_url() ->
    BaseUrl = "https://github.com/AbstractMachinesLab/caramel/releases/download/" ++ ?COMPILER_VSN ++ "/caramel-" ++ ?COMPILER_VSN,
    Arch = case os:type() of
        {unix, linux} ->
           "-x86_64-unknown-linux-gnu.tar.gz";
        {unix, darwin} ->
            "-x86_64-apple-darwin.tar.gz";
        _ ->
            ""
    end,
    BaseUrl ++ Arch.

clean(MlFiles, _AppInfo) ->
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(gen_file(F), "\\.ml$", ".erl", [unicode]))
       || F <- MlFiles]),
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(gen_file(F), "\\.ml$", ".cmi", [unicode]))
       || F <- MlFiles]),
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(gen_file(F), "\\.ml$", ".cmo", [unicode]))
       || F <- MlFiles]),
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(gen_file(F))
       || F <- MlFiles]).

gen_file(MlFile) ->
    FileName = filename:basename(MlFile),
    SrcPath = filename:dirname(MlFile),
    filename:join([filename:dirname(SrcPath), "gen", "src", FileName]).


update_opts(Opts, _AppInfo) ->
    Opts.

src_files(Dir) ->
    %% .ml extension is assumed to be valid here
    case file:list_dir(Dir) of
        {ok, Files} ->
            [filename:join(Dir, File)
             || File <- Files,
                filename:extension(File) =:= ".ml"];
        _ ->
            []
    end.