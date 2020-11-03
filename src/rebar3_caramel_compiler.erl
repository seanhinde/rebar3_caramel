-module(rebar3_caramel_compiler).
% -behaviour(rebar_compiler).

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
    Mappings = [{".erl", filename:join([Dir, "src"])}],
    #{src_dirs => ["src"],
      include_dirs => [],
      src_ext => ".ml",
      out_mappings => Mappings}.

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
    DepErlsOrdered = digraph_utils:topsort(SubGraph),

    rebar_api:console("Needed: ~p", [NeededErlFiles]),
    rebar_api:console("DepErlsOrdered: ~p", [DepErlsOrdered]),

    {{FirstFiles, Opts1}, {DepErlsOrdered, Opts1}}.

%% Define which files each of the files depends on, including includes and whatnot.
%% This is then used to create a digraph of all existing files to know how to propagate
%% file changes. The Digraph is passed to other callbacks as `G' and annotates all files
%% with their last changed timestamp
%% Prior to 3.14, the `State' argument was not available.
dependencies(File, _Dir, SrcDirs, State) ->
    SrcFiles = lists:append([src_files(SrcDir) || SrcDir <- SrcDirs]),
    Cmd = "caramelc sort-deps " ++ lists:join(" ", SrcFiles),
    rebar_api:console("Cmd: ~s", [Cmd]),
    {ok, Res} = rebar_utils:sh(Cmd, [abort_on_error]),
    SortedDeps = string:tokens(Res, " \r\n"),
    rebar_api:console("File: ~p", [File]),
    Deps = tl(lists:dropwhile(fun(D) -> D =/= File end, SortedDeps)),
    rebar_api:console("Deps: ~p", [Deps]),
    Deps.


compile(Source, [{".erl", SrcDir}], _, Opts) ->
    case os:find_executable("caramelc") of
        false ->
            rebar_api:error("caramelc compiler not found. Make sure you have it installed (https://github.com/AbstractMachinesLab/caramel) and it is in your PATH", []),
            rebar_compiler:error_tuple(Source, [], [], Opts);
        Exec ->
            Command = Exec ++ " compile " ++ Source,
            rebar_api:console("Compiling: ~p", [Source]),
            {ok, Res} = rebar_utils:sh(Command, [{cd, SrcDir}, abort_on_error]),
            rebar_compiler:ok_tuple(Source, Res)
    end.

clean(MlFiles, _AppInfo) ->
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(F, "\\.ml$", ".erl", [unicode]))
       || F <- MlFiles]),
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(F, "\\.ml$", ".cmi", [unicode]))
       || F <- MlFiles]),
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(F, "\\.ml$", ".cmo", [unicode]))
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