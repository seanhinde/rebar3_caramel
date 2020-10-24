-module(rebar3_caramel_compiler).
-behaviour(rebar_compiler).

-export([context/1,
         needed_files/4,
         dependencies/3,
         compile/4,
         clean/2]).

-export([update_opts/2]).

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