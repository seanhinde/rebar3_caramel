rebar3_caramel
=====

A rebar compiler plugin to invoke the [caramelc](https://github.com/AbstractMachinesLab/caramel) ocaml compiler in a rebar project.

Early days. Some things on the TODO list:

- tests
- bring the caramel erlang runtime into the project in some smart way

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_caramel, ".*", {git, "https://github.com/seanhinde/rebar3_caramel.git", {branch, "main"}}}
    ]}.

This plugin uses the [custom compiler modules](http://rebar3.org/docs/extending/custom_compiler_modules/) feature of rebar3, hence requires rebar3 version >= 3.7.0.

The plugin generates erlang files from ocaml sources in gen/src. For this reason you must add the following to your rebar.config:

    {src_dirs, ["src", "gen/src"]}.
    {profiles, [
        {test, [{src_dirs, ["src", "test", "gen/src", "gen/test"]}]}
]}.

If you want to use the erlang runtime for caramel in your project you will need to manually copy the pre-generated erlang files from your caramel distro into your erlang project (right now this is only process.erl). This is a short term solution.

Examples
----

There are example projects under examples/

caramel_readme implements the simple server from the caramel README page. This cheats a little by including a snapshot of process.erl from the caramel runtime. 

The server is not yet started by the supervisor. The server start function needs to return {ok, Pid} from ocaml. Not sure how to do that yet.

caradeps is aimed at experimenting with compile dependencies.