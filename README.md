rebar3_caramel
=====

A rebar compiler plugin to invoke the [caramelc](https://github.com/AbstractMachinesLab/caramel) ocaml compiler in a rebar project.

Early days. Some things on the TODO list:

- dependency ordering between ml modules
- clean doesn't remove all the ml intermediate files
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

This also means that no additional configuration is required in your rebar.config beyond loading the plugin.

If you want to use the erlang runtime for caramel in your project you will need to manually copy the pre-generated erlang files from your caramel distro into your erlang project (right now this is only process.erl).

Example
----

There is an example project under examples that implements the simple server from the caramel README page. This cheats a little by including a snapshot of process.erl from the caramel runtime. 

The server is not yet started by the supervisor. The server start function needs to return {ok, Pid} from ocaml. Not sure how to do that yet.