rebar3_caramel
=====

A rebar plugin to invoke the [caramelc](https://github.com/AbstractMachinesLab/caramel) ocaml compiler in a rebar project.

Early days. Some things on the list:

- dependency ordering between ml modules
- clean doesn't remove all the ml intermediate files
- tests
- an example project

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
