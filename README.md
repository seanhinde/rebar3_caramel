rebar3_caramel
=====

A rebar plugin (at least the start of an experiment for one)

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_caramel, {git, "https://host/user/rebar3_caramel.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_caramel
    ===> Fetching rebar3_caramel
    ===> Compiling rebar3_caramel
    <Plugin Output>
