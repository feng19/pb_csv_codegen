pb_csv_codegen
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { pb_csv_codegen, ".*", {git, "git@host:user/pb_csv_codegen.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 pb_csv_codegen
    ===> Fetching pb_csv_codegen
    ===> Compiling pb_csv_codegen
    <Plugin Output>
