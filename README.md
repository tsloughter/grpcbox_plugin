grpcbox Plugin
=====

A rebar3 plugin for generating a behaviour per grpc service.

Build
-----

```
$ rebar3 compile
```
        
Use
---

Add the plugin to your rebar config:

```
{deps, [grpcbox]}.

{grpc, [{protos, "priv/protos"},
        {gpb_opts, [{module_name_suffix, "_pb"}]}]}.

{plugins, [grpcbox_plugin]}.
```

Currently `grpcbox` and this plugin are a bit picky and the `gpb` options will always include `[use_packages, maps, {i, "."}, {o, "src"}]`.

Assuming the `priv/protos` directory of your application has the `route_guide.proto` found in this repo, `test/grpcbox_SUITE_data/route_guide.proto`, the output from running the plugin will be:

```
$ rebar3 grpc gen
===> Writing src/route_guide_pb.erl
===> Writing src/grpcbox_route_guide_behaviour.erl
```
