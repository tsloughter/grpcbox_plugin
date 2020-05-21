grpcbox Plugin
=====

A rebar3 plugin for generating a behaviour per grpc service, for use with [grpcbox](https://github.com/tsloughter/grpcbox).

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

{grpc, [{protos, "proto"},
        {gpb_opts, [{module_name_suffix, "_pb"}]}]}.

{plugins, [grpcbox_plugin]}.
```

Currently `grpcbox` and this plugin are a bit picky and the `gpb` options will always include `[use_packages, maps, type_specs, strings_as_binaries, {i, "."}, {o, "src"}]`.

Assuming the `proto` directory of your application has the `route_guide.proto` found in this repo, `test/grpcbox_SUITE_data/route_guide.proto`, the output from running the plugin will be:

```
$ rebar3 grpc gen
===> Writing src/route_guide_pb.erl
===> Writing src/grpcbox_route_guide_behaviour.erl
```
