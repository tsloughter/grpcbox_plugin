-module(grpcbox_plugin_prv).

-export([init/1, do/1, format_error/1]).

-include_lib("providers/include/providers.hrl").

-define(PROVIDER, gen).
-define(NAMESPACE, grpc).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, ?NAMESPACE},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 grpc gen"}, % How to use the plugin
            {opts, [{protos, $p, "protos", string, "directory of protos to build"},
                    {force, $f, "force", boolean, "overwrite already generated modules"},
                    {type, $t, "type", string, "generate 'client' or 'all' (server behaviour and client)"}]},
            {short_desc, "Generates behaviours for grpc services"},
            {desc, "Generates behaviours for grpc services"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Config = rebar_state:opts(State),
    GrpcConfig = rebar_opts:get(Config, grpc, []),
    {Options, _} = rebar_state:command_parsed_args(State),
    ProtosDirs = case proplists:get_all_values(protos, Options) of
                     [] ->
                         case proplists:get_value(protos, GrpcConfig, ["priv/protos"]) of
                             [H | _]=Ds when is_list(H) ->
                                 Ds;
                             D ->
                                 [D]
                         end;
                     Ds ->
                         Ds
                 end,
    GpbOpts = proplists:get_value(gpb_opts, GrpcConfig, []),
    GrpcOutDir = proplists:get_value(out_dir, GrpcConfig, "src"),
    Type = case proplists:get_value(type, Options, undefined) of
               undefined ->
                   proplists:get_value(type, GrpcConfig, all);
               T when T =:= "all" orelse T =:= "client" ->
                   T
           end,
    TemplateName = to_template_name(Type),
    ServiceModules = proplists:get_value(service_modules, GrpcConfig, []),
    [[begin
          GpbModule = compile_pb(Filename, GrpcOutDir, GpbOpts),
          gen_service_behaviour(TemplateName, ServiceModules, GpbModule, Options, GrpcConfig, State)
      end || Filename <- filelib:wildcard(filename:join(Dir, "*.proto"))]
     || Dir <- ProtosDirs],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error({compile_errors, Errors, Warnings}) ->
    [begin
         rebar_api:warn("Warning building ~s~n", [File]),
         [rebar_api:warn("        ~p: ~s", [Line, M:format_error(E)]) || {Line, M, E} <- Es]
     end || {File, Es} <- Warnings],
    [[io_lib:format("Error building ~s~n", [File]) |
         [io_lib:format("        ~p: ~s", [Line, M:format_error(E)]) || {Line, M, E} <- Es]] || {File, Es} <- Errors];
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

to_template_name(all) ->
    "grpcbox";
to_template_name(client) ->
    "grpcbox_client";
to_template_name("all") ->
    "grpcbox";
to_template_name("client") ->
    "grpcbox_client".

maybe_rename(name) ->
    method;
maybe_rename(N) ->
    N.

unmodified_maybe_rename(name) ->
    unmodified_method;
unmodified_maybe_rename(N) ->
    N.

gen_service_behaviour(TemplateName, ServiceModules, GpbModule, Options, GrpcConfig, State) ->
    OutDir = proplists:get_value(out_dir, GrpcConfig, "src"),
    Force = proplists:get_value(force, Options, true),
    ServicePrefix = proplists:get_value(prefix, GrpcConfig, ""),
    ServiceSuffix = proplists:get_value(suffix, GrpcConfig, ""),
    Services = [begin
                    {{_, Name}, Methods} = GpbModule:get_service_def(S),
                    ModuleName = proplists:get_value(Name, ServiceModules,
                                                     list_snake_case(atom_to_list(Name))),
                    [{out_dir, OutDir},
                     {pb_module, atom_to_list(GpbModule)},
                     {unmodified_service_name, atom_to_list(Name)},
                     {module_name, ServicePrefix++ModuleName++ServiceSuffix},
                     {methods, [lists:flatten([[[{maybe_rename(X), maybe_snake_case(X, atom_to_list(Y))},
                                                   {unmodified_maybe_rename(X), atom_to_list(Y)}]
                                               || {X, Y} <- maps:to_list(Method), X =/= opts],
                                              {message_type, GpbModule:msg_name_to_fqbin(maps:get(input, Method))}])
                                || Method <- Methods]}]
                end || S <- GpbModule:get_service_names()],
    rebar_log:log(debug, "services: ~p", [Services]),
    [rebar_templater:new(TemplateName, Service, Force, State) || Service <- Services].

compile_pb(Filename, GrpcOutDir, Options) ->
    OutDir = proplists:get_value(o, Options, GrpcOutDir),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, Options, ""),
    ModuleNamePrefix = proplists:get_value(module_name_prefix, Options, ""),
    CompiledPB =  filename:join(OutDir, ModuleNamePrefix++filename:basename(Filename, ".proto") ++ ModuleNameSuffix++".erl"),
    rebar_log:log(info, "Writing ~s", [CompiledPB]),
    ok = gpb_compile:file(Filename, [{rename,{msg_name,snake_case}},
                                     {rename,{msg_fqname,base_name}},
                                     use_packages, maps,
                                     strings_as_binaries, {i, "."}, {o, OutDir} | Options]),
    GpbInludeDir = filename:join(code:lib_dir(gpb), "include"),
    case compile:file(CompiledPB,
                      [binary, {i, GpbInludeDir}, return_errors]) of
        {ok, Module, Compiled} ->
            {module, _} = code:load_binary(Module, CompiledPB, Compiled),
            Module;
        {ok, Module, Compiled, Warnings} ->
            [begin
                 rebar_api:warn("Warning building ~s~n", [File]),
                 [rebar_api:warn("        ~p: ~s", [Line, M:format_error(E)]) || {Line, M, E} <- Es]
             end || {File, Es} <- Warnings],
            {module, _} = code:load_binary(Module, CompiledPB, Compiled),
            Module;
        {error, Errors, Warnings} ->
            throw(?PRV_ERROR({compile_errors, Errors, Warnings}))
    end.

maybe_snake_case(name, Name) ->
    list_snake_case(Name);
maybe_snake_case(_, "true") ->
    true;
maybe_snake_case(_, "false") ->
    false;
maybe_snake_case(_, Value) ->
    Value.

list_snake_case(NameString) ->
    Snaked = lists:foldl(fun(RE, Snaking) ->
                                 re:replace(Snaking, RE, "\\1_\\2", [{return, list},
                                                                     global])
                         end, NameString, [%% uppercase followed by lowercase
                                           "(.)([A-Z][a-z]+)",
                                           %% any consecutive digits
                                           "(.)([0-9]+)",
                                           %% uppercase with lowercase
                                           %% or digit before it
                                           "([a-z0-9])([A-Z])"]),
    Snaked1 = string:replace(Snaked, ".", "_", all),
    Snaked2 = string:replace(Snaked1, "__", "_", all),
    string:to_lower(unicode:characters_to_list(Snaked2)).
