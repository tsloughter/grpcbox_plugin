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
    Provider = providers:create(
                 [{name, ?PROVIDER},            % The 'user friendly' name of the task
                  {namespace, ?NAMESPACE},
                  {module, ?MODULE},            % The module implementation of the task
                  {bare, true},                 % The task can be run by the user, always true
                  {deps, ?DEPS},                % The list of dependencies
                  {example, "rebar3 grpc gen"}, % How to use the plugin
                  {opts, [{protos, $p, "protos", string, "directory of protos to build"},
                          {force, $f, "force", boolean, "overwrite already generated modules"},
                          {type, $t, "type", string, "generate 'client', 'server' or 'all'"}]},
                  {short_desc, "Generates behaviours for grpc services"},
                  {desc, "Generates behaviours for grpc services"}]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    {Options, _} = rebar_state:command_parsed_args(State),
    lists:foreach(fun(AppInfo) -> handle_app(AppInfo, Options, State) end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error({compile_errors, Errors}) ->
    [[io_lib:format("Error building ~s~n", [File]) |
      [io_lib:format("        ~p: ~s", [Line, M:format_error(E)])
       || {Line, M, E} <- Es]]
     || {File, Es} <- Errors];
format_error({gpb, File, Error}) ->
    io_lib:format("Error compiling proto file ~s ~s", [filename:basename(File),
                                                       gpb_compile:format_error(Error)]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

handle_app(AppInfo, Options, State) ->
    Opts = rebar_app_info:opts(AppInfo),
    BeamOutDir = rebar_app_info:ebin_dir(AppInfo),
    GrpcOpts = rebar_opts:get(Opts, grpc, []),
    GpbOpts = proplists:get_value(gpb_opts, GrpcOpts, []),
    BaseDir = rebar_app_info:dir(AppInfo),
    GrpcOptOutDir = proplists:get_value(out_dir, GrpcOpts, filename:join(BaseDir, "src")),
    GrpcOutDir = filename:join(BaseDir, GrpcOptOutDir),
    GpbOutDir = filename:join(BaseDir, proplists:get_value(o, GpbOpts, GrpcOptOutDir)),

    ProtosDirs = case proplists:get_all_values(protos, Options) of
                     [] ->
                         case proplists:get_value(protos, GrpcOpts, [filename:join("priv", "protos")]) of
                             [H | _] = Ds when is_list(H) ->
                                 Ds;
                             D ->
                                 [D]
                         end;
                     Ds ->
                         Ds
                 end,
    ProtoFiles = lists:append([filelib:wildcard(filename:join([BaseDir, D, "*.proto"])) || D <- ProtosDirs]),

    Type = case proplists:get_value(type, Options, undefined) of
               undefined ->
                   proplists:get_value(type, GrpcOpts, all);
               T when T =:= "all" orelse T =:= "client" ->
                   T
           end,
    Templates = templates(Type),
    ProtoModules = [compile_pb(Filename, GpbOutDir, BeamOutDir, GpbOpts) || Filename <- ProtoFiles],
    [gen_services(Templates, ProtoModule, ProtoBeam, GrpcOutDir, GrpcOpts, State)
     || {ProtoModule, ProtoBeam} <- ProtoModules],
    ok.

compile_pb(Filename, OutDir, BeamOutDir, GpbOpts) ->
    ModuleName = lists:flatten(
                   [proplists:get_value(module_name_prefix, GpbOpts, ""),
                    filename:basename(Filename, ".proto"),
                    proplists:get_value(module_name_suffix, GpbOpts, "")]),
    GeneratedPB = filename:join(OutDir, ModuleName ++ ".erl"),
    CompiledPB = filename:join(BeamOutDir, ModuleName ++ ".beam"),
    ok = filelib:ensure_dir(GeneratedPB),
    case needs_update(Filename, GeneratedPB) of
        true ->
            rebar_log:log(info, "Writing ~s", [GeneratedPB]),
            case gpb_compile:file(Filename, [{rename,{msg_name,snake_case}},
                                             {rename,{msg_fqname,base_name}},
                                             use_packages, maps,
                                             strings_as_binaries, {i, "."},
                                             {report_errors, false},
                                             {o, OutDir} | GpbOpts]) of
                ok ->
                    ok;
                {error, Error} ->
                    erlang:error(?PRV_ERROR({gpb, Filename, Error}))
            end;
        false ->
            ok
    end,
    case needs_update(GeneratedPB, CompiledPB) of
        true ->
            GpbIncludeDir = filename:join(code:lib_dir(gpb), "include"),
            case compile:file(GeneratedPB, [{outdir, BeamOutDir}, {i, GpbIncludeDir}, return_errors]) of
                {ok, _} ->
                    ok;
                {ok, _, Warnings} ->
                    log_warnings(Warnings),
                    ok;
                {error, Errors, Warnings} ->
                    log_warnings(Warnings),
                    throw(?PRV_ERROR({compile_errors, Errors}))
            end;
        false ->
            ok
    end,
    {module, Module} = code:load_abs(filename:join(BeamOutDir, ModuleName)),
    {Module, CompiledPB}.

gen_services(Templates, ProtoModule, ProtoBeam, OutDir, GrpcConfig, State) ->
    ServiceDefs = [gen_service_def(S, ProtoModule, GrpcConfig, OutDir)
                   || S <- ProtoModule:get_service_names()],
    WithTemplates = [{S, TemplateSuffix, TemplateName}
                     || S <- ServiceDefs, {TemplateSuffix, TemplateName} <- Templates],
    Services = lists:filter(fun(S) -> filter_outdated(S, OutDir, ProtoBeam) end, WithTemplates),
    rebar_log:log(debug, "services: ~p", [Services]),
    [rebar_templater:new(TemplateName, maps:to_list(Service), true, State)
     || {Service, _, TemplateName} <- Services].

gen_service_def(Service, ProtoModule, GrpcConfig, FullOutDir) ->
    ServiceModules = proplists:get_value(service_modules, GrpcConfig, []),
    ServicePrefix = proplists:get_value(prefix, GrpcConfig, ""),
    ServiceSuffix = proplists:get_value(suffix, GrpcConfig, ""),
    {{_, Name}, Methods} = ProtoModule:get_service_def(Service),
    ModuleName = proplists:get_value(Name, ServiceModules, list_snake_case(atom_to_list(Name))),
    #{out_dir => FullOutDir,
      pb_module => atom_to_list(ProtoModule),
      unmodified_service_name => atom_to_list(Name),
      module_name => ServicePrefix ++ ModuleName ++ ServiceSuffix,
      methods => [resolve_method(M, ProtoModule) || M <- Methods]}.

resolve_method(Method, ProtoModule) ->
    MessageType = {message_type, ProtoModule:msg_name_to_fqbin(maps:get(input, Method))},
    MethodData = lists:flatmap(fun normalize_method_opt/1, maps:to_list(Method)),
    [MessageType | MethodData].

filter_outdated({#{module_name := ModuleName}, TemplateSuffix, _}, OutDir, ProtoBeam) ->
    ModulePath = filename:join([OutDir, ModuleName ++ "_" ++ TemplateSuffix ++ ".erl"]),
    ok = filelib:ensure_dir(ModulePath),
    needs_update(ProtoBeam, ModulePath).

templates(S) when is_list(S) ->
    templates(list_to_existing_atom(S));
templates(all) ->
    [{"client", "grpcbox_service_client"},
     {"bhvr", "grpcbox_service_bhvr"}];
templates(client) ->
    [{"client", "grpcbox_service_client"}];
templates(server) ->
    [{"bhvr", "grpcbox_service_bhvr"}].

normalize_method_opt({opts, _}) ->
    [];
normalize_method_opt({name, Name}) ->
    StrName = atom_to_list(Name),
    [{method, list_snake_case(StrName)},
     {unmodified_method, StrName}];
normalize_method_opt({K, V}) when V =:= true; V =:= false ->
    [{K, V}];
normalize_method_opt({K, V}) ->
    [{K, atom_to_list(V)}].

list_snake_case(NameString) ->
    Snaked = lists:foldl(
               fun(RE, Snaking) ->
                       re:replace(Snaking, RE, "\\1_\\2", [{return, list}, global])
               end,
               NameString,
               [%% uppercase followed by lowercase
                "(.)([A-Z][a-z]+)",
                %% any consecutive digits
                "(.)([0-9]+)",
                %% uppercase with lowercase
                %% or digit before it
                "([a-z0-9])([A-Z])"]),
    Snaked1 = string:replace(Snaked, ".", "_", all),
    Snaked2 = string:replace(Snaked1, "__", "_", all),
    string:to_lower(unicode:characters_to_list(Snaked2)).

needs_update(Source, Artifact) ->
    filelib:last_modified(Source) >= filelib:last_modified(Artifact).

log_warnings(Warnings) ->
    [begin
         rebar_api:warn("Warning building ~s~n", [File]),
         [rebar_api:warn("        ~p: ~s", [Line, M:format_error(E)]) || {Line, M, E} <- Es]
     end || {File, Es} <- Warnings].
