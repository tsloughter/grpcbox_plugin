%%%-------------------------------------------------------------------
%% @doc Client module for grpc service {{unmodified_service_name}}.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module({{module_name}}_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, '{{unmodified_service_name}}').
-define(PROTO_MODULE, '{{pb_module}}').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

{{#methods}}
%% @doc {{^input_stream}}{{^output_stream}}Unary RPC{{/output_stream}}{{/input_stream}}
-spec {{method}}({{^input_stream}}{{pb_module}}:{{input}}(){{/input_stream}}) ->
    {{^output_stream}}{{^input_stream}}{ok, {{pb_module}}:{{output}}(), grpcbox:metadata()}{{/input_stream}}{{#input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{#input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{/output_stream}} | grpcbox_stream:grpc_error_response() | {error, any()}.
{{method}}({{^input_stream}}Input{{/input_stream}}) ->
    {{method}}(ctx:new(){{^input_stream}}, Input{{/input_stream}}, #{}).

-spec {{method}}(ctx:t(){{^input_stream}} | {{pb_module}}:{{input}}(){{/input_stream}}{{^input_stream}}, {{pb_module}}:{{input}}(){{/input_stream}} | grpcbox_client:options()) ->
    {{^output_stream}}{{^input_stream}}{ok, {{pb_module}}:{{output}}(), grpcbox:metadata()}{{/input_stream}}{{#input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{#input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{/output_stream}} | grpcbox_stream:grpc_error_response() | {error, any()}.
{{method}}(Ctx{{^input_stream}}, Input{{/input_stream}}) when ?is_ctx(Ctx) ->
    {{method}}(Ctx{{^input_stream}}, Input{{/input_stream}}, #{});
{{method}}({{^input_stream}}Input, {{/input_stream}}Options) ->
    {{method}}(ctx:new(){{^input_stream}}, Input{{/input_stream}}, Options).

-spec {{method}}(ctx:t(){{^input_stream}}, {{pb_module}}:{{input}}(){{/input_stream}}, grpcbox_client:options()) ->
    {{^output_stream}}{{^input_stream}}{ok, {{pb_module}}:{{output}}(), grpcbox:metadata()}{{/input_stream}}{{#input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{#input_stream}}{ok, grpcbox_client:stream()}{{/input_stream}}{{/output_stream}} | grpcbox_stream:grpc_error_response() | {error, any()}.
{{method}}(Ctx{{^input_stream}}, Input{{/input_stream}}, Options) ->
    {{^output_stream}}{{^input_stream}}grpcbox_client:unary(Ctx, <<"/{{unmodified_service_name}}/{{unmodified_method}}">>, Input, ?DEF({{input}}, {{output}}, <<"{{message_type}}">>), Options){{/input_stream}}{{#input_stream}}grpcbox_client:stream(Ctx, <<"/{{unmodified_service_name}}/{{unmodified_method}}">>, ?DEF({{input}}, {{output}}, <<"{{message_type}}">>), Options){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}grpcbox_client:stream(Ctx, <<"/{{unmodified_service_name}}/{{unmodified_method}}">>, Input, ?DEF({{input}}, {{output}}, <<"{{message_type}}">>), Options){{/input_stream}}{{#input_stream}}grpcbox_client:stream(Ctx, <<"/{{unmodified_service_name}}/{{unmodified_method}}">>, ?DEF({{input}}, {{output}}, <<"{{message_type}}">>), Options){{/input_stream}}{{/output_stream}}.

{{/methods}}
