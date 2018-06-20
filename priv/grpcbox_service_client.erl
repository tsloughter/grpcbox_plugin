%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service {{unmodified_service_name}}.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on {{datetime}} and should not be modified manually

-module({{service_name}}_client).

-compile([nowarn_export_all]).
-compile([export_all]).

-include("grpcbox.hrl").

-define(SERVICE, '{{unmodified_service_name}}').
-define(PROTO_MODULE, '{{pb_module}}').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output), #grpcbox_def{service=?SERVICE,
                                         marshal_fun=?MARSHAL_FUN(Input),
                                         unmarshal_fun=?UNMARSHAL_FUN(Output)}).

{{#methods}}
%% @doc {{^input_stream}}{{^output_stream}}Unary RPC{{/output_stream}}{{/input_stream}}
-spec {{method}}(ctx:t(){{^input_stream}}, {{pb_module}}:{{input}}(){{/input_stream}}) ->
    {{^output_stream}}{{^input_stream}}{ok, {{pb_module}}:{{output}}(), grpcbox:metadata()}{{/input_stream}}{{#input_stream}}{ok, grpcbox_stream:t()}{{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}{ok, grpcbox_stream:t()}{{/input_stream}}{{#input_stream}}{ok, grpclient:stream()}{{/input_stream}}{{/output_stream}} | grpcbox_stream:grpc_error_response().
{{method}}(Ctx{{^input_stream}}, Input{{/input_stream}}) ->
    {{method}}(Ctx{{^input_stream}}, Input{{/input_stream}}, #{}).

-spec {{method}}(ctx:t(){{^input_stream}}, {{pb_module}}:{{input}}(){{/input_stream}}, grpcbox_client:options()) ->
    {{^output_stream}}{{^input_stream}}{ok, {{pb_module}}:{{output}}(), grpcbox:metadata()}{{/input_stream}}{{#input_stream}}{ok, grpcbox_stream:t()}{{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}{ok, grpcbox_stream:t()}{{/input_stream}}{{#input_stream}}{ok, grpclient:stream()}{{/input_stream}}{{/output_stream}} | grpcbox_stream:grpc_error_response().
{{method}}(Ctx{{^input_stream}}, Input{{/input_stream}}, Options) ->
    {{^output_stream}}{{^input_stream}}grpcbox_client:unary(Ctx, <<"/{{unmodified_service_name}}/{{unmodified_method}}">>, Input, ?DEF({{input}}, {{output}}), Options){{/input_stream}}{{#input_stream}}grpcbox_client:stream(Ctx, <<"/{{unmodified_service_name}}/{{unmodified_method}}">>, ?DEF({{input}}, {{output}}), Options){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}grpcbox_client:stream(Ctx, <<"/{{unmodified_service_name}}/{{unmodified_method}}">>, Input, ?DEF({{input}}, {{output}}), Options){{/input_stream}}{{#input_stream}}grpcbox_client:stream(Ctx, <<"/{{unmodified_service_name}}/{{unmodified_method}}">>, ?DEF({{input}}, {{output}}), Options){{/input_stream}}{{/output_stream}}.

{{/methods}}
