%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service {{unmodified_service_name}}.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module({{module_name}}_bhvr).

{{#methods}}
%% {{^input_stream}}{{^output_stream}}Unary RPC{{/output_stream}}{{/input_stream}}
-callback {{method}}({{^input_stream}}{{#output_stream}}{{pb_module}}:{{input}}(), grpcbox_stream:t(){{/output_stream}}{{/input_stream}}{{#input_stream}}{{^output_stream}}reference(), grpcbox_stream:t(){{/output_stream}}{{#output_stream}}reference(), grpcbox_stream:t(){{/output_stream}}{{/input_stream}}{{^input_stream}}{{^output_stream}}ctx:t(), {{pb_module}}:{{input}}(){{/output_stream}}{{/input_stream}}) ->
    {{#output_stream}}ok{{/output_stream}}{{^output_stream}}{ok, {{pb_module}}:{{output}}(), ctx:t()}{{/output_stream}} | grpcbox_stream:grpc_error_response().

{{/methods}}
