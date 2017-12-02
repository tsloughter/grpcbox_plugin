%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service {{unmodified_service_name}}.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on {{datetime}} and should not be modified manually

-module({{service_name}}_behaviour).

{{#methods}}
%% @doc {{^input_stream}}{{^output_stream}}Unary RPC{{/output_stream}}{{/input_stream}}
-callback {{method}}(ctx:ctx(), {{#input_stream}}grpcbox_stream:t(){{/input_stream}}{{^input_stream}}{{pb_module}}:'{{input}}'(){{/input_stream}}) ->
    {{#output_stream}}ok{{/output_stream}}{{^output_stream}}{ok, {{pb_module}}:'{{output}}'()}{{/output_stream}} | {error, term()}.

{{/methods}}
