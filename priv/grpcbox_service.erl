%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service {{unmodified_service_name}}.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on {{datetime}} and should not be modified manually

-module({{service_name}}_bhvr).

{{#methods}}
%% @doc {{^input_stream}}{{^output_stream}}Unary RPC{{/output_stream}}{{/input_stream}}
-callback {{method}}({{^input_stream}}{{#output_stream}}{{pb_module}}:'{{input}}'(), grpcbox_stream:t(){{/output_stream}}{{/input_stream}}{{#input_stream}}{{^output_stream}}reference(), grpcbox_stream:t(){{/output_stream}}{{#output_stream}}reference(), grpcbox_stream:t(){{/output_stream}}{{/input_stream}}{{^input_stream}}{{^output_stream}}ctx:ctx(), {{pb_module}}:'{{input}}'(){{/output_stream}}{{/input_stream}}) ->
    {{#output_stream}}ok{{/output_stream}}{{^output_stream}}{ok, {{pb_module}}:'{{output}}'()}{{/output_stream}} | {error, term()}.

{{/methods}}
