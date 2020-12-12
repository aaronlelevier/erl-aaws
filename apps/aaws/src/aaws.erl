%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 28. Nov 2020 8:46 AM
%%%-------------------------------------------------------------------
-module(aaws).
-author("Aaron Lelevier").
-vsn(1.0).
-compile(export_all).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("xmerl/include/xmerl.hrl").

output_error(Resp) ->
    {ErrorType, Error} = Resp,
    io:format("ErrorType:~p Error:~p~n", [ErrorType, element(4, Error)]).

new_aws_config() ->
    AccessKeyID = os:getenv("AWS_ACCESS_KEY_ID"),
    SecretAccessKey = os:getenv("AWS_SECRET_ACCESS_KEY"),
    erlcloud_config:new(AccessKeyID, SecretAccessKey).

list_roles() ->
    Conf = assume_role(),
    {ok, Roles} = erlcloud_iam:list_roles(Conf),
    Roles.

%% TODO: Role to assume is hardcoded
assume_role() ->
    Conf0 = new_aws_config(),
    RoleArn = os:getenv("AWS_IAM_READ_ONLY_ROLE_ARN"),
    {Conf, _Creds} = erlcloud_sts:assume_role(Conf0, RoleArn, "IamReadOnly2", 3600),
    Conf.

%% @doc converts the 'Role' keys back to the AWS standard keys
convert(Role) ->
    convert(Role, erlcloud_iam:data_type("Role"), []).

convert([], _DataType, Acc) ->
    Acc;
convert([H | T], DataTypes, Acc) ->
    {Key, Val} = H,
    DataType = lists:keyfind(Key, 2, DataTypes),
    % must be a binary for 'jsx' to be json encodable
    NewKey = list_to_binary(element(1, DataType)),
    % convert Value for 'jsx' to be json encodable
    NewVal = convert_value(Val, element(3, DataType)),
    convert(T, DataTypes, [{NewKey, NewVal} | Acc]).

convert_value(Val, "String") ->
    list_to_binary(Val);
convert_value(Val, "DateTime") ->
    %% Returns string value in format: 2018-09-15T15:53:00Z
    {{Year, Month, Day}, {Hour, Minute, Second}} = Val,
    Str = str_format(
        "~p-~p-~pT~p:~p:~pZ",
        [Year, Month, Day, Hour, Minute, Second]
    ),
    list_to_binary(Str);
convert_value(Val, "Uri") ->
    list_to_binary(Val).

-spec str_format(String :: string(), Args :: list()) -> string().
str_format(String, Args) ->
    lists:flatten(io_lib:format(String, Args)).

json_encode_decode_example() ->
    file:write_file("files/role_id.json", jsx:encode([{<<"RoleId">>, <<"AROAJHLBK5RFOM1234">>}])),
    {ok, Binary} = file:read_file("files/role_id.json"),
    jsx:decode(Binary).

%% @doc working JSON dump to file for a single 'Role'
json_dump_role() ->
    Roles = list_roles(),
    [Role | _] = Roles,
    Ret3 = convert(Role),
    file:write_file("files/role_id.json", jsx:format(jsx:encode(Ret3), [{space, 1}, {indent, 2}])).

json_dump_role(Role) ->
    RoleName = proplists:get_value(role_name, Role),
    Filename = fmt("files/~s.json", [RoleName]),
    file:write_file(Filename, json_format(convert(Role))).

json_format(Data) ->
    jsx:format(jsx:encode(Data), [{space, 1}, {indent, 2}]).

%%------------------------------------------------------------------------------
%% Xml Response helpers
%%------------------------------------------------------------------------------
list_fields(Resp) ->
    [H | _] = Resp,
    XmlItems = element(9, H),
    [X#xmlElement.name || X <- XmlItems, is_record(X, xmlElement)].

%% @doc Performs string interpolation
-spec fmt(string(), [any()]) -> string().
fmt(String, Args) ->
    lists:flatten(io_lib:format(String, Args)).
