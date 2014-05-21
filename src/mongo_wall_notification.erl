%% @author nitinkapoor
%% @doc @todo Add description to notification.

-module(mongo_wall_notification).

%% ====================================================================
%% API functions
%% ====================================================================

%% -record(preson, {fmc_id, mc_id, user_id, wall, debate, oh, poll, read, wall_notification}).

-export([start/4, start_link/4]).
-compile(export_all).

-define(SERVER, ?MODULE).

%% ====================================================================
%% Internal functions
%% ====================================================================

start(Key, Message, MsgType, SenderId) ->
    spawn(fun() -> init(Key, Message, MsgType, SenderId) end).

start_link(Key, Message, MsgType, SenderId) ->
	spawn(fun() -> init(Key, Message, MsgType, SenderId) end).

init(Key, Message, MsgType, SenderId) ->
	io:fwrite("--- i am in init--- ~n"),
	{ok, Connection} = eredis:start_link(),
	{ok, SubscribeUsers} = eredis:q(Connection, ["HGETALL" | [Key]]),
	spawn(?MODULE, loop, [SubscribeUsers, Message, MsgType, SenderId]).

loop([], Message, MsgType, SenderId) ->
	{ok, Message, MsgType, SenderId};
loop(SubscribeUsers, Message, MsgType, SenderId) ->
	try
		StringLength = string:len(binary_to_list(hd(SubscribeUsers))),
		if 
			StringLength > 50 ->
				SubscribeUserJson = mochijson2:decode(hd(SubscribeUsers)),
				SubscribeUser = jsonobj(SubscribeUserJson),
				UserId = SubscribeUser(<<"user_id">>),
				MongoCodeId = SubscribeUser(<<"mc_id">>),
				ID = bin_to_num(SenderId),
				if 	
					ID =/= UserId ->
						UserKey = "User:" ++ integer_to_list(UserId) ++ ":Info",
						WallKey = "User:" ++ integer_to_list(UserId) ++ ":Walls",
						{ok, Connection} = eredis:start_link(),
						{ok, UserKeyExist} = eredis:q(Connection, ["EXISTS" | [UserKey]]),
						{ok, WallKeyExist} = eredis:q(Connection, ["EXISTS" | [WallKey]]),
						UserInfoKeyExist = bin_to_num(UserKeyExist),
						UserWallKeyExist = bin_to_num(WallKeyExist),
						if 
							UserInfoKeyExist  =:= 1 ->
								{ok, WallBadge} = eredis:q(Connection, ["HGET" | [UserKey | ["walls"]]]),
								if 
									UserWallKeyExist =:= 1 ->
										CodeKey = "mc:" ++ integer_to_list(MongoCodeId),
										{ok, WallData} = eredis:q(Connection, ["HGET" | [WallKey | [CodeKey]]]),
										WallDataJson = mochijson2:decode(WallData),
										UserWallData = jsonobj(WallDataJson),
										FmcId = UserWallData(<<"fmc_id">>),
										McId = UserWallData(<<"mc_id">>),
										Wall = UserWallData(<<"wall">>),
										Debate = UserWallData(<<"debate">>),
										Oh = UserWallData(<<"oh">>),
										Poll = UserWallData(<<"poll">>),
										Read = UserWallData(<<"read">>),
										if 
											Read =:= 1 ->
												Payload = lists:flatten(mochijson:encode({struct, [
													            			{fmc_id,FmcId},
																			{mc_id,McId},
																			{wall,Wall},
																			{debate,Debate},
																			{oh,Oh},
																			{poll,Poll},
													                        {read,0}]})),
												eredis:q(Connection, ["HSET" | [WallKey | [CodeKey | [Payload]]]]),
												{ok, WallBadgeCount} = sum(WallBadge),
												eredis:q(Connection, ["HSET" | [UserKey | ["walls", WallBadgeCount]]]),
												spawn(fun() -> mongo_notification:push(UserKey, Message, MsgType, integer_to_binary(MongoCodeId)) end);
											Read =:= 0 ->
												spawn(fun() -> mongo_notification:push(UserKey, Message, MsgType, integer_to_binary(MongoCodeId)) end),
												ok
										end;
									UserWallKeyExist =:= 0 ->
										ok
								end;
						  UserInfoKeyExist =:= 0 ->
							ok
						end;	
					ID =:= UserId ->
						ok
				end;
			StringLength < 50 ->
				ok
		end,	
		loop(tl(SubscribeUsers), Message, MsgType, SenderId)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

jsonobj({struct,List}) ->
    fun({contains,Key}) ->
        lists:keymember(Key,1,List);
    ({raw,Key}) ->
        {_,Ret} = lists:keyfind(Key,1,List),Ret;
    (Key) ->
        {_,Ret} = lists:keyfind(Key,1,List),
        jsonobj(Ret)
    end;
jsonobj(List) when is_list(List) ->
    fun(len) ->
        length(List);
    (Index) ->
        jsonobj(lists:nth(Index,List))
    end;
jsonobj(Obj) -> Obj.


bin_to_num(Bin) ->
    N = binary_to_list(Bin),
	case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

sum(Wall) ->
	try
		WallBadge = list_to_integer(binary_to_list(Wall)),
		Badge = WallBadge + 1,
		{ok, Badge}
	catch
		Exception:Reason ->
			{caught, Exception, Reason}
	end.

type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;

type_of(_X)                     -> unknown.