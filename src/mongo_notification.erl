%% @author nitinkapoor
%% @doc @todo Add description to notification.

-module(mongo_notification).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/4, start_link/4, push/4]).
-compile(export_all).

%% -record(my_record, {friend_notification, in_bg, is_online, chat, walls, chat_notification, connect, id, notification_count, device_token, connect_notification, in_bg_from, display_name, wall_notification, friend}).

-define(SERVER, ?MODULE).

%% ====================================================================
%% Internal functions
%% ====================================================================

start(Key, Message, MsgType, SenderId) ->
    spawn(fun() -> push(Key, Message, MsgType, SenderId) end).

start_link(Key, Message, MsgType, SenderId) ->
	spawn(fun() -> push(Key, Message, MsgType, SenderId) end).

push(UserKey, Message, MsgType, SenderId) ->
	io:fwrite("~n --- i am in push 4 --- ~n"),
	io:fwrite(SenderId),
	io:fwrite("~n "),
	{ok, Connection} = eredis:start_link(),
	{ok, KeyExist} = eredis:q(Connection, ["EXISTS" | [UserKey]]),
	CanExist = bin_to_num(KeyExist),
	try
		if 
			CanExist =:= 1 ->
				io:fwrite("~n -------------------->1 ~n"),
				{ok, UserInfoRedis} = eredis:q(Connection, ["HGETALL" | [UserKey]]),
				UserInfo = to_proplist(UserInfoRedis),
				DeviceToken = proplists:get_value("device_token", UserInfo),
				Connect = proplists:get_value("connect", UserInfo),
				Chat = proplists:get_value("chat", UserInfo),
				Freindship = proplists:get_value("friend", UserInfo),
				Wall = proplists:get_value("walls", UserInfo),
				NotificationCount = proplists:get_value("notification_count", UserInfo),
				FriendNotification = proplists:get_value("friend_notification", UserInfo),
				ChatNotification = proplists:get_value("chat_notification", UserInfo),
				ConnectNotification = proplists:get_value("connect_notification", UserInfo),
				WallNotification = proplists:get_value("wall_notification", UserInfo),
				NotificationSeting = notification_seting(MsgType, FriendNotification, ChatNotification, ConnectNotification, WallNotification),
				DeviceTokenLength = string:len(string:strip(DeviceToken)),
%% 				io:fwrite("~n --------------------> 1.1 ~n"),
%% 				io:fwrite("~n ~p ~n", [DeviceTokenLength]),
%% 				io:fwrite("~n ~p ~n", [type_of(DeviceTokenLength)]),
%% 				io:fwrite("~n ~p ~n", [WallNotification]),
%% 				io:fwrite("~n ~p ~n", [type_of(WallNotification)]),
%% 				io:fwrite("~n ~p ~n", [T]),
%% 				io:fwrite("~n ~p ~n", [type_of(T)]),
				if 
					DeviceTokenLength > 64 ->
						io:fwrite("~n -------------------->1.2 ~n"),
						if 
							NotificationSeting =:= "false" ->
								io:fwrite("~n -------------------->3 ~n"),
								spawn(mongo_gcm, send_message, [DeviceToken, MsgType, Connect, Chat, Freindship, Wall, SenderId, NotificationCount]);
							NotificationSeting =:= "true" ->
								io:fwrite("~n -------------------->4 ~n"),
								spawn(mongo_gcm, send_message, [DeviceToken, Message, MsgType, Connect, Chat, Freindship, Wall, SenderId, NotificationCount])
						end;
					DeviceTokenLength =:= 64 ->
						if 
							NotificationSeting =:= "false" ->
								io:fwrite("~n -------------------->5 ~n"),
								spawn(mongo_apns, send_message, [DeviceToken, MsgType, Connect, Chat, Freindship, Wall, SenderId, NotificationCount]);
							NotificationSeting =:= "true" ->
								io:fwrite("~n -------------------->6 ~n"),
								spawn(mongo_apns, send_message, [DeviceToken, Message, MsgType, Connect, Chat, Freindship, Wall, SenderId, NotificationCount])
						end;
				    DeviceTokenLength =:= 0 ->
						ok
				end;	
			CanExist =:= 0 ->
				ok
		end
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.


notification_seting(MsgType, FriendNotification, ChatNotification, ConnectNotification, WallNotification) ->
	io:fwrite("~n --------------------> 1.1 ~n"),
	io:fwrite("~n ~p ~n", [MsgType]),
	io:fwrite("~n ~p ~n", [type_of(MsgType)]),
	io:fwrite("~n ~p ~n", [FriendNotification]),
	io:fwrite("~n ~p ~n", [type_of(FriendNotification)]),
	MessageType = binary_to_list(MsgType),
	if
		MessageType =:= "friendship_received" ->
		  FriendNotification;
		MessageType =:= "friendship_accepted" ->
		  FriendNotification;
		MessageType =:= "friendship_rejected" ->
		  FriendNotification;
		MessageType =:= "connect_received" ->
		  ConnectNotification;
		MessageType =:= "connect_accepted" ->
		  ConnectNotification;
		MessageType =:= "connect_rejected" ->
		  ConnectNotification; 
		MessageType =:= "blocked" ->
		  FriendNotification; 
		MessageType =:= "blocked" ->
		  FriendNotification;
		MessageType =:= "unblocked" ->
		  FriendNotification;
		MessageType =:= "delete_chat_history" ->
		  FriendNotification;
		MessageType =:= "wall_notification" ->
		  WallNotification;
		MessageType =:= "pending_chats" ->
		  ChatNotification;
		true ->
		  ok
	end.
  
bin_to_num(Bin) ->
	N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

sum(Connect, Chat, Friends, Walls) ->
  try
  	Connect + Chat + Friends + Walls
  catch
	Exception:Reason -> {caught, Exception, Reason}
  end.


to_proplist([Key,Val|Rest]) -> 
     [{binary_to_list(Key),binary_to_list(Val)} | to_proplist(Rest)]; 
to_proplist([]) -> 
     []. 


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
