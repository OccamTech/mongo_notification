%% @author nitinkapoor
%% @doc @todo Add description to mongo_apns.


-module(mongo_apns).

-define(SERVER, ?MODULE).

-define(MOCK_APN_PORT, 2195).
-define(TIMEOUT, 1000).

%% ====================================================================
%% API functions
%% ====================================================================

-export([send_message/8, send_message/9]).
-compile(export_all).
-import(ssl, [connect/4]).

%% ====================================================================
%% Internal functions
%% ====================================================================

send_message(DdeviceToken, MsgType, ConnectBadge, ChatBadge, FreindshipBadge, WallBadge, UserId, NotificationCount) ->
	{ok, Badge} = sum(ConnectBadge, ChatBadge, FreindshipBadge, WallBadge),
	Payload = lists:flatten(mochijson:encode({struct, [
            			{aps,{struct, [{badge, Badge}]}},
						{user_id, list_to_integer(binary_to_list(UserId))},
						{type,binary_to_list(MsgType)},
						{connect, list_to_integer(ConnectBadge)},
						{chat, list_to_integer(ChatBadge)},
						{friendship, list_to_integer(FreindshipBadge)},
						{wall, list_to_integer(WallBadge)},
                        {notification_count, list_to_integer(NotificationCount)}]})),
	push(DdeviceToken, Payload).

send_message(DdeviceToken, Message, MsgType, ConnectBadge, ChatBadge, FreindshipBadge, WallBadge, UserId, NotificationCount) ->
	{ok, Badge} = sum(ConnectBadge, ChatBadge, FreindshipBadge, WallBadge),
	Payload = lists:flatten(mochijson:encode({struct, [
            			{aps,{struct, [{alert, binary_to_list(Message)}, {badge, Badge}, {sound, "1"}]}},
						{user_id, list_to_integer(binary_to_list(UserId))},
						{type,binary_to_list(MsgType)},
						{connect, list_to_integer(ConnectBadge)},
						{chat, list_to_integer(ChatBadge)},
						{friendship, list_to_integer(FreindshipBadge)},
						{wall, list_to_integer(WallBadge)},
                        {notification_count, list_to_integer(NotificationCount)}]})),
	io:fwrite("~n--------------------------------- ~n"),
	io:fwrite(ConnectBadge),
	io:fwrite("~n"),
	io:fwrite(ChatBadge),
	io:fwrite("~n"),
	io:fwrite(FreindshipBadge),
	io:fwrite("~n"),
	io:fwrite(WallBadge),
	io:fwrite("~n"),
	io:fwrite(Payload),
	io:fwrite("~n--------------------------------- ~n"),
	push(DdeviceToken, Payload).

send_push(Socket, DdeviceToken, Payload) ->
	io:fwrite("--- Server Started APN--- ~n"),
	BinToken = hexstr_to_bin(DdeviceToken),
    PayloadBin = list_to_binary(Payload),
    PayloadLength = size(PayloadBin),
	try
	  Packet = <<0:8,
			      32:16/big,
				  BinToken/binary,
			      PayloadLength:16/big,
			      PayloadBin/binary>>,
    	ssl:send(Socket, Packet),
		ssl:close(Socket)
	catch
		Exception:Reason -> {caught, Exception, Reason}
	end.

push(DdeviceToken, Payload) ->
	Address = "gateway.sandbox.push.apple.com",
  	Port = 2195,
  	Cert = "/Users/nitinkapoor/Android/workspace/mongo_erl/mongo_notification/priv/apns-cert.pem",
  	Key = "/Users/nitinkapoor/Android/workspace/mongo_erl/mongo_notification/priv/apns-key.pem",
  	Options = [{certfile, Cert}, {keyfile, Key}, {mode, binary}, {verify, verify_none}, {active, true}],
  	Timeout = 10000,
	try
		{ok, Socket} = ssl:connect(Address, Port, Options, Timeout),
		send_push(Socket, DdeviceToken, Payload)
  	catch
		
		Exception:Reason ->
			{
			 io:fwrite("--- Server Started APN---> 8 ~n"),
			 io:fwrite(Exception),
			 io:fwrite("~n"),
			 io:fwrite(Reason),
			 io:fwrite("~n"),
			 caught, Exception, Reason}
	end.


hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([$ |T], Acc) ->
    hexstr_to_bin(T, Acc);
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).

sum(Connect, Chat, Freindship, Wall) ->
	try
		ConnectBadge = list_to_integer(Connect),
		ChatBadge = list_to_integer(Chat),
		FreindshipBadge = list_to_integer(Freindship),
		WallBadge = list_to_integer(Wall),
		Badge = ConnectBadge + ChatBadge + FreindshipBadge + WallBadge,
		{ok, Badge}
	catch
		Exception:Reason ->
			{
			 io:fwrite("--- Server Started APN---> 80 ~n"),
			 io:fwrite(Exception),
			 io:fwrite("~n"),
			 io:fwrite(Reason),
			 io:fwrite("~n"),
			 caught, Exception, Reason}
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





%% send_message(DT, Message, MsgType, ConnectBadge, ChatBadge, FreindshipBadge, WallBadge, UserId, NotificationCount) ->
%% 	{ok, Badge} = sum(ConnectBadge, ChatBadge, FreindshipBadge, WallBadge),
%% 	io:fwrite("------------- Type --------- ~n"),
%% 	BadgeCount = integer_to_list(Badge),
%% 	Msg = binary_to_list(Message),
%% 	ConnectCount = binary_to_list(ConnectBadge),
%% 	ChatCount = binary_to_list(ChatBadge),
%% 	FreindshipCount = binary_to_list(FreindshipBadge),
%% 	WallCount = binary_to_list(WallBadge),
%% 	Id = binary_to_list(UserId),
%% 	Nc = binary_to_list(NotificationCount),
%% 	Payload = "{\"aps\":{\"alert\":\"" ++ Message ++ "\",\"badge\":" ++ BadgeCount ++ ",\"sound\":\"1\"}
%% 				\"connect\":" ++ ConnectCount ++ "\",\"chat\":" ++ ChatCount ++ ",\"friendship\":\"1\"}",
%% 	Payload = lists:flatten(mochijson:encode({struct, [
%%             			{aps,{struct, [{alert, Message}, {badge, Badge}, {sound, "1"}]}},
%% 						{user_id,UserId},
%% 						{type,MsgType},
%% 						{connect,ConnectBadge},
%% 						{chat,ChatBadge},
%% 						{friendship,FreindshipBadge},
%% 						{wall,WallBadge},
%%                         {notification_count,NotificationCount}]})),
%% 	io:fwrite("--- 7 --- ~n"),
%% %% 	io:fwrite(Payload).
%% 	push(DT, Payload).