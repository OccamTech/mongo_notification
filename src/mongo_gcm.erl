%% @author nitinkapoor
%% @doc @todo Add description to mongo_gcm.


-module(mongo_gcm).

%% ====================================================================
%% API functions
%% ====================================================================
-export([send_message/2, send_message/3, send_message/8, send_message/9]).
-compile(export_all).

-define(SERVER, ?MODULE).
-define(BASEURL, "https://android.googleapis.com/gcm/send").
-define(API_KEY, "key=AIzaSyB5H646SerppWDgGbY-oel1fnPK-OctqEo").

%% ====================================================================
%% Internal functions
%% ====================================================================

% send only a string
send_message(DdeviceToken, Message) ->
  	Payload = lists:flatten(mochijson:encode({struct, [{registration_ids,{array, [DdeviceToken]}},
            			{data,{struct, [{message_text, Message}]}},
                        {time_to_live,3600},
                        {collapse_key,delete_chat_history}]})),	
  	push(Payload).

% send a string and and a bagde number
send_message(DdeviceToken, Message, Badge) ->
   	Payload = lists:flatten(mochijson:encode({struct, [{registration_ids,{array, [DdeviceToken]}},
                  		{data,{struct, [{message_text, Message},{badge, Badge}]}},
                        {time_to_live,3600},
                        {collapse_key,delete_chat_history}]})),	
  	push(Payload).

send_message(DdeviceToken, MsgType, ConnectBadge, ChatBadge, FreindshipBadge, WallBadge, UserId, NotificationCount) ->
	{ok, Badge} = sum(ConnectBadge, ChatBadge, FreindshipBadge, WallBadge),
	Payload = lists:flatten(mochijson:encode({struct, [{registration_ids,{array, [DdeviceToken]}},
            			{data,{struct, [{badge, Badge},
										{user_id, list_to_integer(UserId)},
										{connect, list_to_integer(ConnectBadge)},
										{chat, list_to_integer(ChatBadge)},
										{friendship, list_to_integer(FreindshipBadge)},
										{wall, list_to_integer(WallBadge)},
										{notification_count, list_to_integer(NotificationCount)}			   
									   ]}},
						{collapse_key,MsgType}
						]})),
	push(Payload).

send_message(DdeviceToken, Message, MsgType, ConnectBadge, ChatBadge, FreindshipBadge, WallBadge, UserId, NotificationCount) ->
	{ok, Badge} = sum(ConnectBadge, ChatBadge, FreindshipBadge, WallBadge),
	Payload = lists:flatten(mochijson:encode({struct, [{registration_ids,{array, [DdeviceToken]}},
            			{data,{struct, [{message_text, Message},
										{badge, Badge},
										{user_id, list_to_integer(binary_to_list(UserId))},
										{connect, list_to_integer(ConnectBadge)},
										{chat, list_to_integer(ChatBadge)},
										{friendship, list_to_integer(FreindshipBadge)},
										{wall, list_to_integer(WallBadge)},
										{notification_count, list_to_integer(NotificationCount)}			   
									   ]}},
						{collapse_key,MsgType}
						]})),
	push(Payload).

push(Payload) ->
	try httpc:request(post, {?BASEURL, [{"Authorization", ?API_KEY}], "application/json", Payload}, [], []) of
		{ok, {{_, 200, _}, Headers, GCMResponse}} ->
	    	{ok, mochijson:decode(GCMResponse), Headers};
	    {error, Reason} ->
	        %% Some general error during the request.
	        io:fwrite("error in request: ~p~n", [Reason]),
	        {error, Reason};
	    {ok, {{_, 400, _}, _, _}} ->
			%% Some error in the Json.
	        {error, json_error};
	   	{ok, {{_, 401, _}, _, _}} ->
	    	%% Some error in the authorization.
			io:fwrite("authorization error! ~n", []),
	        {error, auth_error};
	    {ok, {{_, Code, _}, _, _}} when Code >= 500 andalso Code =< 599 ->
	    	%% TODO: retry with exponential back-off
	        {error, retry};
	    {ok, {{_StatusLine, _, _}, _, _Body}} ->
	    	%% Request handled but some error like timeout happened.
	        {error, timeout};
	    OtherError ->
	    	 %% Some other nasty error.
	        io:fwrite("other error: ~p~n", [OtherError]),
	        {noreply, unknown}
	catch
		Exception ->
	    	{error, Exception}
    end.


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