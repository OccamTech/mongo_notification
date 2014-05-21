-module(mongo_notification_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case mongo_notification_sup:start_link() of
	{ok, Pid} ->
		{ok, Pid};
	other ->
		{error, other}
	end.

stop(_State) ->
    ok.
