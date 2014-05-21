%% @author nitinkapoor
%% @doc @todo Add description to mongo_server.


-module(mongo_server).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/1, start_link/0, get_count/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% set SERVER to module name
-define(SERVER, ?MODULE).

%% Defines default port
-define(DEFAULT_PORT, 1055). 

%% Holds state of process
-record(state, {port, lsock, request_count = 0}). 

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link(Port) ->
 gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @spec start_link -> {ok, Pid}
%% @doc Calls 'start_link(Port)' using the default port.
start_link() ->
	start_link(?DEFAULT_PORT).

%% @spec get_count -> {ok, Count}
%% @doc Fetches the no of requests made to this server
get_count() ->
	gen_server:call(?SERVER, get_count).

%% @spec stop -> ok
%% @doc stop the server
stop() ->
	gen_server:cast(?SERVER, stop).


%% Inatilizes server
init([Port]) ->
	{ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
	{ok, #state{port = Port, lsock = LSock}, 0}.

%% Returns request count
handle_call(get_count, _From, State) ->
	{reply, {ok, State#state.request_count}, State}.

%% shuts down gen_server
handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
	do_rpc(Socket, RawData),
	RequestCount = State#state.request_count,
	{noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
	{ok, _Sock} = gen_tcp:accept(LSock),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

do_rpc(Socket, RawData) ->
	try
		{M, F, A} = split_out_mfa(RawData),
		Result = apply(M, F, A),
		gen_tcp:send(Socket, io_lib:fwrite("~p ~n", [Result]))
	catch
		_Class:Err ->
			gen_tcp:send(Socket, io_lib:fwrite("~p ~n", [Err]))
	end.

split_out_mfa(RawData) ->
	MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
	{match, [M, F, A]} = re:run(
						   			MFA, 
									"(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$", 
									[{capture, [1,2,3], list}, ungreedy]
							   ),
	{list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
	{ok, Toks, _Line} = erl_scan:string("["++ RawArgs ++"]. ", 1),
	{ok, Args} = erl_parse:parse_term(Toks),
  	Args.


%%===================== Compile and Run Application Instruction =====================
%% 1: Go to Application dir
%% 2: erlc -o ebin src/*.erl
%% 3: erl -pa ebin -name mongo
%% 4: application:start(mongo_notification_server).
%% 5: mongo_notification:start("mylist", test).

%% Go to ruby concole
%% r = Erlang::Node.rpc("occam","mongo_notification","start",["mylist", "test"])

%% spawn(mongo_apns, send_message, ["ab15d796e5e45a1136b877664e5347919fc2d05a27c71aa55b5ea4c21b1a3140", "test", "100", "1"]). 
%%===================================================================================   
			