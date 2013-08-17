-module(plivo_server).

-behaviour(gen_server).

-export([start_link/0, get_account/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (ACCOUNT_URI, "https://api.plivo.com/v1/Account/").

-define (AUTH_HEADER,
         {"Authorization",
          "Basic " ++ base64:encode_to_string(?AUTH_ID ++ ":" ++ ?AUTH_TOKEN)}).

-define (AUTH_ID, "").

-define (AUTH_TOKEN, "").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    inets:start(),
    ssl:start(),
    {ok, []}.

%% callbacks

handle_call({get, Payload}, _From, State) ->
    {ok, Response} = request(get, Payload),
    {{_Protocol, _StatusCode, _Reason}, _Headers, Body} = Response,
    {struct, Data} = mochijson:decode(Body),
    {reply, Data, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helpers.
request(Method, Payload) ->
    case Method of
        get -> httpc:request(get, Payload, [], [])
    end.

rest_api(get, Uri) ->
    gen_server:call(?MODULE, {get, {Uri, [?AUTH_HEADER]}}).

%% Api.
get_account(AuthID) -> rest_api(get, ?ACCOUNT_URI ++ AuthID).
