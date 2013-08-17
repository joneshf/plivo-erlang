-module(plivo_server).

-behaviour(gen_server).

%% Plivo Api
-export([get_account/1]).

%% gen_server stuff
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Plivo Account info.
-define(ACCOUNT_URI, "https://api.plivo.com/v1/Account/").
-define(AUTH_HEADER,
        {"Authorization",
         "Basic " ++ base64:encode_to_string(?AUTH_ID ++ ":" ++ ?AUTH_TOKEN)}).
-define(AUTH_ID, "").
-define(AUTH_TOKEN, "").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    inets:start(),
    ssl:start(),
    {ok, []}.

%% Gen_server callbacks.

handle_call({Method, Payload}, _From, State) ->
    {ok, Response} = request(Method, Payload),
    Data = process_response(Response),
    {reply, Data, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Helpers.
parse_json(Json) ->
    {struct, Data} = mochijson:decode(Body),
    Data.

%% TODO: Handle individual responses.
process_response({{_,Code,_R},_H,Body}) when Code =< 199 -> Body;
process_response({{_,Code,_R},_H,Body}) when Code =< 299 -> parse_json(Body);
process_response({{_,Code,_R},_H,Body}) when Code =< 399 -> Body;
process_response({{_,Code,_R},_H,Body}) when Code =< 499 -> Body;
process_response({{_,Code,_R},_H,Body}) when Code =< 599 -> Body.

request(get, Payload) -> httpc:request(get, Payload, [], []).

rest_api(get, Uri) -> gen_server:call(?MODULE, {get, {Uri, [?AUTH_HEADER]}}).

%% Api.
get_account(AccountID) -> rest_api(get, ?ACCOUNT_URI ++ AccountID).
