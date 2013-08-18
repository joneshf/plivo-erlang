-module(rest_api).

-behaviour(gen_server).

%% Plivo Api
-export([get_account/1, set_auth_id/1, set_auth_token/1]).

%% gen_server stuff
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (auth, {id="", token=""}).

%% Plivo Account info.
-define(ACCOUNT_URI, "https://api.plivo.com/v1/Account/").
-define(AUTH_HEADER(Id, Token),
        {"Authorization",
         "Basic " ++ base64:encode_to_string(Id ++ ":" ++ Token)}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    inets:start(),
    ssl:start(),
    {ok, #auth{}}.

%% Gen_server callbacks.

handle_call({Method, Uri}, _From, State=#auth{id=Id, token=Token}) ->
    Payload = {Uri, [?AUTH_HEADER(Id, Token)]},
    {ok, Response} = request(Method, Payload),
    Data = process_response(Response),
    {reply, Data, State}.

handle_cast({auth_id, ID}, State)       -> {noreply, State#auth{id=ID}};
handle_cast({auth_token, Token}, State) -> {noreply, State#auth{token=Token}}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Helpers.
parse_json(Json) ->
    {struct, Data} = mochijson:decode(Json),
    Data.

%% TODO: Handle individual responses.
process_response({{_,Code,_R},_H,Body}) when Code =< 199 -> Body;
process_response({{_,Code,_R},_H,Body}) when Code =< 299 -> parse_json(Body);
process_response({{_,Code,_R},_H,Body}) when Code =< 399 -> Body;
process_response({{_,Code,_R},_H,Body}) when Code =< 499 -> Body;
process_response({{_,Code,_R},_H,Body}) when Code =< 599 -> Body.

request(get, Payload) -> httpc:request(get, Payload, [], []).

api(get, Uri) -> gen_server:call(?MODULE, {get, Uri}).

%% Api.

%% Setup api.
set_auth_id(ID)       -> gen_server:cast(?MODULE, {auth_id, ID}).
set_auth_token(Token) -> gen_server:cast(?MODULE, {auth_token, Token}).

%% Plivo api.
get_account(AccountID) -> api(get, ?ACCOUNT_URI ++ AccountID).

