-module(rest_api).

-behaviour(gen_server).

%% Setup.
-export ([set_auth_id/1, set_auth_token/1]).

%% Plivo Api
%% Account.
-export([get_account/1, modify_account/2]).

%% gen_server stuff
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (auth, {id="", token=""}).

%% Plivo Account info.
-define(API_BASE, "https://api.plivo.com/").
-define(API_URL, ?API_BASE ++ ?API_VERSION).
-define(API_VERSION, "v1/").
-define(AUTH_HEADER(Id, Token),
        {"Authorization",
         "Basic " ++ base64:encode_to_string(Id ++ ":" ++ Token)}).

%% @type json_string() = atom | string() | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = {array, [json_term()]}
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object()

-type payload()      :: {string(), headers()} |
                        {string(), headers(), content_type(), body()}.
-type headers()      :: [header()].
-type header()       :: {string(), string()}.
-type content_type() :: string().
-type body()         :: string().

-type json_string()  :: atom | string() | binary().
-type json_number()  :: integer() | float().
-type json_array()   :: {array, [json_term()]}.
-type json_object()  :: {struct, [{json_string(), json_term()}]}.
-type json_term()    :: json_string() | json_number() | json_array() |
                        json_object().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    inets:start(),
    ssl:start(),
    {ok, #auth{}}.

%% Gen_server callbacks.

handle_call({post, Url, Params}, _From, State=#auth{id=Id, token=Token}) ->
    % Plivo only accepts json with `POST`.
    Payload = {Url,
               [?AUTH_HEADER(Id, Token)],
               "application/json",
               "\"" ++ mochijson2:encode({struct,Params}) ++ "\""},
    Data = request(post, Payload),
    {reply, Data, State};
handle_call({get, Url}, _From, State=#auth{id=Id, token=Token}) ->
    Payload = {Url, [?AUTH_HEADER(Id, Token)]},
    Data = request(get, Payload),
    {reply, Data, State}.

handle_cast({auth_id, ID}, State)       -> {noreply, State#auth{id=ID}};
handle_cast({auth_token, Token}, State) -> {noreply, State#auth{token=Token}}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Helpers.

%% This is the entry point to the gen_server.
-spec api(atom(), Path::string()) -> json_term().
api(get, Path) -> gen_server:call(?MODULE, {get, ?API_URL ++ Path}).
api(post, Path, Params) ->
    gen_server:call(?MODULE, {post, ?API_URL ++ Path, Params}).

%% These are the only valid response codes from plivo right now.
parse_response({{_,  200,_R},_H,Body}) -> mochijson:decode(Body);
parse_response({{_,  201,_R},_H,Body}) -> mochijson:decode(Body);
parse_response({{_,  202,_R},_H,Body}) -> mochijson:decode(Body);
parse_response({{_,  204,_R},_H,Body}) -> Body;
parse_response({{_,  400,_R},_H,Body}) -> Body;
parse_response({{_,  401,_R},_H,Body}) -> Body;
parse_response({{_,  404,_R},_H,Body}) -> Body;
parse_response({{_,  405,_R},_H,Body}) -> Body;
parse_response({{_,  500,_R},_H,Body}) -> Body;
parse_response({{_,_Code,_R},_H,Body}) -> Body.

%% Thin wrapper around httpc:request.
-spec request(atom(), Payload::payload()) -> body() | json_term().
request(Method, Payload) ->
    {ok, Response} = httpc:request(Method, Payload, [], []),
    parse_response(Response).

%% Api.

%% Setup api.

%% @spec set_auth_id(ID::string()) -> ok
%% @doc Set the id for authentication.
%%      This must be set before any requests are made.
set_auth_id(ID)       -> gen_server:cast(?MODULE, {auth_id, ID}).
%% @spec set_auth_token(Token::string()) -> ok
%% @doc Set the token for authentication.
%%      This must be set before any requests are made.
set_auth_token(Token) -> gen_server:cast(?MODULE, {auth_token, Token}).

%% Plivo api.

%% @spec get_account(AccountID::string()) -> json_term()
%% @doc Returns the account information for the supplied AccountID.
-spec get_account(AccountID::string()) -> json_term().
get_account(AccountID) -> api(get, "Account/" ++ AccountID ++ "/").

%% @spec modify_account(AccountID::string(), Params::[{atom(),string()}]) ->
%%       json_term()
%% @doc Modifies an existing account.
%%      Optional Params are name, city and address.
%%      Params must be a list of key, val tuples.
%%      E.g.: [{name, "Wilson"}, {address, "Some island."}]
-spec modify_account(AccountID::string(), Params::[{atom(),string()}]) ->
      json_term().
modify_account(AccountID, Params) ->
    api(post, "Account/" ++ AccountID ++ "/", Params).
