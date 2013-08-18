-module(rest_api).

-behaviour(gen_server).

%% Setup.
-export ([set_auth_id/1, set_auth_token/1]).

%% Plivo Api
%% Account.
-export([create_subaccount/1, delete_subaccount/1, get_account/0,
         get_all_subaccounts/0, get_all_subaccounts/1, get_subaccount/1,
         modify_account/1, modify_subaccount/2]).

%% gen_server stuff
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (auth, {id="", token=""}).

%% Plivo Account info.
-define(API_ACCOUNT, "Account/").
-define(API_BASE, "https://api.plivo.com/").
-define(API_URL, ?API_BASE ++ ?API_VERSION ++ ?API_ACCOUNT).
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

-type params()       :: [param()].
-type param()        :: {atom(), string()}.

-type payload()      :: {string(), headers()} |
                        {string(), headers(), content_type(), body()}.
-type headers()      :: [header()].
-type header()       :: {string(), string()}.
-type content_type() :: string().
-type body()         :: string().

-type status_line()  :: {protocol(), status_code(), reason()}.
-type protocol()     :: string().
-type status_code()  :: integer().
-type reason()       :: string().

-type json_string()  :: atom |
                        string() |
                        binary().
-type json_number()  :: integer() |
                        float().
-type json_array()   :: {array, [json_term()]}.
-type json_object()  :: {struct, [{json_string(), json_term()}]}.
-type json_term()    :: json_string() |
                        json_number() |
                        json_array() |
                        json_object().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    inets:start(),
    ssl:start(),
    {ok, #auth{}}.

%% Gen_server callbacks.

handle_call({post, Path, Params}, _From, State=#auth{id=Id, token=Token}) ->
    % Plivo only accepts json with `POST`.
    Url = ?API_URL ++ Id ++ Path,
    Body = binary_to_list(jsx:encode(Params)),
    Payload = {Url, [?AUTH_HEADER(Id, Token)], "application/json", Body},
    Data = request(post, Payload),
    {reply, Data, State};
handle_call({Method, Path}, _From, State=#auth{id=Id, token=Token}) ->
    Url = ?API_URL ++ Id ++ "/" ++ Path,
    Payload = {Url, [?AUTH_HEADER(Id, Token)]},
    Data = request(Method, Payload),
    {reply, Data, State}.

handle_cast({auth_id,       Id}, State) -> {noreply, State#auth{id=Id}};
handle_cast({auth_token, Token}, State) -> {noreply, State#auth{token=Token}}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Helpers.

%% This is the entry point to the gen_server.
-spec api(atom(), Path::string()) -> json_term().
api(get, Path)    -> gen_server:call(?MODULE, {get,    Path});
api(delete, Path) -> gen_server:call(?MODULE, {delete, Path}).
-spec api(atom(), Path::string(), Params::params()) -> json_term().
api(get, Path, Params) ->
    gen_server:call(?MODULE, {get, Path ++ "?" ++ generate_query(Params)});
api(post, Path, Params) ->
    gen_server:call(?MODULE, {post, Path, Params}).

%% Take [{key, value}] list and create a query string.
-spec generate_query([{Key::string(), Value::string()}]) -> string().
generate_query([]) -> "";
generate_query([{K,V}|P]) -> K ++ "=" ++ V ++ generate_query(P).

%% These are the only valid response codes from plivo right now.
-spec parse_response({status_line(),headers(),body()}) -> json_term().
parse_response({{_,  200,_R},_H,Body}) -> mochijson:decode(Body);
parse_response({{_,  201,_R},_H,Body}) -> mochijson:decode(Body);
parse_response({{_,  202,_R},_H,Body}) -> mochijson:decode(Body);
parse_response({{_,  204,_R},_H,Body}) -> Body;
parse_response({{_,  400,_R},_H,Body}) -> Body;
parse_response({{_,  401,_R},_H,Body}) -> Body;
parse_response({{_,  404,_R},_H,Body}) -> Body;
parse_response({{_,  405,_R},_H,Body}) -> Body;
parse_response({{_,  500,_R},_H,Body}) -> Body;
parse_response({ _StatusLine,_H,Body}) -> Body.

%% Thin wrapper around httpc:request.
-spec request(atom(), Payload::payload()) -> json_term().
request(Method, Payload) ->
    {ok, Response} = httpc:request(Method, Payload, [], []),
    parse_response(Response).

%% Api.

%% Setup api.

%% @spec set_auth_id(Id::string()) -> ok
%% @doc Set the id for authentication.
%%      This must be set before any requests are made.
set_auth_id(Id)       -> gen_server:cast(?MODULE, {auth_id, Id}).
%% @spec set_auth_token(Token::string()) -> ok
%% @doc Set the token for authentication.
%%      This must be set before any requests are made.
set_auth_token(Token) -> gen_server:cast(?MODULE, {auth_token, Token}).

%% Plivo api.

%% Account.

%% @spec create_subaccount(Params::params()) -> json_term()
%% @doc Creates a new subaccount and returns a response.
%%      Requires two params name and enabled.
%%      name is the name of the subaccount.
%%      enabled specifies whether a subaccount should be enabled.
-spec create_subaccount(Params::params()) -> json_term().
create_subaccount(Params) ->
    api(post, "Subaccount/", Params).

%% @spec delete_subaccount(SId::string()) -> json_term()
%% @doc Removes the subaccount from the specified account.
-spec delete_subaccount(SId::string()) -> json_term().
delete_subaccount(SId) ->
    api(delete, "Subaccount/" ++ SId ++ "/").

%% @spec get_account() -> json_term()
%% @doc Returns the account information for the supplied AId.
-spec get_account() -> json_term().
get_account() -> api(get, "/").

%% @spec get_all_subaccounts() -> json_term()
%% @doc Returns the subaccounts information for the supplied AId.
-spec get_all_subaccounts() -> json_term().
get_all_subaccounts() -> get_all_subaccounts([]).

%% @spec get_all_subaccounts(Params::params()) -> json_term()
%% @doc Returns the subaccounts information for the supplied AId.
%%      Optional params are: limit, offset.
%%      limit is the maximum number of results returned.  Max 20.
%%      offset is the subaccount start number.  Zero based.
%%      That is, if you want accounts 23-29, you would pass in params
%%      [{limit, 7}, {offset, 22}]
-spec get_all_subaccounts(Params::params()) -> json_term().
get_all_subaccounts(Params) ->
    api(get, "Subaccount/", Params).

%% @spec get_subaccount(SId::string()) -> json_term()
%% @doc Returns the subaccount information for the supplied SId combo.
-spec get_subaccount(SId::string()) -> json_term().
get_subaccount(SId) ->
    api(get, "Subaccount/" ++ SId ++ "/").

%% @spec modify_account(Params::params()) -> json_term()
%% @doc Modifies an existing account.
%%      Optional Params are name, city and address.
%%      Params must be a list of key, val tuples.
%%      E.g.: [{name, "Wilson"}, {address, "Some island."}]
-spec modify_account(Params::params()) -> json_term().
modify_account(Params) -> api(post, "/", Params).

%% @spec modify_subaccount(SId::string(), Params::params()) ->
%%       json_term()
%% @doc Modifies an existing Subaccount.
%%      Requires two params name and enabled.
%%      name is the name of the subaccount.
%%      enabled specifies whether a subaccount should be enabled.
-spec modify_subaccount(SId::string(), Params::params()) ->
      json_term().
modify_subaccount(SId, Params) ->
    api(post, "Subaccount/" ++ SId ++ "/", Params).
