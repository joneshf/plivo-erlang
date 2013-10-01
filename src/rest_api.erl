-module(rest_api).

-behaviour(gen_server).

%% Types.
-export_type([params/0]).
%% Rest Api.
-export([api/2, api/3]).
%% Auth.
-export([set_auth_id/1, set_auth_token/1]).

%% Plivo Api.

%% Account.
-export([create_subaccount/1, delete_subaccount/1, get_account/0,
         get_subaccounts/0, get_subaccounts/1, get_subaccount/1,
         modify_account/1, modify_subaccount/2]).

%% Application.
-export([create_application/1, delete_application/1, get_application/1,
         get_applications/0, get_applications/1, modify_application/2]).

%% Call.
-export([get_cdr/1, get_cdr/2, get_cdrs/0, get_cdrs/1, get_live_call/1,
         get_live_calls/0, hangup_call/1, hangup_request/1, make_call/1, play/2,
         record/1, record/2, send_digits/2, speak/2, stop_play/1, stop_record/1,
         stop_record/2, stop_speak/1, transfer_call/1, transfer_call/2]).

%% Conference.
-export([get_live_conferences/0, get_live_conference/1,
         hangup_live_conferences/0, hangup_live_conference/1, hangup_member/2,
         kick_member/2, mute_member/2, unmute_member/2, play_member/3,
         unplay_member/2, speak_member/3, deaf_member/2, undeaf_member/2,
         record_conference/1, stop_record_conference/1]).

%% Endpoint.
-export([create_endpoint/1, delete_endpoint/1, get_endpoint/1, get_endpoints/0,
         modify_endpoint/2]).

%% Message.
-export([get_message/1, get_messages/0, get_messages/1, send_message/1]).

%% Pricing.
-export([get_pricing/1]).

%% Recording.
-export([get_recording/1, get_recordings/1]).

%% gen_server stuff
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(auth, {id="", token=""}).

%% Plivo Account info.
-define(API_ACCOUNT, "Account/").
-define(API_BASE, "https://api.plivo.com/").
-define(API_URL, ?API_BASE ++ ?API_VERSION ++ ?API_ACCOUNT).
-define(API_VERSION, "v1/").
-define(AUTH_HEADER(Id, Token),
        {"Authorization",
         "Basic " ++ base64:encode_to_string(Id ++ ":" ++ Token)}).

-type params()       :: [param()].
-type param()        :: {atom(), binary()}.

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

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    inets:start(),
    ssl:start(),
    {ok, #auth{}}.

%% Gen_server callbacks.

handle_call({post, Path, Params}, _From, State=#auth{id=Id, token=Token}) ->
    % Plivo only accepts json with `POST`.
    Url = ?API_URL ++ Id ++ "/" ++ Path,
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
-spec api(atom(), Path::string()) -> jsx:json_term().
api(get, Path)    -> gen_server:call(?MODULE, {get,    Path});
api(delete, Path) -> gen_server:call(?MODULE, {delete, Path}).

-spec api(atom(), Path::string(), Params::params()) -> jsx:json_term().
api(get, Path, Params) ->
    Query = http_uri:encode(lists:flatten(generate_query(Params))),
    gen_server:call(?MODULE, {get, Path ++ "?" ++ Query});
api(delete, Path, Params) ->
    Query = http_uri:encode(lists:flatten(generate_query(Params))),
    gen_server:call(?MODULE, {delete, Path ++ "?" ++ Query});
api(post, Path, Params) ->
    gen_server:call(?MODULE, {post, Path, Params}).

%% Basically slap a `=` between the pair.
%% Handles types better.
-spec predify({K::atom(), V::jsx:json_term()}) -> string().
predify({K,V}) -> io_lib:format("~s=~s", [K,V]).

%% Take [{key, value}] list and create a query string.
-spec generate_query([{Key::string(), Value::string()}]) -> string().
generate_query([])     -> "";
generate_query([P])    -> predify(P);
generate_query([P|Ps]) -> predify(P) ++ "&" ++ generate_query(Ps).

%% These are the only valid response codes from plivo right now.
-spec parse_response({status_line(),headers(),body()}) -> jsx:json_term().
parse_response({{_,  200,_R},_H,Body}) -> jsx:decode(list_to_binary(Body));
parse_response({{_,  201,_R},_H,Body}) -> jsx:decode(list_to_binary(Body));
parse_response({{_,  202,_R},_H,Body}) -> jsx:decode(list_to_binary(Body));
parse_response({{_,  204,_R},_H,Body}) -> Body;
parse_response({{_,  400,_R},_H,Body}) -> Body;
parse_response({{_,  401,_R},_H,Body}) -> Body;
parse_response({{_,  404,_R},_H,Body}) -> Body;
parse_response({{_,  405,_R},_H,Body}) -> Body;
parse_response({{_,  500,_R},_H,Body}) -> Body;
parse_response({ _StatusLine,_H,Body}) -> Body.

%% Thin wrapper around httpc:request.
-spec request(atom(), Payload::payload()) -> jsx:json_term().
request(Method, Payload) ->
    {ok, Response} = httpc:request(Method, Payload, [], []),
    parse_response(Response).

%% ===================================================================
%% Api.
%% ===================================================================

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

%% ===================================================================
%% Account
%% ===================================================================

%% @spec create_subaccount(Params::params()) ->  jsx:json_term()
-spec create_subaccount(Params::params()) -> jsx:json_term().
create_subaccount(Params) -> plivo_account:create_subaccount(Params).

%% @spec delete_subaccount(SId::string()) ->  jsx:json_term()
-spec delete_subaccount(SId::string()) -> jsx:json_term().
delete_subaccount(SId) -> plivo_account:delete_subaccount(SId).

%% @spec get_account() ->  jsx:json_term()
-spec get_account() -> jsx:json_term().
get_account() -> plivo_account:get_account().

%% @spec get_subaccounts() ->  jsx:json_term()
-spec get_subaccounts() -> jsx:json_term().
get_subaccounts() -> plivo_account:get_subaccounts().

%% @spec get_subaccounts(Params::params()) ->  jsx:json_term()
-spec get_subaccounts(Params::params()) -> jsx:json_term().
get_subaccounts(Params) -> plivo_account:get_subaccounts(Params).

%% @spec get_subaccount(SId::string()) ->  jsx:json_term()
-spec get_subaccount(SId::string()) -> jsx:json_term().
get_subaccount(SId) -> plivo_account:get_subaccount(SId).

%% @spec modify_account(Params::params()) ->  jsx:json_term()
-spec modify_account(Params::params()) -> jsx:json_term().
modify_account(Params) -> plivo_account:modify_account(Params).

%% @spec modify_subaccount(SId::string(), Params::params()) -> jsx:json_term()
-spec modify_subaccount(SId::string(), Params::params()) -> jsx:json_term().
modify_subaccount(SId, Params) -> plivo_account:modify_subaccount(SId, Params).

%% ===================================================================
%% Application
%% ===================================================================


%% @spec create_application(Params::params()) ->  jsx:json_term()
-spec create_application(Params::params()) -> jsx:json_term().
create_application(Params) -> plivo_application:create_application(Params).

%% @spec delete_application(AppId::string()) ->  jsx:json_term()
-spec delete_application(AppId::string()) -> jsx:json_term().
delete_application(AppId) -> plivo_application:delete_application(AppId).

%% @spec get_application(AppId::string()) ->  jsx:json_term()
-spec get_application(AppId::string()) -> jsx:json_term().
get_application(AppId) -> plivo_application:get_application(AppId).

%% @spec get_applications() ->  jsx:json_term()
-spec get_applications() -> jsx:json_term().
get_applications() -> plivo_application:get_applications().

%% @spec get_applications(Params::params()) ->  jsx:json_term()
-spec get_applications(Params::params()) -> jsx:json_term().
get_applications(Params) -> plivo_application:get_applications(Params).

%% @spec modify_application(AppId::string(), Params::params()) ->
%%       jsx:json_term()
-spec modify_application(AppId::string(), Params::params()) -> jsx:json_term().
modify_application(AppId, Params) ->
    plivo_application:modify_application(AppId, Params).

%% ===================================================================
%% Call
%% ===================================================================

%% @spec get_cdrs() ->  jsx:json_term()
-spec get_cdrs() -> jsx:json_term().
get_cdrs() -> plivo_call:get_cdrs().

%% @spec get_cdrs(Params::params()) ->  jsx:json_term()
-spec get_cdrs(Params::params()) -> jsx:json_term().
get_cdrs(Params) -> plivo_call:get_cdrs(Params).

%% @spec get_cdr(CallId::string()) ->  jsx:json_term()
-spec get_cdr(CallId::string()) -> jsx:json_term().
get_cdr(CallId) -> plivo_call:get_cdr(CallId).

%% @spec get_cdr(CallId::string(), Params::params()) ->  jsx:json_term()
-spec get_cdr(CallId::string(), Params::params()) -> jsx:json_term().
get_cdr(CallId, Params) -> plivo_call:get_cdr(CallId, Params).

%% @spec get_live_call(CallId::string()) ->  jsx:json_term()
-spec get_live_call(CallId::string()) -> jsx:json_term().
get_live_call(CallId) -> plivo_call:get_live_call(CallId).

%% @spec get_live_calls() ->  jsx:json_term()
-spec get_live_calls() -> jsx:json_term().
get_live_calls() -> plivo_call:get_live_calls().

%% @spec hangup_call(CallId::string()) ->  jsx:json_term()
-spec hangup_call(CallId::string()) -> jsx:json_term().
hangup_call(CallId) -> plivo_call:hangup_call(CallId).

%% @spec hangup_request(ReqId::string()) ->  jsx:json_term()
-spec hangup_request(ReqId::string()) -> jsx:json_term().
hangup_request(ReqId) -> plivo_call:hangup_request(ReqId).

%% @spec make_call(Params::params()) ->  jsx:json_term()
-spec make_call(Params::params()) -> jsx:json_term().
make_call(Params) -> plivo_call:make_call(Params).

%% @spec play(CallId::string(), Params::params()) ->  jsx:json_term()
-spec play(CallId::string(), Params::params()) -> jsx:json_term().
play(CallId, Params) -> plivo_call:play(CallId, Params).

%% @spec record(CallId::string()) ->  jsx:json_term()
-spec record(CallId::string()) -> jsx:json_term().
record(CallId) -> plivo_call:record(CallId).

%% @spec record(CallId::string(), Params::params()) ->  jsx:json_term()
-spec record(CallId::string(), Params::params()) -> jsx:json_term().
record(CallId, Params) -> plivo_call:record(CallId, Params).

%% @spec send_digits(CallId::string(), Params::params()) ->  jsx:json_term()
-spec send_digits(CallId::string(), Params::params()) -> jsx:json_term().
send_digits(CallId, Params) -> plivo_call:send_digits(CallId, Params).

%% @spec speak(CallId::string(), Params::params()) ->  jsx:json_term()
-spec speak(CallId::string(), Params::params()) -> jsx:json_term().
speak(CallId, Params) -> plivo_call:speak(CallId, Params).

%% @spec stop_play(CallId::string()) ->  jsx:json_term()
-spec stop_play(CallId::string()) -> jsx:json_term().
stop_play(CallId) -> plivo_call:stop_play(CallId).

%% @spec stop_record(CallId::string()) ->  jsx:json_term()
-spec stop_record(CallId::string()) -> jsx:json_term().
stop_record(CallId) -> plivo_call:stop_record(CallId).

%% @spec stop_record(CallId::string(), Params::params()) ->  jsx:json_term()
-spec stop_record(CallId::string(), Params::params()) -> jsx:json_term().
stop_record(CallId, Params) -> plivo_call:stop_record(CallId, Params).

%% @spec stop_speak(CallId::string()) ->  jsx:json_term()
-spec stop_speak(CallId::string()) -> jsx:json_term().
stop_speak(CallId) -> plivo_call:stop_speak(CallId).

%% @spec transfer_call(CallId::string()) ->  jsx:json_term()
-spec transfer_call(CallId::string()) -> jsx:json_term().
transfer_call(CallId) -> plivo_call:transfer_call(CallId).

%% @spec transfer_call(CallId::string(), Params::params()) ->  jsx:json_term()
-spec transfer_call(CallId::string(), Params::params()) -> jsx:json_term().
transfer_call(CallId, Params) -> plivo_call:transfer_call(CallId, Params).

%% ===================================================================
%% Conference
%% ===================================================================

%% @spec get_live_conferences() -> jsx:json_term()
-spec get_live_conferences() -> jsx:json_term().
get_live_conferences() -> plivo_conference:get_live_conferences().

%% @spec get_live_conference(CName::string()) -> jsx:json_term()
-spec get_live_conference(CName::string()) -> jsx:json_term().
get_live_conference(CName) -> plivo_conference:get_live_conference(CName).

%% @spec hangup_live_conferences() -> jsx:json_term()
-spec hangup_live_conferences() -> jsx:json_term().
hangup_live_conferences() -> plivo_conference:hangup_live_conferences().

%% @spec hangup_live_conference(CName::string()) -> jsx:json_term()
-spec hangup_live_conference(CName::string()) -> jsx:json_term().
hangup_live_conference(CName) -> plivo_conference:hangup_live_conference(CName).

%% @spec hangup_member(CName::string(), MId::string()) -> jsx:json_term()
-spec hangup_member(CName::string(), MId::string()) -> jsx:json_term().
hangup_member(CName, MId) -> plivo_conference:hangup_member(CName, MId).

%% @spec kick_member(CName::string(), MId::string()) -> jsx:json_term()
-spec kick_member(CName::string(), MId::string()) -> jsx:json_term().
kick_member(CName, MId) -> plivo_conference:kick_member(CName, MId).

%% @spec mute_member(CName::string(), MId::string()) -> jsx:json_term()
-spec mute_member(CName::string(), MId::string()) -> jsx:json_term().
mute_member(CName, MId) -> plivo_conference:mute_member(CName, MId).

%% @spec unmute_member(CName::string(), MId::string()) -> jsx:json_term()
-spec unmute_member(CName::string(), MId::string()) -> jsx:json_term().
unmute_member(CName, MId) -> plivo_conference:unmute_member(CName, MId).

%% @spec play_member(CName::string(), MId::string(), Params::params()) ->
%%       jsx:json_term()
-spec play_member(CName::string(), MId::string(), Params::params()) ->
      jsx:json_term().
play_member(CName, MId, Params) ->
    plivo_conference:play_member(CName, MId, Params).

%% @spec unplay_member(CName::string(), MId::string()) -> jsx:json_term()
-spec unplay_member(CName::string(), MId::string()) -> jsx:json_term().
unplay_member(CName, MId) -> plivo_conference:unplay_member(CName, MId).

%% @spec speak_member(CName::string(), MId::string(), Params::params()) ->
%%       jsx:json_term()
-spec speak_member(CName::string(), MId::string(), Params::params()) ->
      jsx:json_term().
speak_member(CName, MId, Params) ->
    plivo_conference:speak_member(CName, MId, Params).

%% @spec deaf_member(CName::string(), MId::string()) -> jsx:json_term()
-spec deaf_member(CName::string(), MId::string()) -> jsx:json_term().
deaf_member(CName, MId) -> plivo_conference:deaf_member(CName, MId).

%% @spec undeaf_member(CName::string(), MId::string()) -> jsx:json_term()
-spec undeaf_member(CName::string(), MId::string()) -> jsx:json_term().
undeaf_member(CName, MId) -> plivo_conference:undeaf_member(CName, MId).

%% @spec record_conference(CId::string()) -> jsx:json_term()
-spec record_conference(CId::string()) -> jsx:json_term().
record_conference(CId) -> plivo_conference:record_conference(CId).

%% @spec stop_record_conference(CId::string()) -> jsx:json_term()
-spec stop_record_conference(CId::string()) -> jsx:json_term().
stop_record_conference(CId) -> plivo_conference:stop_record_conference(CId).

%% ===================================================================
%% Endpoint
%% ===================================================================

%% @spec create_endpoint(Params::params()) ->  jsx:json_term()
-spec create_endpoint(Params::params()) -> jsx:json_term().
create_endpoint(Params) -> plivo_endpoint:create_endpoint(Params).

%% @spec delete_endpoint(EId::string()) ->  jsx:json_term()
-spec delete_endpoint(EId::string()) -> jsx:json_term().
delete_endpoint(EId) -> plivo_endpoint:delete_endpoint(EId).

%% @spec get_endpoint(EId::string()) ->  jsx:json_term()
-spec get_endpoint(EId::string()) -> jsx:json_term().
get_endpoint(EId) -> plivo_endpoint:get_endpoint(EId).

%% @spec get_endpoints() ->  jsx:json_term()
-spec get_endpoints() -> jsx:json_term().
get_endpoints() -> plivo_endpoint:get_endpoints().

%% @spec modify_endpoint(EId::string(), Params::params()) ->  jsx:json_term()
-spec modify_endpoint(EId::string(), Params::params()) -> jsx:json_term().
modify_endpoint(EId, Params) -> plivo_endpoint:modify_endpoint(EId, Params).

%% ===================================================================
%% Message
%% ===================================================================

%% @spec get_message(MId::string()) ->  jsx:json_term()
-spec get_message(MId::string()) -> jsx:json_term().
get_message(MId) -> plivo_message:get_message(MId).

%% @spec get_messages() ->  jsx:json_term()
-spec get_messages() -> jsx:json_term().
get_messages() -> plivo_message:get_messages().

%% @spec get_messages(Params::params()) ->  jsx:json_term()
-spec get_messages(Params::params()) -> jsx:json_term().
get_messages(Params) -> plivo_message:get_messages(Params).

%% @spec send_message(Params::params()) ->  jsx:json_term()
-spec send_message(Params::params()) -> jsx:json_term().
send_message(Params) -> plivo_message:send_message(Params).

%% ===================================================================
%% Number
%% ===================================================================

%% ===================================================================
%% Carrier
%% ===================================================================

%% ===================================================================
%% Pricing
%% ===================================================================

%% @spec get_pricing(Params::params()) ->  jsx:json_term()
-spec get_pricing(Params::params()) -> jsx:json_term().
get_pricing(Params) -> plivo_pricing:get_pricing(Params).

%% ===================================================================
%% Recording
%% ===================================================================

%% @spec get_recording(RId::string()) -> jsx:json_term()
-spec get_recording(RId::string()) -> jsx:json_term().
get_recording(RId) -> plivo_recording:get_recording(RId).

%% @spec get_recordings(Params::params()) -> jsx:json_term()
-spec get_recordings(Params::rest_api:params()) -> jsx:json_term().
get_recordings(Params) -> plivo_recording:get_recordings(Params).
