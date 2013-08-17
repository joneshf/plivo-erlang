-module (plivo).

-export ([get_account/1]).

-define (ACCOUNT_URI, "https://api.plivo.com/v1/Account/").
-define (AUTH_HEADER,
         {"Authorization",
          "Basic " ++ base64:encode_to_string(?AUTH_ID ++ ":" ++ ?AUTH_TOKEN)}).
-define (AUTH_ID, "").
-define (AUTH_TOKEN, "").

rest_api(get, Uri) ->
    inets:start(),
    ssl:start(),
    httpc:request(get, {Uri, [?AUTH_HEADER]}, [], []).

get_account(AuthID) ->
    {ok, Response} = rest_api(get, ?ACCOUNT_URI ++ AuthID),
    {{_Crap, StatusCode, Reason}, _Headers, Body} = Response,
    mochijson:decode(Body).
