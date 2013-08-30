-module(plivo_application).

-export([create_application/1, delete_application/1, get_application/1,
         get_applications/0, get_applications/1, modify_application/2]).

%% @spec create_application(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Creates a new application.
%%      Required params:
%%          answer_url  The URL Plivo will fetch when a call executes this
%%                      application.
%%          app_name    The name of your application.
%%      Optional params:
%%          answer_method        The method used to call the answer_url.
%%                               Defaults to POST.
%%          hangup_url           The URL that will be notified by Plivo
%%                               when the call hangs up.
%%          hangup_method        The method used to call the hangup_url.
%%                               Defaults to POST.
%%          fallback_answer_url  Invoked by Plivo only if answer_url is
%%                               unavailable or the XML response is invalid.
%%                               Should contain a XML response.
%%          fallback_method      The method used to call the
%%                               fallback_answer_url.
%%                               Defaults to POST.
%%          message_url          The URL that will be notified by Plivo
%%                               when an inbound message is received.
%%                               Defaults not set.
%%          message_method       The method used to call the message_url.
%%                               Defaults to POST.
%%          default_number_app   If set to true,
%%                               this parameter ensures that
%%                               newly created numbers,
%%                               which don't have an app_id,
%%                               point to this application.
%%          default_endpoint_app If set to true,
%%                               this parameter ensures that newly created
%%                               endpoints, which don't have an app_id,
%%                               point to this application.
-spec create_application(Params::rest_api:params()) -> jsx:json_term().
create_application(Params) -> rest_api:api(post, "Application/", Params).

%% @spec delete_application(AppId::string()) ->  jsx:json_term()
%% @doc Deletes an application.
-spec delete_application(AppId::string()) -> jsx:json_term().
delete_application(AppId) ->
    rest_api:api(delete, "Application/" ++ AppId ++ "/").

%% @spec get_application(AppId::string()) ->  jsx:json_term()
%% @doc Grabs one specific application.
-spec get_application(AppId::string()) -> jsx:json_term().
get_application(AppId) -> rest_api:api(get, "Application/" ++ AppId ++ "/").

%% @spec get_applications() ->  jsx:json_term()
%% @doc Grabs all of the applications.
-spec get_applications() -> jsx:json_term().
get_applications() -> get_applications([]).

%% @spec get_applications(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Grabs all of the applications.
%%      Optional params:
%%          subaccount Id or name or alias of the subaccount,
%%                     in case only subaccount applications are needed.
%%          limit      Used to display the number of results per page.
%%                     The maximum number of results that can be fetched is 20.
%%          offset     Denotes the number of value items by which
%%                     the results should be offset.
-spec get_applications(Params::rest_api:params()) -> jsx:json_term().
get_applications(Params) -> rest_api:api(get, "Application/", Params).

%% @spec modify_application(AppId::string(), Params::rest_api:params()) ->
%%       jsx:json_term()
%% @doc Modifies an existing application.
%%      Optional params:
%%          answer_url           The URL invoked by Plivo
%%                               when a call executes this application.
%%          answer_method        The method used to call the answer_url.
%%                               Defaults to POST
%%          hangup_url           The URL that is notified by Plivo
%%                               when the call hangs up.
%%          hangup_method        The method used to call the hangup_url.
%%                               Defaults to POST
%%          fallback_answer_url  Invoked by Plivo only if answer_url is
%%                               unavailable or the XML response is invalid.
%%                               Should contain a XML response.
%%          fallback_method      The method used to call the
%%                               fallback_answer_url.
%%                               Defaults to POST.
%%          message_url          The URL that is notified by Plivo
%%                               when an inbound message is received.
%%                               Defaults not set.
%%          message_method       The method used to call the message_url.
%%                               Defaults to POST.
%%          default_number_app   If set to true,
%%                               associates all newly created Plivo numbers
%%                               that have not specified an app_id,
%%                               to this application.
%%          default_endpoint_app If set to true,
%%                               associates all newly created Plivo endpoints
%%                               that have not specified an app_id,
%%                               to this application.
-spec modify_application(AppId::string(), Params::rest_api:params()) ->
      jsx:json_term().
modify_application(AppId, Params) ->
    rest_api:api(post, "Application/" ++ AppId ++ "/", Params).
