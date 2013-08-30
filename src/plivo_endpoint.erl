-module(plivo_endpoint).

-export([create_endpoint/1, delete_endpoint/1, get_endpoint/1, get_endpoints/0,
         modify_endpoint/2]).

%% @spec create_endpoint(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Creates a SIP endpoint.
%%      Required Params
%%      username The username for the endpoint to be created.
%%      password The password for your endpoint username.
%%      alias    Alias for this endpoint
%%
%%      Optional Params
%%      app_id  The app_id of the application that is to be attached
%%              to this endpoint.
%%              If this is not provided the default_endpoint_app
%%              is attached to this endpoint.
-spec create_endpoint(Params::rest_api:params()) -> jsx:json_term().
create_endpoint(Params) -> rest_api:api(post, "Endpoint/", Params).

%% @spec delete_endpoint(EId::string()) ->  jsx:json_term()
%% @doc Delete a specific endpoint.
-spec delete_endpoint(EId::string()) -> jsx:json_term().
delete_endpoint(EId) -> rest_api:api(delete, "Endpoint/" ++ EId ++ "/").

%% @spec get_endpoint(EId::string()) ->  jsx:json_term()
%% @doc Get a specific endpoint.
-spec get_endpoint(EId::string()) -> jsx:json_term().
get_endpoint(EId) -> rest_api:api(get, "Endpoint/" ++ EId ++ "/").

%% @spec get_endpoints() ->  jsx:json_term()
%% @doc Get all endpoints.
-spec get_endpoints() -> jsx:json_term().
get_endpoints() -> rest_api:api(get, "Endpoint/").

%% @spec modify_endpoint(EId::string(), Params::rest_api:params()) ->
%%       jsx:json_term()
%% @doc Modify a specific endpoint.
%%      Optional Params
%%      password The password for your endpoint username.
%%      alias    Alias for this endpoint
%%      app_id  The app_id of the application that is to be attached
%%              to this endpoint.
%%              If this is not provided the default_endpoint_app
%%              is attached to this endpoint.
-spec modify_endpoint(EId::string(), Params::rest_api:params()) ->
      jsx:json_term().
modify_endpoint(EId, Params) ->
    rest_api:api(post, "Endpoint/" ++ EId ++ "/", Params).
