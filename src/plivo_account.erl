-module(plivo_account).

-export([create_subaccount/1, delete_subaccount/1, get_account/0,
         get_subaccounts/0, get_subaccounts/1, get_subaccount/1,
         modify_account/1, modify_subaccount/2]).

%% @spec create_subaccount(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Creates a new subaccount and returns a response.
%%      Requires two params name and enabled.
%%      name is the name of the subaccount.
%%      enabled specifies whether a subaccount should be enabled.
-spec create_subaccount(Params::rest_api:params()) -> jsx:json_term().
create_subaccount(Params) -> rest_api:api(post, "Subaccount/", Params).

%% @spec delete_subaccount(SId::string()) ->  jsx:json_term()
%% @doc Removes the subaccount from the specified account.
-spec delete_subaccount(SId::string()) -> jsx:json_term().
delete_subaccount(SId) -> rest_api:api(delete, "Subaccount/" ++ SId ++ "/").

%% @spec get_account() ->  jsx:json_term()
%% @doc Returns the account information for the supplied AId.
-spec get_account() -> jsx:json_term().
get_account() -> rest_api:api(get, "").

%% @spec get_subaccounts() ->  jsx:json_term()
%% @doc Returns the subaccounts information for the supplied AId.
-spec get_subaccounts() -> jsx:json_term().
get_subaccounts() -> get_subaccounts([]).

%% @spec get_subaccounts(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Returns the subaccounts information for the supplied AId.
%%      Optional params are: limit, offset.
%%      limit is the maximum number of results returned.  Max 20.
%%      offset is the subaccount start number.  Zero based.
%%      That is, if you want accounts 23-29, you would pass in params
%%      [{limit, 7}, {offset, 22}]
-spec get_subaccounts(Params::rest_api:params()) -> jsx:json_term().
get_subaccounts(Params) -> rest_api:api(get, "Subaccount/", Params).

%% @spec get_subaccount(SId::string()) ->  jsx:json_term()
%% @doc Returns the subaccount information for the supplied SId combo.
-spec get_subaccount(SId::string()) -> jsx:json_term().
get_subaccount(SId) -> rest_api:api(get, "Subaccount/" ++ SId ++ "/").

%% @spec modify_account(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Modifies an existing account.
%%      Optional Params are name, city and address.
%%      Params must be a list of key, val tuples.
%%      E.g.: [{name, "Wilson"}, {address, "Some island."}]
-spec modify_account(Params::rest_api:params()) -> jsx:json_term().
modify_account(Params) -> rest_api:api(post, "", Params).

%% @spec modify_subaccount(SId::string(), Params::rest_api:params()) ->
%%       jsx:json_term()
%% @doc Modifies an existing Subaccount.
%%      Requires two params name and enabled.
%%      name is the name of the subaccount.
%%      enabled specifies whether a subaccount should be enabled.
-spec modify_subaccount(SId::string(), Params::rest_api:params()) ->
      jsx:json_term().
modify_subaccount(SId, Params) ->
    rest_api:api(post, "Subaccount/" ++ SId ++ "/", Params).
