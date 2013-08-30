-module(plivo_pricing).

-export([get_pricing/1]).

%% @spec get_pricing(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Gets the pricing for the specified country.
%%      Required Params
%%      country_iso The 2 digit country ISO code. eg. US, GB, QA
-spec get_pricing(Params::rest_api:params()) -> jsx:json_term().
get_pricing(Params) -> rest_api:api(get, "Pricing/", Params).
