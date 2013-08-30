-module(plivo_recording).

-export([get_recording/1, get_recordings/1]).

%% @spec get_recording(RId::string()) -> jsx:json_term()
%% @doc Gets a specific recording.
-spec get_recording(RId::string()) -> jsx:json_term().
get_recording(RId) -> rest_api:api(get, "Recording/" ++ RId ++ "/").

%% @spec get_recordings(Params::params()) -> jsx:json_term()
%% @doc Gets all the recordings for this account.
%%      Optional Params
%%      subaccount auth_id of the subaccount.
%%                 Lists only those recordings of the main accounts which
%%                 are tied to the specified subaccount.
%%      call_uuid  Used to filter recordings for a specific call.
%%      add_time   Used to filter out recordings according to the time added.
%%      limit      The number of results.
%%      offset     The number of pages by which the results should be offset.
-spec get_recordings(Params::rest_api:params()) -> jsx:json_term().
get_recordings(Params) -> rest_api:api(get, "Recording/", Params).
