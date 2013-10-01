-module(plivo_conference).

-export([get_live_conferences/1, get_live_conference/1,
         hangup_live_conferences/1, hangup_live_conference/1, hangup_member/1,
         kick_member/1, mute_member/1, unmute_member/1, play_member/1,
         unplay_member/1, speak_member/1, deaf_member/1, undeaf_member/1,
         record_conference/1, stop_record_conference/1]).

%% @spec get_live_conferences() -> jsx:json_term()
%% @doc Gets a list of all the current conferences.
-spec get_live_conferences() -> jsx:json_term().
get_live_conferences() -> rest_api:api(get, "Conference/").

%% @spec get_live_conference(CName::string()) -> jsx:json_term()
%% @doc Gets a specific conference.
-spec get_live_conference(CName::string()) -> jsx:json_term().
get_live_conference(CName) -> rest_api:api(get, "Conference/" ++ CName ++ "/").

%% @spec hangup_live_conferences() -> jsx:json_term()
%% @doc Hangs up all the current conferences.
-spec hangup_live_conferences() -> jsx:json_term().
hangup_live_conferences() -> rest_api:api(delete, "Conference/").

%% @spec hangup_live_conference(CName::string()) -> jsx:json_term()
%% @doc Hangs up a specified conference.
-spec hangup_live_conference(CName::string()) -> jsx:json_term().
hangup_live_conference(CName) ->
    rest_api:api(delete, "Conference/" ++ CName ++ "/").

%% @spec hangup_member(CName::string(), MId::string()) -> jsx:json_term()
%% @doc Hangs up a specific member of a conference call.
-spec hangup_member(CName::string(), MId::string()) -> jsx:json_term().
hangup_member(CName, MId) ->
    rest_api:api(delete, "Conference/" ++ CName ++ "/Member/" ++ MId ++ "/").

%% @spec kick_member(CName::string(), MId::string()) -> jsx:json_term()
%% @doc Kicks specific member from a conference call.
-spec kick_member(CName::string(), MId::string()) -> jsx:json_term().
kick_member(CName, MId) ->
    rest_api:api(post, "Conference/" ++ CName ++ "/Member/" ++ MId ++ "/Kick/").

%% @spec mute_member(CName::string(), MId::string()) -> jsx:json_term()
%% @doc Mutes a specific member in a conference call.
-spec mute_member(CName::string(), MId::string()) -> jsx:json_term().
mute_member(CName, MId) ->
    rest_api:api(post, "Conference/" ++ CName ++ "/Member/" ++ MId ++ "/Mute/").

%% @spec unmute_member(CName::string(), MId::string()) -> jsx:json_term()
%% @doc Unmutes a specific member in a conference call.
-spec unmute_member(CName::string(), MId::string()) -> jsx:json_term().
unmute_member(CName, MId) ->
    rest_api:api(delete,
                 "Conference/" ++ CName ++ "/Member/" ++ MId ++ "/Mute/").

%% @spec play_member(CName::string(), MId::string(),
%%                   Params::rest_api:params()) -> jsx:json_term()
%% @doc Plays a sound for a member in a conference call.
%%      Required params:
%%          URL Url of the sound to be played.
-spec play_member(CName::string(), MId::string(), Params::rest_api:params()) ->
      jsx:json_term().
play_member(CName, MId, Params) ->
    rest_api:api(post,
                 "Conference/" ++ CName ++ "/Member/" ++  MId ++ "/Play/",
                 Params).

%% @spec unplay_member(CName::string(), MId::string()) -> jsx:json_term()
%% @doc Stops playing a sound for a member in a conference call.
-spec unplay_member(CName::string(), MId::string()) -> jsx:json_term().
unplay_member(CName, MId) ->
    rest_api:api(delete,
                 "Conference/" ++ CName ++ "/Member/" ++  MId ++ "/Play/").

%% @spec speak_member(CName::string(), MId::string(),
%%                   Params::rest_api:params()) -> jsx:json_term()
%% @doc Plays a speech for a member in a conference call.
%%      Required Parameters
%%          text The text that the member must hear.
%%      Optional Parameters
%%          voice    The voice to be used. Can be MAN or WOMAN.
%%                   Defaults to WOMAN.
%%          language The language to be used,
%%                   see supported voices and languages.
%%                   Defaults to en-US.
-spec speak_member(CName::string(), MId::string(), Params::rest_api:params()) ->
      jsx:json_term().
speak_member(CName, MId, Params) ->
    rest_api:api(post,
                 "Conference/" ++ CName ++ "/Member/" ++ MId ++ "/Speak/",
                 Params).

%% @spec deaf_member(CName::string(), MId::string()) -> jsx:json_term()
%% @doc Makes a member deaf in a conference call.
-spec deaf_member(CName::string(), MId::string()) -> jsx:json_term().
deaf_member(CName, MId) ->
    rest_api(post, "Conference/" ++ CName ++ "/Member/" ++ MId ++ "/Deaf/").

%% @spec undeaf_member(CName::string(), MId::string()) -> jsx:json_term()
%% @doc Makes a member able to hear in a conference call.
-spec undeaf_member(CName::string(), MId::string()) -> jsx:json_term().
undeaf_member(CName, MId) ->
    rest_api(delete, "Conference/" ++ CName ++ "/Member/" ++ MId ++ "/Deaf/").

%% @spec record_conference(CId::string()) -> jsx:json_term()
%% @doc Records a specific conference call.
%%      Optional params:
%%          file_format          The file format of the record can be of
%%                               mp3 or wav format. Defaults to mp3 format.
%%          transcription_type   The type of transcription required.
%%                               The following values are allowed:
%%              auto   This is the default value.
%%                     Transcription is completely automated.
%%                     Turnaround time is about 5 minutes.
%%              hybrid Transcription is a combination of automated and
%%                     human verification processes.
%%                     Turnaround time is about 10-15 minutes.
%%
%%          transcription_url    The URL where the transcription is available.
%%          transcription_method The method used on the transcription_url.
%%                               Defaults to POST.
%%          callback_url         The URL invoked when the recording ends.
%%                               The following parameters are sent:
%%              api_id                The same API ID returned by
%%                                    the conference record API.
%%              record_url            The URL to access the recorded file.
%%              conference_name       The conference name recorded.
%%              recording_duration    Duration in seconds of the recording.
%%              recording_duration_ms Duration in milliseconds of the recording.
%%              recording_start_ms    Started (epoch time UTC) in milliseconds.
%%              recording_end_ms      Ended (epoch time UTC) in milliseconds.
%%          callback_method      The method is used on the callback_url URL.
%%                               Defaults to POST.
-spec record_conference(CId::string()) -> jsx:json_term().
record_conference(CId) ->
    rest_api:api(post, "Conference/" ++ CId ++ "/Record/").

%% @spec stop_record_conference(CId::string()) -> jsx:json_term()
%% @doc Stops recording a specific conference call.
-spec stop_record_conference(CId::string()) -> jsx:json_term().
stop_record_conference(CId) ->
    rest_api:api(delete, "Conference/" ++ CId ++ "/Record/").

