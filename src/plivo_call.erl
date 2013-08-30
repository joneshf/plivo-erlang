-module(plivo_call).

-export([get_cdr/1, get_cdr/2, get_cdrs/0, get_cdrs/1, get_live_call/1,
         get_live_calls/0, hangup_call/1, hangup_request/1, make_call/1, play/2,
         record/1, record/2, send_digits/2, speak/2, stop_play/1, stop_record/1,
         stop_record/2, stop_speak/1, transfer_call/1, transfer_call/2]).

%% @spec get_cdrs() ->  jsx:json_term()
%% @doc Gets all call detail records.
-spec get_cdrs() -> jsx:json_term().
get_cdrs() -> get_cdrs([]).

%% @spec get_cdrs(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Gets all call detail records.
%%      subaccount     The id of the subaccount.
%%      call_direction Filter the results
%%                     by call direction.
%%                     The valid inputs are inbound and outbound.
%%      from_number    Filter the results
%%                     by the number from where the call originated.
%%      to_number      Filter the results
%%                     by the number to which the call was made.
%%      bill_duration  Filter the results
%%                     according to billed duration.
%%                     The value of billed duration is in seconds.
%%      end_time       Filter the results
%%                     according to the time of completion.
%%      limit          Used to display the number of results per page.
%%                     The maximum number of results that can be fetched is 20.
%%      offset         Denotes the number of value items by which the results
%%                     should be offset.
-spec get_cdrs(Params::rest_api:params()) -> jsx:json_term().
get_cdrs(Params) -> rest_api:api(get, "Call/", Params).

%% @spec get_cdr(CallId::string()) ->  jsx:json_term()
%% @doc Gets call detail record for a specified call.
%%      CallId UUID of a call.
-spec get_cdr(CallId::string()) -> jsx:json_term().
get_cdr(CallId) -> get_cdr(CallId, []).

%% @spec get_cdr(CallId::string(), Params::rest_api:params()) -> jsx:json_term()
%% @doc Gets call detail record for a specified call.
%%      CallId UUID of a call.
%%
%%      Optional Params
%%      subaccount The id of the subaccount.
%%      limit      The number of results.
%%      offset     The number of pages by which the results should be offset.
-spec get_cdr(CallId::string(), Params::rest_api:params()) -> jsx:json_term().
get_cdr(CallId, Params) -> rest_api:api(get, "Call/" ++ CallId ++ "/", Params).

%% @spec get_live_call(CallId::string()) ->  jsx:json_term()
%% @doc Gets details of a specific active call.
%%      CallId UUID of a call.
-spec get_live_call(CallId::string()) -> jsx:json_term().
get_live_call(CallId) ->
    rest_api:api(get, "Call/" ++ CallId ++ "/", [{status, <<"live">>}]).

%% @spec get_live_calls() ->  jsx:json_term()
%% @doc Gets all currently active calls.
-spec get_live_calls() -> jsx:json_term().
get_live_calls() -> rest_api:api(get, "Call/", [{status, <<"live">>}]).

%% @spec hangup_call(CallId::string()) ->  jsx:json_term()
%% @doc Hangs up a specific call.
%%      CallId UUID of a call.
-spec hangup_call(CallId::string()) -> jsx:json_term().
hangup_call(CallId) -> rest_api:api(delete, "Call/" ++ CallId ++ "/").

%% @spec hangup_request(ReqId::string()) ->  jsx:json_term()
%% @doc Hangs up a specific call.  This uses the request_uuid from make_call/1.
%%      ReqId UUID of a request.
-spec hangup_request(ReqId::string()) -> jsx:json_term().
hangup_request(ReqId) -> rest_api:api(delete, "Request/" ++ ReqId ++ "/").

%% @spec make_call(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Create an outbound call.
%%      Required Params
%%      from       The phone number to be used as the caller id
%%                 with the country code.
%%                 E.g, a USA caller id number could be:
%%                 &lt;&lt;"15677654321"&gt;&gt;,
%%                 with 1 for country code.
%%      to         The regular number(s) or sip endpoint(s) to call.
%%                 Regular number must be prefixed with country code but
%%                 without the '+' sign.
%%                 E.g, to dial a number in the USA, the number could be:
%%                 15677654321, with '1' for the country code.
%%                 Sip endpoints must be prefixed with 'sip:'.
%%                 E.g., sip:john1234@phone.plivo.com.
%%                 To make bulk calls, the delimiter '&lt;' is used.
%%                 E.g.:
%%                 15677654321&lt;15673464321&lt;sip:john1234@phone.plivo.com
%%      answer_url The URL invoked by Plivo when the outbound call is answered.
%%
%%      Optional Params
%%      answer_method          The method used to call the answer_url.
%%                             Defaults to POST.
%%      ring_url               The URL that is notified by Plivo
%%                             when the call is ringing. Defaults not set.
%%      ring_method            The method used to call the ring_url.
%%                             Defaults to POST.
%%      hangup_url             The URL that is notified by Plivo
%%                             when the call hangs up.
%%                             Defaults to answer_url.
%%      hangup_method          The method used to call the hangup_url.
%%                             Defaults to POST.
%%      fallback_url           Invoked by Plivo only if answer_url
%%                             is unavailable or the XML response is invalid.
%%                             Should contain a XML response.
%%      fallback_method        The method used to call the fallback_url.
%%                             Defaults to POST.
%%      caller_name            Caller name to use with the call.
%%      send_digits            Plivo plays DTMF tones when the call is answered.
%%                             This is useful when dialing a phone number
%%                             and an extension.
%%                             Plivo will dial the number,
%%                             and when the automated system picks up,
%%                             sends the DTMF tones to connect to the extension.
%%      send_on_preanswer      If set to true and send_digits is also set,
%%                             digits are sent when the call is in a
%%                             preanswer state.
%%                             Defaults to false.
%%      time_limit             Schedules the call for hangup at a specified
%%                             time after the call is answered.
%%      hangup_on_ring         Schedules the call for hangup at a specified
%%                             time after the call starts ringing.
%%      machine_detection      Used to detect if the call has been answered
%%                             by a machine.
%%                             The valid values are true and hangup.
%%                             Default time to analyze is 5000 milliseconds
%%                             You can change it with the
%%                             machine_detection_time parameter.
%%                             Note that no XML is processed during
%%                             the analysis phase.
%%                             If a machine is detected during the call and
%%                             machine_detection is set to true,
%%                             the Machine parameter will be set to true and
%%                             will be sent to the answer_url, hangup_url, or
%%                             any other URL that is invoked by the call.
%%                             If a machine is detected during the call and
%%                             machine_detection is set to hangup,
%%                             the call hangs up immediately and
%%                             a request is made to the hangup_url with
%%                             the Machine parameter set to true.
%%      machine_detection_time Time allotted to analyze if the call has been
%%                             answered by a machine.
%%                             It should be 2000 &lt;= and &lt;= 10000 in ms.
%%                             The default value is 5000 ms.
%%      sip_headers            List of SIP headers in the form of
%%                             'key=value' pairs, separated by commas.
%%                             E.g. head1=val1,head2=val2,...,headN=valN.
%%                             The SIP headers are always prefixed with X-PH-.
%%                             The SIP headers are present for every HTTP
%%                             request made by the outbound call.
%%                             Only [A-Z], [a-z] and [0-9] characters
%%                             are allowed for the SIP headers key and value.
%%                             Additionally, the '%' character is also allowed
%%                             for the SIP headers value so that you can encode
%%                             this value in the URL.
%%      ring_timeout           Determines the time in seconds
%%                             the call should ring.
%%                             If the call is not answered within the
%%                             ring_timeout value or
%%                             the default value of 120 seconds, it is canceled.
-spec make_call(Params::rest_api:params()) -> jsx:json_term().
make_call(Params) -> rest_api:api(post, "Call/", Params).

%% @spec play(CallId::string(), Params::rest_api:params()) ->  jsx:json_term()
%% @doc Plays a sound during a call.
%%      Required Params
%%      urls A single URL or a list of comma separated URLs
%%           pointing to an mp3 or wav file.
%%
%%      Optional Params
%%      length Maximum length in seconds to play.
%%      legs   The leg which to be used, can be aleg (current call),
%%             bleg (the other party during Dial) or both.
%%             Defaults to aleg.
%%      loop   If set to true, play the sounds indefinitely.
%%             Defaults to false.
%%      mix    If set to true, sounds are mixed with current audio flow.
%%             Defaults to true.
-spec play(CallId::string(), Params::rest_api:params()) -> jsx:json_term().
play(CallId, Params) ->
    rest_api:api(post, "Call/" ++ CallId ++ "/Play/", Params).

%% @spec record(CallId::string()) ->  jsx:json_term()
%% @doc Record a call.
-spec record(CallId::string()) -> jsx:json_term().
record(CallId) -> record(CallId, []).

%% @spec record(CallId::string(), Params::rest_api:params()) ->  jsx:json_term()
%% @doc Record a call.
%%      time_limit           Max recording duration in seconds. Defaults to 60.
%%      file_format          The format of the recording.
%%                           The valid formats are mp3 and wav formats.
%%                           Defaults to mp3.
%%      transcription_type   The type of transcription required.
%%                           The following values are allowed:
%%          auto   This is the default value.
%%                 Transcription is completely automated.
%%                 Turnaround time is about 5 minutes.
%%          hybrid Transcription is a combination of automated and
%%                 human verification processes.
%%                 Turnaround time is about 10-15 minutes.
%%      transcription_url    The URL where the transcription is available.
%%      transcription_method The method used to invoke the transcription_url.
%%                           Defaults to POST.
%%      callback_url         The URL invoked by the API when the recording ends.
%%                           The following parameters are sent:
%%          api_id                the same API ID returned by
%%                                the call record API.
%%          record_url            the URL to access the recorded file.
%%          call_uuid             the call uuid of the recorded call.
%%          recording_duration    duration in seconds of the recording.
%%          recording_duration_ms duration in milliseconds of the recording.
%%          recording_start_ms    when the recording started (epoch time UTC)
%%                                in milliseconds.
%%          recording_end_ms      when the recording ended (epoch time UTC)
%%                                in milliseconds.
%%      callback_method      The method which is used to invoke the
%%                           callback_url URL.
%%                           Defaults to POST.
-spec record(CallId::string(), Params::rest_api:params()) -> jsx:json_term().
record(CallId, Params) ->
    rest_api:api(post, "Call/" ++ CallId ++ "/Record/", Params).

%% @spec send_digits(CallId::string(), Params::rest_api:params()) ->
%%       jsx:json_term()
%% @doc Send digits to a call.
%%      Required Params
%%      digits Digits to send.
%%      Optional Params
%%      leg The leg to be used, can be]
%%          aleg (the current call) or bleg (the other party in a Dial).
%%          Defaults to aleg.
-spec send_digits(CallId::string(), Params::rest_api:params()) ->
      jsx:json_term().
send_digits(CallId, Params) ->
    rest_api:api(post, "Call/" ++ CallId ++ "/DTMF/", Params).

%% @spec speak(CallId::string(), Params::rest_api:params()) ->  jsx:json_term()
%% @doc Play text during a call.
%%      Required Params
%%      text Text to be played.
%%
%%      Optional Params
%%      voice    The voice to be used, can be MAN,WOMAN. Defaults to WOMAN.
%%      language The language to be used.
%%               See offical documentation for available languages.
%%               Defaults to en-US.
%%      loop     If set to true, play indefinitely. Defaults to false.
%%      mix      If true, mixes sounds with current audio flow.
%%               Defaults to true.
-spec speak(CallId::string(), Params::rest_api:params()) -> jsx:json_term().
speak(CallId, Params) ->
    rest_api:api(post, "Call/" ++ CallId ++ "/Speak/", Params).

%% @spec stop_play(CallId::string()) ->  jsx:json_term()
%% @doc Stop playing all sounds during a call.
-spec stop_play(CallId::string()) -> jsx:json_term().
stop_play(CallId) -> rest_api:api(delete, "Call/" ++ CallId ++ "/Play/").

%% @spec stop_record(CallId::string()) ->  jsx:json_term()
%% @doc Stop recording all calls.
%%      Optional Params
%%      URL Stops specific recording.
-spec stop_record(CallId::string()) -> jsx:json_term().
stop_record(CallId) -> stop_record(CallId, []).

%% @spec stop_record(CallId::string(), Params::rest_api:params()) ->
%%       jsx:json_term()
%% @doc Stop recording all calls.
%%      Optional Params
%%      URL Stops specific recording.
-spec stop_record(CallId::string(), Params::rest_api:params()) ->
    jsx:json_term().
stop_record(CallId, Params) ->
    rest_api:api(delete, "Call/" ++ CallId ++ "/Record/", Params).

%% @spec stop_speak(CallId::string()) ->  jsx:json_term()
%% @doc Stop playing text on specified call.
-spec stop_speak(CallId::string()) -> jsx:json_term().
stop_speak(CallId) -> rest_api:api(delete, "Call/" ++ CallId ++ "/Speak/").

%% @spec transfer_call(CallId::string()) ->  jsx:json_term()
%% @doc Transfer calls from one url to another.
-spec transfer_call(CallId::string()) -> jsx:json_term().
transfer_call(CallId) -> rest_api:api(post, "Call/" ++ CallId ++ "/", []).

%% @spec transfer_call(CallId::string(), Params::rest_api:params()) ->
%%       jsx:json_term()
%% @doc Transfer calls from one url to another.
%%      legs        'aleg', 'bleg' or 'both'.
%%                  Defaults to 'aleg' 'aleg' will transfer call_uuid.
%%                  'bleg' will transfer the bridged leg of call_uuid.
%%                  'both' will transfer call_uuid and bridged leg of call_uuid.
%%      aleg_url    URL to transfer for 'aleg'.
%%                  if legs is 'aleg' or 'both',
%%                  then aleg_url has to be specified.
%%      aleg_method HTTP method to invoke aleg_url. Defaults to POST.
%%      bleg_url    URL to transfer for bridged leg.
%%                  if legs is 'bleg' or 'both',
%%                  then bleg_url has to be specified.
%%      bleg_method HTTP method to invoke bleg_url. Defaults to POST.
-spec transfer_call(CallId::string(), Params::rest_api:params()) ->
      jsx:json_term().
transfer_call (CallId, Params) ->
    rest_api:api(post, "Call/" ++ CallId ++ "/", Params).
