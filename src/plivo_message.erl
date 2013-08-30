-module(plivo_message).

-export([get_message/1, get_messages/0, get_messages/1, send_message/1]).

%% @spec get_message(MId::string()) ->  jsx:json_term()
%% @doc Get a specific message.
-spec get_message(MId::string()) -> jsx:json_term().
get_message(MId) -> rest_api:api(get, "Message/" ++ MId ++ "/").

%% @spec get_messages() ->  jsx:json_term()
%% @doc Get all messages.
-spec get_messages() -> jsx:json_term().
get_messages() -> get_messages([]).

%% @spec get_messages(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Get all messages.
%%      Optional Params
%%      limit      The number of results.
%%      offset     The number of pages by which the results should be offset.
-spec get_messages(Params::rest_api:params()) -> jsx:json_term().
get_messages(Params) -> rest_api:api(get, "Message/", Params).

%% @spec send_message(Params::rest_api:params()) ->  jsx:json_term()
%% @doc Send a message via SMS.
%%      Required Params
%%      src  The phone number to be used as the caller id
%%           (with the country code).
%%           E.g. a USA number, 15671234567
%%      dst  The number to which the message needs to be sent.
%%           Regular phone numbers must be prefixed with the country code
%%           but without the ‘+’ sign.
%%           E.g, a USA caller id number could be, 15677654321,
%%           with '1' for the country code.
%%           Multiple numbers can be sent by using a delimiter.
%%           E.g. 15677654321&lt;12077657621&lt;12047657621.
%%      text The text to send encoded in Unicode UTF-8.
%%           The API accepts up to 1000 bytes of UTF-8 encoded text in a
%%           single API request.
%%           The text will be automatically split into multiple parts and
%%           sent if it will not fit into a single SMS.
%%
%%      Optional Params
%%      type   The type of message. Should be 'sms' for a text message.
%%             Defaults to 'sms'.
%%      url    The URL to which with the status of the message is sent.
%%             The following parameters are sent to the URL:
%%                To                Receiver number of the SMS
%%                From              Sender number of the SMS
%%                Status            Status value of the messages, one of:
%%                                  "queued", "sent", "failed", "delivered",
%%                                  "undelivered" or "rejected"
%%                MessageUUID       A unique ID for the message
%%                ParentMessageUUID ID of the first part
%%                PartInfo          Specifies sequence information
%%      method The method used to call the url. Defaults to POST.
-spec send_message(Params::rest_api:params()) -> jsx:json_term().
send_message(Params) -> rest_api:api(post, "Message/", Params).
