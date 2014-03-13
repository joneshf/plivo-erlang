## plivo-erlang

Erlang helper library for Plivo API.

### Usage

This is a work in progress, at the moment there is only documentation for usage from the `erl` shell.

Requires rebar to set up dependencies.

* Clone this repo

```
$ git clone https://github.com/joneshf/plivo-erlang.git
```

* Grab the dependencies

```
$ rebar get-deps
```

* Compile

```
$ rebar compile
```

* Open up erl and set up your authorization.

```erlang
$ erl -pa ebin deps/*/ebin

1> application:start(plivo).
ok
2> rest_api:set_auth_id("Your auth id").
ok
3> rest_api:set_auth_token("Your auth token").
ok
```

At this point you're free to start using the `rest_api` to interact with Plivo's API.
