pushmail backend gen_smtp
=========================

A plugin for PushMail to send the email with
[gen_smtp](https://github.com/Vagabond/gen_smtp).

Configuration
-------------

To configure for example the plugin to send emails.

```erlang
[
  {pushmail, [
    {backend, pushmail_backend_gen_smtp}
    {backend_gen_smtp, [
      {ssl, true},
      {tls, if_available}, % always, never, if_available
      {auth, always},
      {hostname, "127.0.0.1"},
      {retries, 1}, % how many retries per smtp host on temporary failure
      {relay, "smtp.server.com"},
      {username, "user@server.com"},
      {password, "mypassword"}
    ]}
  ]}
]
```


Usage
-----

Start `pushmail`.
You can directly the backend configuration calling `pushmail:start/1`.
If you don't specify any argument, the backend load configuration from
`.config`.

```erlang
application:ensure_all_started(pushmail).
{ok, AppCtx} = pushmail:start().
```

After that, you are ready to send email with:

```erlang
Mail = #{
  sender => <<"Sender <test@mydomain.org>">>,
  receivers => [<<"Receiver1 <receiver@otherdomain.org>">>],
  subject => <<"My subject">>,
  message => <<"My message!">>,
  headers => {}
},
{ok, NewAppCtx} = pushmail:send(Mail, AppCtx).
```

To stop the application:

```erlang
pushmail:stop(NewAppCtx).
```

Build
-----

    $ ./utils/rebar3 compile
