%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2016 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc Application error_logger backend: it use erlang error_logger
%%%      to log sent email.
%%% @end

-module(pushmail_backend_gen_smtp).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(pushmail_backend).

-export([start/0, start/1, stop/1, send/2, wait_reply/1]).

-spec start() ->
  {ok, pushmail:appctx()} | {error: term()}.
start() ->
  {ok, AppCtx} = application:get_env(pushmail, pushmail_backend_gen_smtp),
  start(AppCtx).

-spec start(pushmail:appctx()) ->
  {ok, pushmail:appctx()} | {error: term()}.
start(AppCtx) ->
  start_ssl_if_needed(AppCtx),
  {ok, AppCtx}.

-spec stop(pushmail:appctx()) -> ok | {error: term()}.
stop(_) ->
  ok.

%% @doc Send a email (in appctx you can pass parameters)
-spec send(pushmail:mail(), pushmail:appctx()) ->
  {ok, pushmail:appctx()} | {error, term()}.
send(#{sender := Sender, receivers := Receivers}=Mail, AppCtx) ->
  EncodedEmail = encode_body(Mail),
  gen_smtp_client:send({Sender, Receivers, EncodedEmail}, AppCtx, wait_reply),
  {ok, AppCtx}.

-spec encode_body(pushmail:mail()) -> binary().
encode_body(#{sender := Sender, receivers := Receivers, subject := Subject,
              message := Message}) ->
  From = {<<"From">>, Sender},
  To = {<<"To">>, binary_join(Receivers, <<", ">>)},
  EmailSubject = {<<"Subject">>, Subject},
  mimemail:encode(
    {<<"text">>, <<"plain">>, [From, To, EmailSubject], [], Message}).


-spec binary_join(list(), binary() | string()) -> binary().
binary_join([], _Separator) -> <<"">>;
binary_join([Binary], _Separator) when is_binary(Binary) -> Binary;
binary_join(Binary, _Separator) when is_binary(Binary) ->
  Binary;
binary_join([Head | Tail], Separator) ->
  lists:foldl(fun(Value, Acc) ->
                 << Acc/binary, Separator/binary, Value/binary >>
             end, Head, Tail).

-spec wait_reply(
        {ok, binary()} | {error, atom(), any()} | {exit, any()}) -> ok.
wait_reply({ok, Receipt}) ->
  error_report:info_msg("Email succesfully sent: ~p~n", [Receipt]);
wait_reply({error, Type, Message}) ->
  error_logger:error_report({
    {module, pushmail},
    {plugin, backend_gen_smtp},
    {error, [
       {type, Type},
       {reason, Message}
    ]}
  });
wait_reply({exit, ExitReason}) ->
  error_logger:error_report({
    {module, pushmail},
    {plugin, backend_gen_smtp},
    {error, ExitReason}
  }).

start_ssl_if_needed(AppCtx) ->
  case proplists:get_value(ssl, AppCtx) of
    undefined -> ok;
    false -> ok;
    true -> application:ensure_all_started(ssl)
  end.
