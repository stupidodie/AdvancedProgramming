-module(test_rps).
-export([test_all/0]).
-export([rock_to_game_over/2]).
-compile([debug,export_all]).
%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").


test_all() ->
    eunit:test(
      [
       start_broker(),
       test_move()
      ], [verbose]).


start_broker() ->
    {"Start a broker, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, rps:start())
     end}.
test_move()->
  {"always making the rock move",
  fun() ->
    {ok,Broker}=rps:start(),
    spawn(fun()->{ok, _, Coor} = rps:queue_up(Broker, "Rock bot2(tom)", 1),
    rps:move(Coor,rock) end),
    {ok, Name, Coor} = rps:queue_up(Broker, "Rock bot(tom)", 1),
    % ?assertEqual("Rock bot2(tom)", Name),
    % Result=rps:statistics(Broker),
    % gen_statem:call(Coor, getData),
    {game_over,Me,SomeLoser}=rps:move(Coor, paper),
    % ?assertEqual("Rock bot2(tom)", rps:statistics(Broker))

    ?assertEqual(1, Me),
    ?assertEqual(0, SomeLoser)
  end 
  }.

rock_to_game_over(Coor,Choice) ->
  case rps:move(Coor, Choice) of
      {game_over, Me, SomeLoser} ->
          {ok, Me, SomeLoser};
      server_stopping ->
          server_stopping;
      notmatch->
        {ok,notmatch,notmatch};
      _ -> rock_to_game_over(Coor,Choice)
  end.

