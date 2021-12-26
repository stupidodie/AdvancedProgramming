-module(test_rps).
-export([test_all/0]).
%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").


test_all() ->
    eunit:test(
      [
       start_broker(),
       test_statistics(),
       test_move()
      ], [verbose]).


move_one({Coordinator, Choice}) ->
  rps:move(Coordinator, Choice).
    apply_player(_Me, BrokerRef, Name, Round, Choices) -> 
    case rps:queue_up(BrokerRef, Name, Round) of
        {ok, _Player, Coordinator} -> lists:map(fun move_one/1, 
            [{Coordinator, Choice}|| Choice <- Choices]);
        _ -> server_stopping
    end.


start_broker() ->
    {"Start a broker, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, rps:start())
     end}.
    test_statistics() ->
      {"test statistics",
        fun() ->
            {ok, BrokerRef} = rps:start(),
            spawn(fun() -> apply_player(self(), BrokerRef, "P1", 7, 
                [rock, paper, scissors, rock, rock, scissors, scissors, paper, paper, rock, paper]) end),
            spawn(fun() -> apply_player(self(), BrokerRef, "P2", 7, 
                [rock, paper, scissors, paper, scissors, paper, rock, scissors, rock, paper, rock]) end),
            spawn(fun() -> apply_player(self(), BrokerRef, "P3", 33, 
                [rock, paper, scissors, rock, rock, scissors, scissors, paper, paper, rock, paper]) end),
            spawn(fun() -> apply_player(self(), BrokerRef, "P4", 33, 
                [rock, paper, scissors, paper, scissors, paper, rock, scissors, rock, paper, rock]) end),
            timer:sleep(100),
            Statistics_Res = rps:statistics(BrokerRef),
            ?assertMatch({ok, 11, 0, 1}, Statistics_Res)
        end}.
test_move()->
  {"always making the rock move",
  fun() ->
    {ok,Broker}=rps:start(),
    spawn(fun()->{ok, _, Coor} = rps:queue_up(Broker, "Rock bot2(tom)", 1),
      rock_to_game_over(Coor, rock,666)
  end),
    {ok, _, Coor} = rps:queue_up(Broker, "Rock bot(tom)", 1),
    rock_to_game_over(Coor, rock, 665),
    {ok,Me,SomeLoser}=rock_to_game_over(Coor, paper,1),
    ?assertEqual(1, Me),
    ?assertEqual(0, SomeLoser),
    ?assertEqual({ok,666,0,0}, rps:statistics(Broker)),
    ?assertEqual({ok,666,0,0}, rps:statistics(Broker)),
    rps:drain(Broker, self(), aa)
  end 
  }.

rock_to_game_over(Coor,Choice,N) ->
  case rps:move(Coor, Choice) of
      {game_over, Me, SomeLoser} ->
          {ok, Me, SomeLoser};
      server_stopping ->
          server_stopping;
      Result->if N-1>0 ->rock_to_game_over(Coor, Choice,N-1);
    true->Result
      end
  end.
