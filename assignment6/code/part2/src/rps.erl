-module(rps).
-behaviour(gen_statem).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).
-export([init/1,broker/3]).
-export([find_group/2]).
-export([callback_mode/0]).
-export([init_coor/3,one_choice/3,terminate/3,end_state/3,server_stopping/3]).



start() -> gen_statem:start(?MODULE, init_broker, []).

queue_up(BrokerRef,Name, Rounds) -> gen_statem:call(BrokerRef,{queue_up,Name,Rounds}).

move(Coordinator, Choice) -> gen_statem:call(Coordinator, {move,Choice}).

statistics(BrokerRef) -> gen_statem:call(BrokerRef,ask).

drain(BrokerRef, none, _) ->  gen_statem:call(BrokerRef,drain);
drain(BrokerRef, Pid, Msg) ->  gen_statem:call(BrokerRef,drain) ,Pid!Msg.

init_coor({call,_From},drain,Data)->
    {next_state,server_stopping,Data};
init_coor({call,From},ask,Data)->
    {_From1,_Name1,_From2,_Name2,CurrentRound,_Rounds,_Name1Wins,_Name2Wins}=Data, 
    {keep_state_and_data,[{reply,From,{ongoing,CurrentRound}}]};
init_coor({call,From},{move,Choice},Data)->
    {From1,Name1,From2,Name2,CurrentRound,Rounds,Name1Wins,Name2Wins}=Data,
    {Pid,_}=From,
    {Pid1,_}=From1,
    {Pid2,_}=From2,
    case (Pid==Pid1) orelse (Pid==Pid2) of
        true->
            if Pid1==Pid ->{next_state,one_choice,{From1,Name1,Choice,From2,Name2,CurrentRound,Rounds,Name1Wins,Name2Wins}};
                true->{next_state,one_choice,{From2,Name2,Choice,From1,Name1,CurrentRound,Rounds,Name2Wins,Name1Wins}}
            end;
        false->{keep_state_and_data,[{reply,From,notmatch}]}
    end.
% init_coor({call,From},getData,Data)->{keep_state_and_data,[{reply,From,Data}]}.

one_choice({call,_},drain,Data)->
    {next_state,server_stopping,Data};
one_choice({call,From},ask,Data)->
    {_From1,_Name1,_Choice1,_From2,_Name2,CurrentRound,_Rounds,_Name1Wins,_Name2Wins}=Data,
    {keep_state_and_data,[{reply,From,{ongoing,CurrentRound}}]};
one_choice({call,From},{move,Choice2},Data)->
    {From1,Name1,Choice1,From2,Name2,CurrentRound,Rounds,Name1Wins,Name2Wins}=Data,
    {Pid,_}=From,
    {Pid1,_}=From1,
    {Pid2,_}=From2,
    case (Pid==Pid1) or (Pid==Pid2) of 
    false->{keep_state_and_data,[{reply,From,notmatch}]};
    true->
        case Pid==Pid2 of
        false->{keep_state_and_data, [postpone]};
        true->
        UpdateRound=CurrentRound+1,
        case UpdateRound == Rounds of
            false->
                case judge(Choice1,Choice2) of
                    win->
                        gen_statem:reply(From1,win),
                        gen_statem:reply(From2, {loss,Choice1}),
                        {next_state,init_coor,{From1,Name1,From2,Name2,UpdateRound,Rounds,[UpdateRound|Name1Wins],Name2Wins}};
                    lose->
                        gen_statem:reply(From2,win),
                        gen_statem:reply(From1, {loss,Choice1}),
                        {next_state,init_coor,{From1,Name1,From2,Name2,UpdateRound,Rounds,Name1Wins,[UpdateRound|Name2Wins]}};
                    tie->
                        gen_statem:reply(From1,tie),
                        gen_statem:reply(From2,tie),
                        {next_state,init_coor,{From1,Name1,From2,Name2,UpdateRound,Rounds,Name1Wins,Name2Wins}}
                end;
            true->
                case judge(Choice1,Choice2) of
                    win->
                        gen_statem:reply(From1,{game_over,lists:sort([UpdateRound|Name1Wins]),lists:sort(Name2Wins)}),
                        gen_statem:reply(From2,{game_over,lists:sort(Name2Wins),lists:sort([UpdateRound|Name1Wins])}),
                        {next_state,end_state,Rounds,[]};
                    tie->
                        gen_statem:reply(From1,{game_over,lists:sort(Name1Wins),lists:sort(Name2Wins)}),
                        gen_statem:reply(From2,{game_over,lists:sort(Name2Wins),lists:sort(Name1Wins)}),
                        {next_state,end_state,Rounds};
                    lose->
                        gen_statem:reply(From2,{game_over,lists:sort([UpdateRound|Name2Wins]),lists:sort(Name1Wins)}),
                        gen_statem:reply(From1,{game_over,lists:sort(Name1Wins),lists:sort([UpdateRound|Name2Wins])}),
                        {next_state,end_state,Rounds}
                end
        end
    end
end.


server_stopping({call,From},_,_)->{keep_state_and_data,[{reply,From,server_stopping}]}.
end_state({call,_From},drain,Data)->
    {next_state,server_stopping,Data};
end_state({call,From},ask,Data)->
    {keep_state_and_data,[{reply,From,{end_state,Data}}]};
end_state({call,From},_,Data)->{keep_state_and_data,[{reply,From,{end_state,Data}}]}.
terminate(_Reason, _State, _Data) ->
    ok.
judge(rock,paper)->lose;
judge(paper,rock)->win;
judge(paper,scissors)->lose;
judge(scissors,paper)->win;
judge(rock,scissors)->win;
judge(scissors,rock)->lose;
judge(rock,rock)->tie;
judge(paper,paper)->tie;
judge(scissors,scissors)->tie;
judge(rock,_)-> win;
judge(paper,_)->win;
judge(scissors,_)->win;
judge(_,rock)-> lose;
judge(_,paper)->lose;
judge(_,scissors)->lose.



broker({call,_},drain,Data)->
    CorList=find_coor_list(Data),
    lists:map(fun(Cor)->gen_statem:call(Cor,drain) end,CorList),
    {next_state,server_stopping,Data};
broker({call,From},ask,Data)->
    CorList=find_coor_list(Data),
    LongestGame=find_longest_game(CorList),
    InQueue=cal_inqueue(Data),
    Ongoing=cal_ongoing(CorList),
    {keep_state_and_data,[{reply,From,{ok,LongestGame,InQueue,Ongoing}}]};
broker({call,From},{queue_up,Name,Rounds},Data)->
    case find_group(Rounds,Data) of
        none->{keep_state,[{need_group,From,Name,Rounds}|Data]};
        {From1,Name1,Rounds1}->
            {ok,Coordinator}=gen_statem:start(?MODULE, {init_coor,From1,Name1,From,Name,Rounds}, []),
            gen_statem:reply(From1,{ok,Name,Coordinator}),
            gen_statem:reply(From,{ok,Name1,Coordinator}),
            {keep_state,[{grouped,From1,Name1,From,Name,Rounds}|lists:delete({need_group,From1,Name1,Rounds1},addcoor(Coordinator,Data))]}
    end.
addcoor(Coordinator,List)->
    case List of
        []->[{coordinatorList,[Coordinator]}];
        [{need_group,From,Name,Rounds}|Rest]->[{need_group,From,Name,Rounds}|addcoor(Coordinator,Rest)];
        [{grouped,From1,Name1,From,Name,Rounds}|Rest]->[{grouped,From1,Name1,From,Name,Rounds}|addcoor(Coordinator,Rest)];
        [{coordinatorList,CorList}|Rest]->[{coordinatorList,[Coordinator|CorList]}|Rest]
    end.
cal_ongoing(CorList)->
    lists:sum(lists:map(fun(Cor)->case gen_statem:call(Cor,ask) of {end_state,_}->0; {ongoing,_}->1 end end,CorList)).
cal_inqueue(List)->
    case List of
        []->0;
        [{need_group,_,_,_}|Rest]->1+cal_inqueue(Rest);
        [_|Rest]->cal_inqueue(Rest)
    end.
find_coor_list(List)->
    case List of
        []->[0];
        [{need_group,_,_,_}|Rest]->find_coor_list(Rest);
        [{grouped,_,_,_,_,_}|Rest]->find_coor_list(Rest); 
        [{coordinatorList,CorList}|_Rest]->CorList
    end.
find_longest_game(CorList)->
    lists:max(lists:map(fun(Cor)->{_,Rounds}=gen_statem:call(Cor,ask),Rounds end,CorList)).

find_group(Rounds,List)->
    case List of
        []->none;
        [{need_group,From,Name,Rounds1}|Rest]-> 
            if 
            Rounds == Rounds -> {From,Name,Rounds1}; 
            true->find_group(Rounds,Rest) 
            end;
        [{grouped,_,_,_,_,_}|Rest]->find_group(Rounds, Rest);
        [{coordinatorList,_}|Rest]->find_group(Rounds, Rest)
    end.
callback_mode()->state_functions.
init(init_broker)->{ok, broker,[]};
init({init_coor,From1,Name1,From,Name,Rounds})->{ok,init_coor,{From1,Name1,From,Name,0,Rounds,[],[]}}.

