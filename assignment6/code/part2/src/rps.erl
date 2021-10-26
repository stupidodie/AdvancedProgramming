-module(rps).
-behaviour(gen_statem).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).
-export([init/1,broker/3]).
-export([find_group/2]).
-export([callback_mode/0]).
-export([init_coor/3,one_choice/3,end_of_state/3,server_stopping/3]).


start() -> gen_statem:start(?MODULE, init_broker, []).

queue_up(BrokerRef,Name, Rounds) ->  gen_statem:call(BrokerRef,{queue_up,Name,Rounds}).

move(Coordinator, Choice) -> gen_statem:call(Coordinator, {move,Choice}).

statistics(BrokerRef) -> gen_statem:call(BrokerRef,ask).

drain(BrokerRef, none, _) ->  spawn(fun()->gen_statem:call(BrokerRef,drain) end);
drain(BrokerRef, Pid, Msg) ->  spawn(fun()->gen_statem:call(BrokerRef,drain),Pid!Msg end) .

init_coor({call,From},drain,Data)->
    {_From1,_Name1,_From2,_Name2,CurrentRound,_Rounds,_Name1Wins,_Name2Wins}=Data,
    gen_statem:reply(From,server_stopping),
    {next_state,server_stopping,CurrentRound};
init_coor({call,From},ask,Data)->
    {_From1,_Name1,_From2,_Name2,CurrentRound,_Rounds,_Name1Wins,_Name2Wins}=Data,
    gen_statem:reply(From, {ongoing,CurrentRound}),
    keep_state_and_data;
init_coor({call,From},{move,Choice},Data)->
    {From1,Name1,From2,Name2,CurrentRound,Rounds,Name1Wins,Name2Wins}=Data,
    {Pid,_}=From,
    {Pid1,_}=From1,
    {Pid2,_}=From2,
    case (Pid==Pid1) orelse (Pid==Pid2) of
        true-> case Pid1==Pid of
            true->{next_state,one_choice,{From,Name1,Choice,From2,Name2,CurrentRound,Rounds,Name1Wins,Name2Wins}};
            false->{next_state,one_choice,{From,Name2,Choice,From1,Name1,CurrentRound,Rounds,Name2Wins,Name1Wins}}
            end;
        false->{keep_state_and_data,[{reply,From,notmatch}]}
    end.

one_choice({call,From},drain,Data)->
    {From1,_Name1,_Choice1,_From2,_Name2,CurrentRound,_Rounds,_Name1Wins,_Name2Wins}=Data,
    gen_statem:reply(From,server_stopping), 
    gen_statem:reply(From1,server_stopping),
    {next_state,server_stopping,CurrentRound};
one_choice({call,From},ask,Data)->
    {_From1,_Name1,_Choice1,_From2,_Name2,CurrentRound,_Rounds,_Name1Wins,_Name2Wins}=Data,
    gen_statem:reply(From,{ongoing,CurrentRound}),
    keep_state_and_data;
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
        case judge(Choice1,Choice2) of
            win->
                NewName1Wins=Name1Wins+1,
                case NewName1Wins>(Rounds/2) of
                    false-> 
                        gen_statem:reply(From1,win),
                        gen_statem:reply(From, {loss,Choice1}),
                        {next_state,init_coor,{From1,Name1,From,Name2,UpdateRound,Rounds,NewName1Wins,Name2Wins}};
                    true-> 
                        gen_statem:reply(From1,{game_over,NewName1Wins,Name2Wins}),
                        gen_statem:reply(From,{game_over,Name2Wins,NewName1Wins}),
                        {next_state,end_of_state,UpdateRound}
                end;
            lose-> 
                NewName2Wins=Name2Wins+1,
                case NewName2Wins>(Rounds/2) of
                    false->
                        gen_statem:reply(From,win),
                        gen_statem:reply(From1, {loss,Choice2}),
                        {next_state,init_coor,{From1,Name1,From,Name2,UpdateRound,Rounds,Name1Wins,NewName2Wins}};
                    true-> 
                        gen_statem:reply(From1,{game_over,Name1Wins,NewName2Wins}),
                        gen_statem:reply(From,{game_over,NewName2Wins,Name1Wins}),
                        {next_state,end_of_state,UpdateRound}
                end;
            tie-> 
                gen_statem:reply(From1,tie),
                gen_statem:reply(From,tie),
                {next_state,init_coor,{From1,Name1,From,Name2,UpdateRound ,Rounds,Name1Wins,Name2Wins}}
        end
    end
end.

end_of_state({call,From},drain,Data)->
    gen_statem:reply(From, server_stopping),
    {next_state,server_stopping,Data};
end_of_state({call,From},ask,Data)->
    {keep_state_and_data,[{reply,From,{end_of_state,Data}}]};
end_of_state({call,_},_,_)->{keep_state_and_data,[postpone]}.
server_stopping({call,From},ask,Data)->gen_statem:reply(From,Data),keep_state_and_data;
server_stopping({call,From},{queue_up,_,_},_)->gen_statem:reply(From,server_stopping),keep_state_and_data;
server_stopping({call,From},{move,_},_)->gen_statem:reply(From,server_stopping),keep_state_and_data;
server_stopping({call,From},_,_)-> gen_statem:reply(From,server_stopping),keep_state_and_data.

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



broker({call,From},drain,Data)->
    CorList=find_coor_list(Data),
    LongestGame=find_longest_game(CorList),
    lists:map(fun(Queue)->gen_statem:reply(Queue, server_stopping)end,find_all_queue(Data)),
    lists:map(fun(Cor)->gen_statem:call(Cor,drain) end,CorList),
    {next_state,server_stopping,{ok,LongestGame,0,0},[{reply,From,server_stopping}]};
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
cal_ongoing([])->0;
cal_ongoing(CorList)->
    lists:sum(lists:map(fun(Cor)->case gen_statem:call(Cor,ask) of {end_of_state,_}->0; {ongoing,_}->1;_->0 end end,CorList)).
cal_inqueue(List)->
    case List of
        []->0;
        [{need_group,_,_,_}|Rest]->1+cal_inqueue(Rest);
        [_|Rest]->cal_inqueue(Rest)
    end.
find_coor_list(List)->
    case List of
        []->[];
        [{need_group,_,_,_}|Rest]->find_coor_list(Rest);
        [{grouped,_,_,_,_,_}|Rest]->find_coor_list(Rest); 
        [{coordinatorList,CorList}|_Rest]->CorList
    end.
find_longest_game([])->0;
find_longest_game(CorList)->
    % throw(lists:map(fun(Cor)->case gen_statem:call(Cor,ask) of {_,Rounds}->Rounds;Rounds->Rounds end end,CorList)),
    lists:max(lists:map(fun(Cor)->case gen_statem:call(Cor,ask) of {_,Rounds}->Rounds;Rounds->Rounds end end,CorList)).
find_all_queue(List)->
    case List of 
    []->[];
    [{need_group,From,_Name,_Rounds}|Rest]->[From|find_all_queue(Rest)];
    [_|Rest]->find_all_queue(Rest)
end.
find_group(Rounds,List)->
    case List of
        []->none;
        [{need_group,From,Name,Rounds1}|Rest]-> 
            if 
            Rounds == Rounds1 -> {From,Name,Rounds1}; 
            true->find_group(Rounds,Rest) 
            end;
        [{grouped,_,_,_,_,_}|Rest]->find_group(Rounds, Rest);
        [{coordinatorList,_}|Rest]->find_group(Rounds, Rest)
    end.
callback_mode()->state_functions.
init(init_broker)->{ok, broker,[]};
init({init_coor,From1,Name1,From,Name,Rounds})->{ok,init_coor,{From1,Name1,From,Name,0,Rounds,0,0}}.