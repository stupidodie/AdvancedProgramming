-module(async).

-export([new/2, wait/1, poll/1,myTest/0]).

new(Fun, Arg) -> 
    Me = self(), 
    spawn(fun()->
    try 
        Res=Fun(Arg),
        Me!{self(),{ok,Res}},
        self()
    catch
        _:Reason->Me!{self(),Reason}
    end
end).

wait(Aid) -> 
    case poll(Aid) of 
        nothing -> 
            io:format("Still waiting"),
            wait(Aid);
        ResOrException -> ResOrException
    end.
        
poll(Aid) -> 
    receive 
        {Aid,{ok, Res}} -> io:format("~w~n",[{ok, Res}]), {ok, Res};
        {Aid,Reason} -> {exception,Reason }
    after 0 
        -> nothing 
    end.

myTest()->
    Aid=new(fun(X)->timer:sleep(200), X+1 end,1),
    io:format("Aid is ~p\n",[Aid]),
    io:format("run the poll\n"),
    wait(Aid),
    Aid2=new(fun(X)->throw("das") end,233),
    io:format("Aid2 is ~p\n",[Aid2]),
    io:format("run the poll\n"),
    wait(Aid2).