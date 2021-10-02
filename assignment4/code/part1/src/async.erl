-module(async).
% -compile[debug,export_all].
-export([new/2, wait/1, poll/1]).

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
        nothing -> wait(Aid);
        {ok, Res}->Res;
        {exception,Reason }->throw(Reason)
    end.
        
poll(Aid) -> 
    receive 
        {Aid,{ok, Res}} -> self()!{Aid,{ok,Res}},{ok, Res};
        {Aid,Reason} -> self()!{Aid,Reason},{exception,Reason }
    after 0 
        -> nothing 
    end.

% myTest()->
%     A = new(fun(X) -> X end, 54),
%     54 =:= wait(A),
%     wait(A) =:= wait(A). 