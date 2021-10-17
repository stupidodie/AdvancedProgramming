-module(async).
-behaviour(gen_statem).
-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).
-export([callback_mode/0,init/1]).
-export([calculate/3,handle_common/3]).
new(Fun, Arg) -> 
    gen_statem:start({local,?MODULE}, ?MODULE, [{Fun,Arg}], []).
wait(Aid) -> 
    case gen_statem:call(Aid, ask) of
        nothing->wait(Aid);
        {ok,Res}->Res;
        {exception, Ex}->throw(Ex)
    end.
poll(Aid) -> gen_statem:call(Aid, ask).
wait_catch(Aid) -> 
    case gen_statem:call(Aid, ask) of
        nothing -> wait_catch(Aid);
        {ok, Res} -> {ok, Res};
        {exception, Ex} -> {exception, Ex}
    end.
wait_any(Aids) -> nope.

-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).
handle_common({call,From},ask,Data) ->
    {keep_state, Data,
     [{reply,From,Data}]}.
calculate(cast,{exception,Reason},_) ->
    {keep_state,{exception,Reason}};
calculate(cast,{ok,Result},_) ->
    {keep_state,{ok,Result}};
    ?HANDLE_COMMON.

init({Fun,Arg}) ->
    spawn(fun()->
        try
            Result=Fun(Arg),
            gen_statem:cast(?MODULE, {ok,Result})
        catch
            _:Reason->gen_statem:cast(?MODULE, {exception,Reason})
        end
    end),
    {ok,calculate,nothing}.
callback_mode()->state_function.