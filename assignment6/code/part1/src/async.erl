-module(async).

-behaviour(gen_server).

-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).
-export([init/1]).
-export([handle_cast/2, handle_call/3]).
-export([terminate/2]).

new(Fun, Arg) ->
    case gen_server:start(?MODULE, {Fun, Arg}, []) of
        {ok, Aid} ->
            Aid
    end.

wait(Aid) ->
    case gen_server:call(Aid, wait) of
        {ok, Res} ->
            Res;
        {exception, Ex} ->
            throw(Ex)
    end.

poll(Aid) ->
    gen_server:call(Aid, poll).

wait_catch(Aid) ->
    gen_server:call(Aid, wait).

wait_any(Aids) ->
    Work =
        fun(X) ->
           Worker = self(),
           lists:foreach(fun(Aid) ->
                            spawn(fun() ->
                                     Result = gen_server:call(Aid, wait),
                                     Worker ! {Aid, Result}
                                  end)
                         end,
                         X),
        receive 
           {Aid, Res} -> {Aid, Res} 
        end
    end,
    Async = new(Work, Aids),
    case wait(Async) of
        {Aid,{ok,Result}}->{Aid,Result};
        {_,{exception,Reason}}->throw(Reason)
    end.

init({Fun, Arg}) ->
    Me = self(),
    spawn(fun() ->
             try
                 Result = Fun(Arg),
                 gen_server:cast(Me, {ok, Result})
             catch
                 Reason -> gen_server:cast(Me, {exception, Reason})
             end
          end),
    {ok, []}.

handle_call(poll, _From, State) ->
    case State of
        {ok, _} ->
            {reply, State, State};
        {exception, _} ->
            {reply, State, State};
        _ ->
            {reply, nothing, State}
    end;
handle_call(wait, From, State) ->
    case State of
        {ok, _} ->
            {reply, State, State};
        {exception, _} ->
            {reply, State, State};
        _ ->
            {noreply, [From | State]}
    end.

handle_cast(NewState, State) ->
    case State of
        [] ->
            {noreply, NewState};
        _ ->
            lists:foreach(fun(From) -> gen_server:reply(From, NewState) end, State),
            {noreply, NewState}
    end.

terminate(_Reason, _State) ->
    ok.
