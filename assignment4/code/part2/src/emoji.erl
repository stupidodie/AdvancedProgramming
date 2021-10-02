-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).
-compile[debug,export_all].
-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).


start(Initial) -> 
    ShortCodeList=lists:map(fun(Emoji)->case Emoji of {ShortCode, _}->ShortCode end end, Initial),
    case judgeListNotDuplicate(ShortCodeList)  of
        false->{error, "Duplicated shortcodes!"};
        true->
            try 
                E=spawn(fun()->loop() end),
                {ok,E}
            catch 
                _:Reason->{error, Reason}
            end
    end.

new_shortcode(_, _, _) -> not_implemented.

alias(_, _, _) -> not_implemented.

delete(_, _) -> not_implemented.

lookup(_, _) -> not_implemented.

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(_) -> not_implemented.

judgeListNotDuplicate(ShortCodeList)->
    case ShortCodeList of
        []->true;
        [_|[]] -> true;
        [Front|Rest] ->
            case lists:member(Front, Rest) of
                true -> false;
                false -> judgeListNotDuplicate(Rest)
            end
    end.
loop()->hello.