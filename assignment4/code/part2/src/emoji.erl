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
                E=spawn(fun()->loop(Initial) end),
                {ok,E}
            catch 
                _:Reason->{error, Reason}
            end
    end.

new_shortcode(E, Short, Emo) -> 
    E!{self(),new_shortcode,Short,Emo},
    receive
        {E,ok}->ok;
        {E,error,Reason}->{error, Reason}
    end.

alias(E,Short1, Short2) -> 
    E!{self(),alias,Short1,Short2},
    receive
        {E,ok}->ok;
        {E,error,Reason}->{error, Reason}
    end.

delete(E, Short) -> 
    E!{self(),delete,Short},
    ok.

lookup(E, Short) ->
    E!{self(),lookup,Short}, 
    receive
        {E,ok,Emo}->{ok,Emo};
        {E,no_emoji}->no_emoji
    end.

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(E) -> 
    E!{self(),stop},
    receive 
        {E,ok}->ok;
        {E,error,Reason}->{error, Reason}
    end.

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
loop(EmojiList)->
    ShortCodeList=lists:map(fun(Emoji)->case Emoji of {ShortCode, _}->ShortCode end end, EmojiList),
    receive
        {From,new_shortcode,Short,Emo}->
            case lists:member(Short,ShortCodeList) of
                true-> From!{self(),{error, "Can't register the same shortcode more than once"}},loop(EmojiList);
                false-> From!{self(),ok},loop([{Short,Emo}|EmojiList])
            end;
        {From,alias,Short1,Short2}->
            case lists:member(Short1,ShortCodeList) of
                false-> From!{self(),{error, "The Short1 is not registered!"}},loop(EmojiList);
                true->
                    case lists:member(Short2,ShortCodeList) of
                        true->From!{self(),{error, "The Short2 is already registered!"}},loop(EmojiList);
                        false->From!{self(),ok},loop(aliasShort(Short1,Short2,EmojiList))
                    end
            end;
        {_,delete,Short}->
            case lists:member(Short,ShortCodeList) of
                false->loop(EmojiList);
                true->loop(deleteValue(searchALLAlias([Short],EmojiList),EmojiList))
            end;
        {From,lookup,Short}->
            case lists:member(Short,ShortCodeList) of
                false->From!{self(),no_emoji},loop(EmojiList);
                true->From!{self(),{ok,lookupValue(Short,EmojiList)}},loop(EmojiList)
            end;
        {From,stop}->From!{self(),ok}
    end.
lookupValue(Short,List)->
    case List of
        []->[];
        [{Short1,Emo}|Rest]->
            case Short1==Short of
                true->
                    case Emo of
                        {alias,Short2}->lookupValue(Short2,List);
                        _->Emo
                    end;
                false-> lookupValue(Short,Rest)
            end
    end.
aliasShort(Short1,Short2,List)->
    case List of
        []->[];
        [{Short,Emo}|Rest]->
            case Short==Short1 of
                true->[{Short,Emo}|[{Short2,{alias,Short1}}|Rest]];
                false->[{Short,Emo}|aliasShort(Short1,Short2,Rest)]
            end
    end.
searchALLAlias(AliasList,List)->
    case List of
        []->AliasList;
        [{Short1,Emo}|Rest]->
            case lists:member(Short1,AliasList) of
                true-> 
                    case Emo of
                        {alias,Short2}->
                            case lists:member(Short2,AliasList) of
                                true->searchALLAlias(AliasList,Rest);
                                false->searchALLAlias([Short2|AliasList],List)
                            end;
                        _->searchALLAlias(AliasList,Rest)
                    end;
                false-> 
                    case Emo of
                        {alias,Short2}->
                            case lists:member(Short2,AliasList) of
                                true->searchALLAlias([Short1|AliasList],List);
                                false->searchALLAlias(AliasList,Rest)
                            end;
                        _->searchALLAlias(AliasList,Rest)
                    end
            end
    end.
deleteValue(AliasList,List)-> 
    case List of
        []->[];
        [{Short1,Emo}|Rest]->
            case lists:member(Short1,AliasList) of
                true->Rest;
                false->
                    case Emo of
                        {alias,Short2}->
                            case lists:member(Short2,AliasList) of
                                true->deleteValue(AliasList,Rest);
                                false->[{Short1,Emo}|deleteValue(AliasList,Rest)]
                            end;
                        _->[{Short1,Emo}|deleteValue(AliasList,Rest)]
                    end
            end
    end.
