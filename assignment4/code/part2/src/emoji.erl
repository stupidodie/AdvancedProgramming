-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).
-compile[debug,export_all].
-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).


start(Initial) -> 
    ShortCodeList=
        lists:map(fun(Emoji)->case Emoji of {ShortCode, _}->ShortCode end end, Initial),
    case judgeListNotDuplicate(ShortCodeList)  of
        false->{error, "Duplicated shortcodes!"};
        true->
            try 
                E=spawn(fun()->loop(convertList(Initial)) end),
                {ok,E}
            catch 
                _:Reason->{error, Reason}
            end
    end.

new_shortcode(E, Short, Emo) -> 
    E!{self(),new_shortcode,Short,Emo},
    receive
        {E,ok}->ok;
        {E,{error,Reason}}->{error, Reason}
    end.

alias(E,Short1, Short2) -> 
    E!{self(),alias,Short1,Short2},
    receive
        {E,ok}->ok;
        {E,{error,Reason}}->{error, Reason}
    end.

delete(E, Short) -> 
    E!{self(),delete,Short},
    ok.

lookup(E, Short) ->
    E!{self(),lookup,Short}, 
    receive
        {E,{ok,Emo}}->{ok,Emo};
        {E,no_emoji}->no_emoji
    end.

analytics(E, Short, Fun, Label, Init) -> 
    E!{self(),analytics,Short,Fun,Label,Init},
    receive
        {E,ok}->ok;
        {E,{error,Reason}}->{error, Reason}
    end.

get_analytics(E, Short) -> 
    E!{self(),get_analytics,Short},
    receive
        {E,{ok,Stat}}->{ok,Stat};
        {E,{error,Reason}}->{error, Reason}
    end.


remove_analytics(E, Short, Label) -> 
    E!{self(),remove_analytics,Short,Label}.

stop(E) -> 
    E!{self(),stop},
    receive 
        {E,ok}->ok;
        {E,{error,Reason}}->{error, Reason}
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
convertList(List)->
    case List of
        []->[];
        [{ShortCode, Emo}|Rest]->[{ShortCode, Emo,[ShortCode],[]}|convertList(Rest)]
    end.
loop(EmojiList)->
    ShortCodeList=
        lists:map(fun(Emoji)->case Emoji of {ShortCode, _,_,_}->ShortCode end end, EmojiList),
    receive
        {From,new_shortcode,Short,Emo}->
            case lists:member(Short,ShortCodeList) of
                true-> From!{self(),{error, "Can't register the same shortcode more than once"}},loop(EmojiList);
                false-> From!{self(),ok},loop([{Short,Emo,[Short],[]}|EmojiList])
            end;
        {From,alias,Short1,Short2}->
            case lists:member(Short1,ShortCodeList) of
                false-> From!{self(),{error, "The Short1 is not registered!"}},loop(EmojiList);
                true->
                    case lists:member(Short2,ShortCodeList) of
                        true->From!{self(),{error, "The Short2 is already registered!"}},loop(EmojiList);
                        false->From!{self(),ok},
                        loop(aliasShort(findOriginalName(Short1,EmojiList),Short2,EmojiList))
                    end
            end;
        {_,delete,Short}->
            case lists:member(Short,ShortCodeList) of
                false->loop(EmojiList);
                true->loop(deleteValue(searchALLAliasAndSelf(findOriginalName(Short,EmojiList),EmojiList),EmojiList))
            end;
        {From,lookup,Short}->
            case lists:member(Short,ShortCodeList) of
                false->From!{self(),no_emoji},loop(EmojiList);
                true->
                    try NewEmojiList=callAnalysis(findOriginalName(Short,EmojiList),EmojiList),
                        From!{self(),{ok,lookupValue(findOriginalName(Short,EmojiList),EmojiList)}},
                     loop(NewEmojiList)
                    catch _:Reason->From!{self(),{error, Reason}}
                end
                    
            end;
        {From,stop}->From!{self(),ok};
        {From,analytics,Short,Fun,Label,Init}->
            case lists:member(Short,ShortCodeList) of
                false->From!{self(),{error, "The Short is not registered!"}},loop(EmojiList);
                true-> case checkLabelAvailabel(findOriginalName( Short,EmojiList),Label,EmojiList) of
                    false->From!{self(),{error, "The Label is already registered!"}},loop(EmojiList);
                    true->From!{self(),ok},loop(addAnalysis(findOriginalName(Short,EmojiList),Fun,Label,Init,EmojiList))
                end
            end;
        {From,get_analytics,Short}->
            case lists:member(Short,ShortCodeList) of
                false->From!{self(),{error, "The Short is not registered!"}},loop(EmojiList);
                true->From!{self(),{ok,getAnalysis(findOriginalName(Short,EmojiList),EmojiList)}},loop(EmojiList)
            end;
        {_,remove_analytics,Short,Label}->
            case lists:member(Short,ShortCodeList) of
                false->loop(EmojiList);
                true->loop(removeAnalysis(findOriginalName(Short,EmojiList),Label,EmojiList))
            end;
        {From,_,_}->From!{self(),{error, "Unknown command!"}},loop(EmojiList)
    end.
removeAnalysis(Short,Label,List)->
    case List of 
        []->[];
        [{ShortCode, Emo, Alias, FunList}|Rest]->
            case ShortCode==Short of
                true->[{ShortCode, Emo, Alias, removeLabel(Label,FunList)}|Rest];
                false->[{ShortCode, Emo, Alias, FunList}|removeAnalysis(Short,Label,Rest)]
            end
    end.
removeLabel(Label,FunList)->
    case FunList of
        []->[];
        [{Fun1,Label1, State1}|Rest]->
            case Label1==Label of
                true->Rest;
                false->[{Fun1,Label1, State1}|removeLabel(Label,Rest)]
            end
end.
getAnalysis(Short,List)->
    case List of
        []->[];
        [{ShortCode,_,_,FunList}|Rest]->
            case Short==ShortCode of
                true->getStat(FunList);
                false->getAnalysis(Short,Rest)
            end
    end.
getStat(FunList)->
    case FunList of
        []->[];
        [{_,Label,State}|Rest]->[{Label,State}|getStat(Rest)]
    end.
findOriginalName(Short,List)->
    case List of
        []->[];
        [{ShortCode, Emo,Alias,_}|Rest]->
            case ShortCode==Short of
                false->findOriginalName(Short,Rest);
                true-> 
                    case Emo of
                        alias->Alias;
                        _->Short
                    end
             end
end.
addAnalysis(Short,Fun,Label,Init,EmojiList)->
    case EmojiList of
        []->[];
        [{ShortCode,Emo,Alias,FunList}|Rest]->
            case ShortCode==Short of
                false->[{ShortCode, Emo,Alias,FunList}|addAnalysis(Short,Fun,Label,Init,Rest)];
                true->[{ShortCode, Emo,Alias,[{Fun,Label,Init}|FunList]}|Rest]
            end
    end.

checkLabelAvailabel(Short,Label,List)->
    case List of
        []->[];
        [{Short1,_,_,FunList}|Rest]->
            case Short1==Short of
                false->checkLabelAvailabel(Short,Label,Rest);
                true-> 
                    case checkLableInFunList(Label,FunList) of
                            true->false;
                            false->true
                    end
            end
    end.
checkLableInFunList(Label,FunList)->
    case FunList of
        []->false;
        [{_,Label1,_}|Rest]->
            case Label1==Label of
                true->true;
                false->checkLableInFunList(Label,Rest)
            end
    end.
lookupValue(Short,List)->
    case List of
        []->[];
        [{Short1,Emo,_,_}|Rest]->
            case Short1==Short of
                true->Emo;
                false-> lookupValue(Short,Rest)
            end
    end.
callAnalysis(Short,List)->
    case List of
        []->[];
        [{Short1,Emo,Alias,FunList}|Rest]->
            case Short1==Short of
                true->[{Short1,Emo,Alias,applyFunction(Short,FunList)}|Rest];
                false-> [{Short1,Emo,Alias,FunList}|callAnalysis(Short,Rest)]
            end
    end.
applyFunction(Short,FunList)->
    case FunList of
        []->[];
        [{Fun,Label,State}|Rest]->[{Fun,Label,Fun(Short,State)}|applyFunction(Short,Rest)]
    end.
aliasShort(Short1,Short2,List)->
    case List of
        []->[];
        [{Short,Emo,Alias,FunList}|Rest]->
            case Short==Short1 of
                false->[{Short,Emo,Alias,FunList}|aliasShort(Short1,Short2,Rest)];
                true->[{Short,Emo,[Short2|Alias],FunList}|[{Short2,alias,Short,[]}|Rest]]
            end
    end.
searchALLAliasAndSelf(Short,List)->
    case List of
        []->[];
        [{Short1,_,Alias,_}|Rest]->
           case Short1==Short of
                false->searchALLAliasAndSelf(Short,Rest);
                true->Alias
            end
    end.
deleteValue(AliasList,List)-> 
    case List of
        []->[];
        [{Short,Emo,Alias,FunList}|Rest]->
            case lists:member(Short,AliasList) of
                true->deleteValue(AliasList,Rest);
                false->[{Short,Emo,Alias,FunList}|deleteValue(AliasList,Rest)]
            end
    end.
