-module(emoji).
% -compile[denug,export_all].
-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).
-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

-spec start(Initial::list())-> any() .
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

-spec new_shortcode(E::pid(),Short::shortcode(),Emo::emoji()) ->any().
new_shortcode(E, Short, Emo) -> 
    E!{self(),new_shortcode,Short,Emo},
    receive
        {E,ok}->ok;
        {E,{error,Reason}}->{error, Reason}
    end.

-spec alias(E::pid(),Short::shortcode(),Emo::emoji()) ->any().
alias(E,Short1, Short2) -> 
    E!{self(),alias,Short1,Short2},
    receive
        {E,ok}->ok;
        {E,{error,Reason}}->{error, Reason}
    end.


-spec delete(E::pid(),Short::shortcode()) ->any().
delete(E, Short) -> 
    E!{self(),delete,Short},
    ok.

-spec lookup(E::pid(),Short::shortcode()) ->any().
lookup(E, Short) ->
    E!{self(),lookup,Short}, 
    receive
        {E,{ok,Emo}}->{ok,Emo};
        {E,no_emoji}->no_emoji
    end.

-spec analytics(E::pid(),Short::shortcode(),Fun::analytic_fun(State::any()),Label::string(),State::any()) ->any().
analytics(E, Short, Fun, Label, Init) -> 
    E!{self(),analytics,Short,Fun,Label,Init},
    receive
        {E,ok}->ok;
        {E,{error,Reason}}->{error, Reason}
    end.

-spec get_analytics(E::pid(),Short::shortcode()) ->any().
get_analytics(E, Short) -> 
    E!{self(),get_analytics,Short},
    receive
        {E,{ok,Stat}}->{ok,Stat};
        {E,{error,Reason}}->{error, Reason}
    end.

-spec remove_analytics(E::pid(),Short::shortcode(),Label::string()) ->any().
remove_analytics(E, Short, Label) -> 
    E!{self(),remove_analytics,Short,Label}.

-spec stop(E::pid()) ->any().
stop(E) -> 
    E!{self(),stop},
    receive 
        {E,ok}->ok;
        {E,{error,Reason}}->{error, Reason}
    end.

-spec judgeListNotDuplicate(List::list()) ->boolean().
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

%% The List after converted is a list of {ShortCode,Emo,Alias,FunList}
%% The Emo is either a binary or an atom alias (when it is an alias)
%% The Alias is a list of itself and its alias
%% The FunList is a list of  {Fun, Lable, State}
%% Example:
%% The First Emoji List: [{"smiley", <<240, 159, 152, 131>>, ["smiley"], []}]
%% Add an alias SS to the Emoji "smiley":
%% [{"smiley", <<240, 159, 152, 131>>, ["smiley","SS"], []}, 
%% {"SS", alias, "smiley", []}]
%% Add an analytics (fun(_, N) -> N+1 end, "Add", 0) to the Emoji "smiley":
%% [{"smiley", <<240, 159, 152, 131>>, ["smiley","SS"], [{un(_, N) -> N+1 end, "Add", 0]}, 
%% {"SS", alias, "smiley", []}]

-spec convertList(List::list()) ->list().
convertList(List)->
    case List of
        []->[];
        [{ShortCode, Emo}|Rest]->[{ShortCode, Emo,[ShortCode],[]}|convertList(Rest)]
    end.

-spec loop(list())->any().
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
                    From!{self(),{ok,lookupValue(findOriginalName(Short,EmojiList),EmojiList)}},
                    NewEmojiList=callAnalysis(findOriginalName(Short,EmojiList),EmojiList,Short),
                    loop(NewEmojiList)
                %%  The Old handle Code...
                    % Me=self(),
                    % process_flag(trap_exit,true),
                    % Worker=spawn_link(fun()->
                    %         NewEmojiList=callAnalysis(findOriginalName(Short,EmojiList),EmojiList,Short),
                    %         Me!{self(),NewEmojiList} 
                    % end),
                    % NewState=
                    % receive
                    %     {Worker,NewEmojiList}->NewEmojiList;
                    %     {'EXIT', Worker, _}->EmojiList
                    % end,
                    %  loop(NewState)    
                %%  END LINE
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

-spec removeAnalysis(Short::shortcode(),Label::string(),List::list())->list().
removeAnalysis(Short,Label,List)->
    case List of 
        []->[];
        [{ShortCode, Emo, Alias, FunList}|Rest]->
            case ShortCode==Short of
                true->[{ShortCode, Emo, Alias, removeLabel(Label,FunList)}|Rest];
                false->[{ShortCode, Emo, Alias, FunList}|removeAnalysis(Short,Label,Rest)]
            end
    end.

-spec removeLabel(Short::shortcode(),List::list())->list().
removeLabel(Label,FunList)->
    case FunList of
        []->[];
        [{Fun1,Label1, State1}|Rest]->
            case Label1==Label of
                true->Rest;
                false->[{Fun1,Label1, State1}|removeLabel(Label,Rest)]
            end
end.
-spec getAnalysis(Short::shortcode(),List::list())->list().
getAnalysis(Short,List)->
    case List of
        []->[];
        [{ShortCode,_,_,FunList}|Rest]->
            case Short==ShortCode of
                true->getStat(FunList);
                false->getAnalysis(Short,Rest)
            end
    end.
-spec getStat(FunList::list())->list().
getStat(FunList)->
    case FunList of
        []->[];
        [{_,Label,State}|Rest]->[{Label,State}|getStat(Rest)]
    end.

-spec findOriginalName(Short::shortcode(),List::list()) ->Short::shortcode().
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
-spec addAnalysis(Short::shortcode(),Fun::analytic_fun(Init::any()),Label::string(),Init::any(),List::list())->list().
addAnalysis(Short,Fun,Label,Init,EmojiList)->
    case EmojiList of
        []->[];
        [{ShortCode,Emo,Alias,FunList}|Rest]->
            case ShortCode==Short of
                false->[{ShortCode, Emo,Alias,FunList}|addAnalysis(Short,Fun,Label,Init,Rest)];
                true->[{ShortCode, Emo,Alias,[{Fun,Label,Init}|FunList]}|Rest]
            end
    end.

-spec checkLabelAvailabel(Short::shortcode(),Label::string(),List::list())->boolean().
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
-spec checkLableInFunList(Label::string(),FunList::list())->boolean().
checkLableInFunList(Label,FunList)->
    case FunList of
        []->false;
        [{_,Label1,_}|Rest]->
            case Label1==Label of
                true->true;
                false->checkLableInFunList(Label,Rest)
            end
    end.
-spec lookupValue(Short::shortcode(),List::list())->any().
lookupValue(Short,List)->
    case List of
        []->[];
        [{Short1,Emo,_,_}|Rest]->
            case Short1==Short of
                true->Emo;
                false-> lookupValue(Short,Rest)
            end
    end.
-spec callAnalysis(Short::shortcode(),List::list(),OriginalShort::shortcode())->any().
callAnalysis(Short,List,OriginalShort)->
    case List of
        []->[];
        [{Short1,Emo,Alias,FunList}|Rest]->
            case Short1==Short of
                true->[{Short1,Emo,Alias,applyFunction(OriginalShort,FunList)}|Rest];
                false-> [{Short1,Emo,Alias,FunList}|callAnalysis(Short,Rest,OriginalShort)]
            end
    end.
-spec applyFunction(Short::shortcode(),FunList::list())->list().
applyFunction(Short,FunList)->
    case FunList of
        []->[];
        [{Fun,Label,State}|Rest]->
            try [{Fun,Label,Fun(Short,State)}|applyFunction(Short,Rest)]
        catch _:_->[{Fun,Label,State}|applyFunction(Short,Rest)]
    end
    end.
-spec aliasShort(Short1::shortcode(),Short2::shortcode(),List::list())->list().
aliasShort(Short1,Short2,List)->
    case List of
        []->[];
        [{Short,Emo,Alias,FunList}|Rest]->
            case Short==Short1 of
                false->[{Short,Emo,Alias,FunList}|aliasShort(Short1,Short2,Rest)];
                true->[{Short,Emo,[Short2|Alias],FunList}|[{Short2,alias,Short,[]}|Rest]]
            end
    end.
-spec searchALLAliasAndSelf(Short::shortcode(),List::list())->list().
searchALLAliasAndSelf(Short,List)->
    case List of
        []->[];
        [{Short1,_,Alias,_}|Rest]->
           case Short1==Short of
                false->searchALLAliasAndSelf(Short,Rest);
                true->Alias
            end
    end.
-spec deleteValue(AliasList::list(),List::list())->list().
deleteValue(AliasList,List)-> 
    case List of
        []->[];
        [{Short,Emo,Alias,FunList}|Rest]->
            case lists:member(Short,AliasList) of
                true->deleteValue(AliasList,Rest);
                false->[{Short,Emo,Alias,FunList}|deleteValue(AliasList,Rest)]
            end
    end.

