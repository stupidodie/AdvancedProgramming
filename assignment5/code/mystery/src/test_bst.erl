-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A non-symbolic generator for bst, parameterised by key and value generators

%% insert values in to a leaf

%The Original bst
bst(Key, Value) ->
 ?LET(KVS, eqc_gen:list({Key, Value}),%       lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
                 empty(),
                KVS)).
%bst()->
%    ?LAZY(
%       oneof([{call,bst,empty,[]},
%              ?LET(T,bst(),
%                   {call,bst,insert,[atom_key(),int_value(),T]})])
%        ).
% bst()->?SIZED(Size,subbst(Size)).
% subbst(0)->{call,bst,empty,[]};
% subbst(Size)->
% T=subbst(Size div 2),
% oneof([
%   subbst(0),
%   {call,bst,insert,[atom_key(),int_value(),T]}
% ]).

%% foldl(Fun, Acc0, List) -> Acc1
%% Types
%% Fun = fun((Elem :: T, AccIn) -> AccOut)
%% Acc0 = Acc1 = AccIn = AccOut = term()
%% List = [T]
%% T = term()
%% examples show as below:
%% lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]). -> 15
%% lists:foldl(fun(X, Prod) -> X * Prod end, 1, [1,2,3,4,5]). ->120


% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
prop_arbitrary_valid() ->
    ?FORALL(T, bst(),
            valid(eval(T))).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            valid (insert(K, V, eval(T)))).
% if the empty tree is valid
prop_empty_valid()->
    ?FORALL(T,empty(),valid(T)).
% if we delete into a valid tree it stays valid
prop_delete_valid()->
    ?FORALL({K,T},{atom_key(),bst()},
           valid(delete(K,eval(T)))).
% if we union two valid trees it stays valid
prop_union_valid()->
    ?FORALL({T1,T2},{bst(),bst()},
           valid(union(eval(T1),eval(T2)))).
%%% -- postcondition properties

prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst()},
            eqc:equals(find(K2, insert(K1, V, eval(T))),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, eval(T))
                       end)).


prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            eqc:equals(find(K, insert(K, V, eval(T))),
                       {found, V})).


prop_find_post_absent() ->
   % ∀ k t. find k (delete k t) === nothing
    ?FORALL({K,T},{atom_key(),bst()},
           eqc:equals(find(K,delete(K,eval(T))),nothing)).

prop_delete_post()->
    ?FORALL({K1,K2,T},{atom_key(),atom_key(),bst()},
           eqc:equals(find(K1,delete(K2,eval(T))),
                      case K1=:=K2 of
                          true-> nothing;
                          false->find(K1,eval(T))
                      end)).
prop_union_post()->
    %prop UnionPost t t′ k = find k (union t t′ ) === (find k t < | > find k t′ )
    ?FORALL({K,T1,T2},{atom_key(),bst(),bst()},
           eqc:equals(find(K,union(eval(T1),eval(T2))),
                     case find(K,eval(T1)) of
                         nothing->find(K,eval(T2));
                         {found,V}->{found,V}
                     end)).

%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            bst:size(insert(K, V, eval(T))) >= bst:size(eval(T))).



obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(),
             bst()},
            obs_equals(insert(K1, V1, insert(K2, V2, eval(T))),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, eval(T));
                           false -> insert(K2, V2, insert(K1, V1, eval(T)))
                       end)).

prop_insert_delete()->
    ?FORALL({K1,V1,K2,T},{atom_key(),int_value(),atom_key(),bst()},
           obs_equals(insert(K1,V1,delete(K2,eval(T))),
                      case K1=:=K2 of
                          true-> insert(K1,V1,eval(T));
                          false->delete(K2,insert(K1,V1,eval(T)))
                      end)).
prop_insert_union()->
    ?FORALL({K,V,T1,T2},{atom_key(),int_value(),bst(),bst()},
           obs_equals(insert(K,V,union(eval(T1),eval(T2))),union(insert(K,V,eval(T1)),eval(T2)))).

prop_delete_nil()->
    ?FORALL(K,atom_key(),eqc:equals(delete(K,empty()),empty())).
prop_delete_insert()->
    ?FORALL({K1,K2,V2,T},{atom_key(),atom_key(),int_value(),bst()},
           obs_equals(delete(K1,insert(K2,V2,eval(T))),
                     case K1=:=K2 of
                         true-> delete(K1,eval(T));
                         false->insert(K2,V2,delete(K1,eval(T)))
                     end)).
prop_delete_delete()->
    ?FORALL({K1,K2,T},{atom_key(),atom_key(),bst()},
           obs_equals(delete(K1,delete(K2,eval(T))),delete(K2,delete(K1,eval(T))))).
prop_delete_union()->
    ?FORALL({K,T1,T2},{atom_key(),bst(),bst()},
           obs_equals(delete(K,union(eval(T1),eval(T2))),union(delete(K,eval(T1)),delete(K,eval(T2))))).
prop_union_nil1()->
    ?FORALL(T,bst(),eqc:equals(union(empty(),eval(T)),eval(T))).
prop_union_nil2()->
    ?FORALL(T,bst(),eqc:equals(union(eval(T),empty()),eval(T))).
prop_union_delete_insert()->
    ?FORALL({T1,T2,K,V},{bst(),bst(),atom_key(),int_value()},
           obs_equals(union(delete(K,eval(T1)),insert(K,V,eval(T2))),insert(K,V,union(eval(T1),eval(T2))))).
prop_union_union_idem()->
    ?FORALL({T},{bst()},
           obs_equals(union(eval(T),eval(T)),eval(T))).
prop_union_union_assoc()->
    ?FORALL({T1,T2,T3},{bst(),bst(),bst()},
           eqc:equals(union(union(eval(T1),eval(T2)),eval(T3)),union(eval(T1),union(eval(T2),eval(T3))))).
%%% -- Model based properties
model(T) -> to_sorted_list(T).


prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst()},
            equals(model(insert(K, V,eval(T))),
                   sorted_insert(K, V, delete_key(K, model(eval(T)))))).

prop_nil_model()->eqc:equals(model(empty()),[]).

prop_delete_model()->
    ?FORALL({K,T},{atom_key(),bst()},equals(model(delete(K,eval(T))),delete_key(K,model(eval(T))))).
prop_union_model()->
     ?FORALL({T1,T2},{bst(),bst()},
             equals(model(union(eval(T1),eval(T2))),unionby(model(eval(T1)),model(eval(T2))))).
prop_find_model()->
    ?FORALL({K,T},{atom_key(),bst()},equals(find(K,eval(T)),lookup(K,model(eval(T))))).
-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].

lookup(Key,L)->
case L of
    []->nothing;
    [{K,V}|Rest]->
        case Key=:=K of
            true->{found,V};
            false->lookup(Key,Rest)
        end
end.
unionby(L1,L2)->
    lists:usort(fun({K1,_},{K2,_})-> K1=<K2 end,L1++L2).

%% -- Test all properties in the module: eqc:module(test_bst)
