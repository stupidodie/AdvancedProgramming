-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A non-symbolic generator for bst, parameterised by key and value generators

%% insert values in to a leaf
bst(Key, Value) ->
    ?LET(KVS, eqc_gen:list({Key, Value}),
         lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
                     empty(),
                     KVS)).
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
    ?FORALL(T, bst(atom_key(), int_value()),
            valid(T)).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            valid (insert(K, V, T))).
% if the empty tree is valid
prop_empty_valid()->
    ?FORALL(T,empty(),valid(T)).
% if we delete into a valid tree it stays valid
prop_delete_valid()->
    ?FORALL({K,T},{atom_key(),bst(atom_key(),int_value())},
           valid(delete(K,T))).
% if we union two valid trees it stays valid
prop_union_valid()->
    ?FORALL({T1,T2},{bst(atom_key(),int_value()),bst(atom_key(),int_value())},
           valid(union(T1,T2))).
%%% -- postcondition properties

prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, insert(K1, V, T)),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, T)
                       end)).


prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K, insert(K, V, T)),
                       {found, V})).


prop_find_post_absent() ->
   % ∀ k t. find k (delete k t) === nothing
    ?FORALL({K,T},{atom_key(),bst(atom_key(),int_value())},
           eqc:equals(find(K,delete(K,T)),nothing)).

prop_delete_post()->
    ?FORALL({K1,K2,T},{atom_key(),atom_key(),bst(atom_key(),int_value())},
           eqc:equals(find(K1,delete(K2,T)),
                      case K1=:=K2 of
                          true-> nothing;
                          false->find(K1,T)
                      end)).
prop_union_post()->
    ?FORALL({K0,K1,V1,T1,K2,V2,T2},{atom_key(),atom_key(),int_value(),bst(atom_key(),int_value()),atom_key(),int_value(),bst(atom_key(),int_value())},
           eqc:equals(find(K0,union(insert(K1,V1,T1),insert(K2,V2,T2))),
                     case K0=:=K1 of
                         true->{found,V1};
                         false->case find(K0,T1) of
                                    nothing->case K0=:= K2 of
                                                 true->{found,V2};
                                                 false->find(K0,T2)
                                             end;
                                     {found,V}->{found,V}
                                 end
                     end)).

%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            bst:size(insert(K, V, T)) >= bst:size(T)).



obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(),
             bst(atom_key(), int_value())},
            obs_equals(insert(K1, V1, insert(K2, V2, T)),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, T);
                           false -> insert(K2, V2, insert(K1, V1, T))
                       end)).


%%% -- Model based properties
model(T) -> to_sorted_list(T).


prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            equals(model(insert(K, V, T)),
                   sorted_insert(K, V, delete_key(K, model(T))))).


-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].



%% -- Test all properties in the module: eqc:module(test_bst)
