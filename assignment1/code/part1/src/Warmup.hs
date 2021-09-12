module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East (x,y)=(x+1,y)
move South (x,y)= (x,y-1)

moves :: [Direction] -> Pos -> Pos
moves [] (x,y)=(x,y)
moves (x':xs) (x,y)=moves xs $move x' (x,y)

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add Zero n =n
add (Succ m) n= Succ (add m n) 

mult :: Nat -> Nat -> Nat
mult Zero n= Zero
mult (Succ m) n=add (mult m n) n  

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero=0
nat2int (Succ n)= 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n =  Succ (int2nat (n-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert n Leaf= Node n Leaf Leaf
insert n (Node x left right)
    | n==x = Node x left right
    | n<x = Node x (insert n left) right
    | n>x =Node x left (insert n right)

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a) deriving (Eq, Show, Read, Ord)

pinsert :: (Ord a)=> a -> PTree a -> PTree a  -- uncomment and replace with the proper type of pinsert
pinsert n PLeaf = PNode n PLeaf PLeaf
pinsert n (PNode x left right)
    | n==x = PNode x left right
    | n<x = PNode x (pinsert n left) right
    | n>x = PNode x left (pinsert n right )