module Main where

import Data.List

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

type Subst = Assoc Char Bool

type Assoc k v = [(k, v)]

find' :: Eq k => k -> Assoc k v -> v
find' k t = head [v | (k', v) <- t, k == k']

p1 :: Prop
p1 = (Var 'A') `And` (Not (Var 'A'))

p2 :: Prop
p2 = ((Var 'A') `And` (Var 'B') `Imply` (Var 'A'))

p3 :: Prop
p3 = (Var 'A') `Imply` ((Var 'A') `And` (Var 'B'))

p4 :: Prop
p4 = ((Var 'A') `And` ((Var 'A') `Imply` (Var 'B'))) `Imply` (Var 'B')

main :: IO ()
main = do
  print $ isTaut p1
  print $ isTaut p2
  print $ isTaut p3
  print $ isTaut p4

avalia :: Subst -> Prop -> Bool
avalia _ (Const b)    = b
avalia ss (Var c)     = find' c ss
avalia ss (Not p)     = not $ avalia ss p
avalia ss (And p q)   = and $ avalia ss p $ avalia ss q
avalia ss (Imply p q) = not $ avalia ss p || avalia ss q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not x)     = vars x
vars (And x y)   = vars x ++ vars y
vars (Imply x y) = vars x ++ vars y

uniquevars = nub . vars

bools :: Int -> [[Bool]]
bools 0 = []
bools 1 = [[False], [True]]
bools n = map (False :) bool ++ map (True :) bool
  where
    bool = bools (n - 1)

substs :: Prop -> [Subst]
substs x = map (zip chars) bools'
  where
    chars = uniquevars x
    len   = length chars
    bools'= bools len

isTaut :: Prop -> Bool
isTaut = undefined
