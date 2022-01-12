-- Tautology checker for propositional formulae
-- adapted from
-- Graham Hutton "Programming in Haskell" 10.5 (1st ed.), 8.6 (2nd ed.)
--module Proposition where

import Data.Maybe (fromMaybe) -- cf exercise 3.7
import Data.Set (Set)
import qualified Data.Set as Set

-- propositional formulae, parameterized by the type of variables
data Prop a
    = Var a
    | Not (Prop a)
    | And (Prop a) (Prop a)
    | Imply (Prop a) (Prop a)
    deriving (Show)

-- test cases for a tautology checker
p1, p2, p3, p4 :: Prop Char
p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

tautologyTests :: [(Prop Char, Bool)]
tautologyTests = [
    (p1, False), (p2, True),
    (p3, False), (p4, True)]

-- test cases for which the function does not produce the expected output
failures :: Eq b =>
    (a -> b) -> [(a, b)] -> [(a, b, b)]
failures f xys =
    [(x, y, f x) | (x, y) <- xys, f x /= y]

-- create a proposition by mapping f over each variable in p
mapProp :: (a -> b) -> Prop a -> Prop b
mapProp f (Var v) = Var (f v)
mapProp f (Not p) = Not (mapProp f p)
mapProp f (And p q) =
    And (mapProp f p) (mapProp f q)
mapProp f (Imply p q) =
    Imply (mapProp f p) (mapProp f q)

-- an assignment of a Boolean value to each variable
type Subst a = [(a, Bool)]

-- evaluate a proposition with a given substitution for its variables
eval :: Ord a => Subst a -> Prop a -> Bool
eval s p = evalBool (mapProp (value s) p)

-- evaluate a proposition in which variables have Boolean values
evalBool :: Prop Bool -> Bool
evalBool (Var b) = b
evalBool (Not p) = not (evalBool p)
evalBool (And p q) = evalBool p && evalBool q
evalBool (Imply p q) =
    not (evalBool p) || evalBool q

-- get the value of a variable from a substitution, defaulting to False
value :: Eq a => Subst a -> a -> Bool
value s v = fromMaybe False (lookup v s)

-- the set of variables in a proposition
vars :: Ord a => Prop a -> Set a
vars (Var v) = Set.singleton v
vars (Not p) = vars p
vars (And p q) = Set.union (vars p) (vars q)
vars (Imply p q) = Set.union (vars p) (vars q)

-- all possible substitutions for a list of variables
bools :: [a] -> [Subst a]
bools [] = [[]]
bools (x:xs) =
    [(x, b):s | b <- [False, True], s <- bools xs]

-- all possible substitutions for a proposition
substs :: Ord a => Prop a -> [Subst a]
substs p = bools (Set.elems (vars p))

-- Is the proposition a tautology (i.e. True for all substitutions)?
tautology :: Ord a => Prop a -> Bool
tautology p = and [eval s p | s <- substs p]