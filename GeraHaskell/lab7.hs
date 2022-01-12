--Pattern Matching and Recursion
import Data.Char
--1 What does the function foo do?
--foo [1,2,1,1,1,4,0] = [1,2,1,4,0]
--foo removes duplicate values using reccursion
foo :: Eq a => [a] -> [a]
foo [] = []
foo (x:xs) = x : bar x xs

bar :: Eq a => a -> [a] -> [a]
bar x [] = []
bar x (y:ys)
  | x == y = bar x ys
  | otherwise = y : bar y ys

--2 define a function (using pattern matching over lists) that removes the first element of the list that is a digit,but keeps the rest. (You will need to import Data.Char.) Test your function on the strings "r2d2" and "2d2"

removeFirstDigit :: [Char] -> [Char]
removeFirstDigit [] = []
removeFirstDigit (y:ys)
  | isDigit y = ys
  | otherwise = y : removeFirstDigit  ys
--Generalise to a higher order function that removes the first element of the list that does not satisfy the property, but keeps the rest
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst p [] = []
removeFirst p (x:xs)
  | p x = xs-- add p instead of is digit
  | otherwise = x : removeFirst p xs

--3 define a function that adds corresponding elements of two lists of the same type, with the extra elements of the longer list added at the end:
--i.e addLists [1,2,3] [1,3,5,7,9] = [2,5,8,7,9]
addLists :: Num a => [a] -> [a] -> [a]
addLists [] [] = []
addLists (x:xs) (y:ys) = (x+y):addLists xs ys
--or: once one of the lists is empty, we can just return the other one:

addLists2 :: Num a => [a] -> [a] -> [a]
addLists2 [] ys = ys
addLists2 xs [] = xs
addLists2 (x:xs) (y:ys) = (x+y):addLists xs ys

--Generalise the  previous function  to a higher-order function that takes the combining function as an argument
--For example, addLists should be equivalent to longZip (+).
longZip2 :: (a -> a -> a) -> [a] -> [a] -> [a]
longZip2 p [] [] = []
longZip2 p (x:xs) (y:ys) = p x y:longZip2 p xs ys
--or:
longZip :: (a -> a -> a) -> [a] -> [a] -> [a]
longZip p [] ys = ys
longZip p  xs [] = xs
longZip p (x:xs) (y:ys) = p x y:longZip p xs ys

--4 Give recursive definitions of functions: (from lab 4)
reccur_odds :: [a] -> [a]
reccur_odds []=[]
reccur_odds (x:xs) = x:evens xs

--evens "abcde" = "bd"
-- elements in even-numbered positions, counting from 1
reccur_evens :: [a] -> [a]
reccur_evens []=[]
reccur_evens (x:xs) = odds xs

--odds "abcde" = "ace"
-- elements in odd-numbered positions, counting from 1
odds :: [a] -> [a]
odds xs = [x | (i, x) <- zip [1..] xs, odd i]

--evens "abcde" = "bd"
-- elements in even-numbered positions, counting from 1
evens :: [a] -> [a]
evens xs = [x | (i, x) <- zip [1..] xs, even i]

--write a function that produces an ordered list from merging two lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys
  
--implement a merge sort:
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (odds xs)) (mergeSort (evens xs))

--recap: pattern matching on tuples:
data Shape
  = Circle Double
  | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi*r*r
area (Rectangle w h) = w*h

--Pattern matching on lists:
--Cons: add element to front of list:

-- 1:[2,3,4] = [1,2,3,4]
--'a':"bcd" = "abcd"
-- [] = []
-- 3:[] = [3]
-- 2:3:[] = [2,3]
-- 1:2:3:[] = [1,2,3]
-- 'h':'e':'l':'l':'o':[] = "hello"
-- :t (:) = (:) :: a -> [a] -> [a]

--Primitive lists
{-
Defining a function on lists
So when defining a function on lists, there will be two cases.

Testing whether a list is empty (a Prelude function): 

null :: [a] -> Bool
null [] = True
null (x:xs) = False

As only the first matching clause is used, the following is equivalent: 
null [] = True
null xs = False

We can also use an “anonymous” variable “_”: 
null [] = True
null _ = False

Head and tail of a list:
head :: [a] -> a
head (x:xs) = x
tail :: [a] -> [a]
tail (x:xs) = xs


head [1,2,3,4] = 1
head (1:[2,3,4]) = 1
tail [1,2,3,4] = [2,3,4]
tail (1:[2,3,4]) = [2,3,4]
head [] = *** Exception: Prelude.head: empty list
tail [] = *** Exception: Prelude.tail: empty list
-}

{- Recursive definitions:
Using the same function (recursion):

Problem: determine the length of a list.-}

lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList xs
{-
Expanding an example step by step:
length [6,7,8] -> 1 + length [7,8]
               -> 1 + (1 + length [8])
               -> 1 + (1 + (1 + length []))
               -> 1 + (1 + (1 + 0))
               -> 3

-}

{-recursive function on a list:

calculating the product of a list:
-}
productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs

{-singleton lists: 
The pattern [x] matches a list with exactly one element:

last :: [a] -> a
last [x] = x
last (x:xs) = last xs

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs
-}

{-Functions on two lists:
zip :: [a] -> [b] -> [(a,b)]
zip [] [] = []
zip [] (y:ys) = []
zip (x:xs) [] = []
zip (x:xs) (y:ys) = (x,y):zip xs ys

or:

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y):zip xs ys
zip _ _ = []
-}

{-Concatenating two lists
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)
-}

{-Combining patterns and guards

Example 1:
Testing whether a value is in a list:

elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys)
| x == y = True
| otherwise = elem x ys

Repeated variables are not allowed:

elem x (x:ys) = True -- ILLEGAL
elem x (y:ys) = elem x ys

Alternative version: 

elem x [] = False
elem x (y:ys) = x == y || elem x ys

Example 2:
take n [] = []
take n (x:xs)
| n > 0 = x:take (n-1) xs
| otherwise = []
-}

{-
Generalizing selecting elements from a list
Generalize from:

letters :: [Char] -> [Char]
letters [] = []
letters (c:cs)
| isAlpha c = c : letters cs
| otherwise = letters cs

to take a function returning a Bool as a parameter:

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
| p x = x : filter p xs
| otherwise = filter p xs
-}

{-Splitting lists

Getting letters from the start of a list:
-}
takeLetters :: [Char] -> [Char]
takeLetters [] = []
takeLetters (c:cs)
  | isAlpha c = c : takeLetters cs
  | otherwise = []
{-
Generalizing over the predicate: 

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
| p x = x : takeWhile p xs
| otherwise = []
-}
{-Getting the rest
The rest of the list after initial letters:
-}
dropLetters :: [Char] -> [Char]
dropLetters [] = []
dropLetters (c:cs)
  | isAlpha c = dropLetters cs
  | otherwise = c:cs

{-Generalizing over the predicate: 

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs)
| p x = dropWhile p xs
| otherwise = x:xs
-}

{-Other higher-order list functions
Mapping a function over a list
Generalize from:
-}

ordAll :: [Char] -> [Int]
ordAll [] = []
ordAll (c:cs) = ord c : ordAll cs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (n:ns) = double n : doubleAll ns
  where double x = 2*x

--map ord "Hello world"

--Folding lists to summary values
foldsum :: [Int] -> Int
foldsum [] = 0
foldsum (n:ns) = n + foldsum ns

foldproduct :: [Int] -> Int
foldproduct [] = 1
foldproduct (n:ns) = n * foldproduct ns

foldconcat :: [[a]] -> [a]
foldconcat [] = []
foldconcat (xs:xss) = xs ++ foldconcat xss

{-Generalized folding

Special case:
sum :: [Int] -> Int
sum [] = 0
sum (n:ns) = n + sum ns

Generalization: 
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

Using foldr:
The previous functions become
sum ns = foldr (+) 0 ns
product ns = foldr (*) 1 ns
concat xss = foldr (++) [] xss
-}