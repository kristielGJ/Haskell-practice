--1. Define a constant for the number of seconds in a week.
--output: 604800
secondsInWeek::Integer 
secondsInWeek = 60*60*24*7

--2. Define a floating point constant for the “golden ratio”
phi::Double
phi= (1+ sqrt(5))/2

--3.Define functions:
mile :: Double
mile= 1.609344

milesToKm :: Double -> Double
milesToKm n = n * mile
kmToMiles :: Double -> Double
kmToMiles n = n / mile

--4. Define a function that squares its input and returns the triple of that. 
tripleOfSquare :: Integer -> Integer
tripleOfSquare n = triple (square n)

--6. factorial
factorial :: Integer -> Integer
factorial n = product [1..n]

--7.
norm :: Double -> Double -> Double
norm x y = sqrt((x*x)+(y*y))

--returns the length of something
--test: Main.length "word"
lengthDefined :: [a] -> Int
lengthDefined [] = 0
lengthDefined (x:xs) = 1 + lengthDefined xs

--squares an integer 
square :: Integer -> Integer
square n = n*n

-- Triple an integer.
triple :: Integer -> Integer
triple n = 3*n

--Using other functions
squareOfTriple :: Integer -> Integer
squareOfTriple n = square (triple n)

-- inbuilt functions: can run in ghci
-- max  4 8  (returns maximum)
-- div (division)
-- mod (modulo)

-- infix: 
-- 13 `div` 5
-- 13 `mod` 5

-- lists: (CAN BE DONE WITH STRINGS)
-- [1..6] == [1,2,3,4,5,6]
-- product [1..3] ==6 (product of list )
-- sum [1..3] == 6 (sum of list items)

-- length [1,3,5,7] == 4
-- replicate 5 2 == [2,2,2,2,2]
-- reverse [1,3,5] == [5,3,1]
-- [1,2,3] ++ [4,5,6,7] = [1,2,3,4,5,6,7](CONCATENATE TWO LISTS)

--types:
-- :type 'a' == 'a' :: Char
-- :t ['a', 'b', 'c'] = ['a', 'b', 'c'] :: [Char]
-- :t reverse = reverse :: [a] -> [a] (a stands for any type- polymorphic)

--type classes: 
--The Integral types include Int and Integer.
--Eq types that support equality testing
--Show types whose values can be shown as strings

--static types: Integer, Int (fixed size integers), Double, Char, Bool.
--3 has type Integer – we write 3 :: Integer, pronouncing :: as “has type”.