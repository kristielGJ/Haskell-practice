import Data.Char
--1.Write a function threeDifferent that takes three integer arguments and returns True if its arguments are all different from each other.

threeDifferent::Int-> Int->Int->Bool
threeDifferent x y z = x /= y && y /= z && x /= z
--or:
--threeDifferent x y z = not (x == y || y == z || x == z)

--2. What does thisfunction do?
mystery :: Int -> Int -> Int -> Bool
mystery x y z = not (x == y && y == z)
--checks if three integers are not the same number and returns a boolean ,similar to q1
--3. rewrite mystery without the "not" (using demorgans law)
mysteryNot :: Int -> Int -> Int -> Bool
mysteryNot x y z = (x /= y || y /= z)

--5. write a function that omputes the fractional part of a Double e.g mapping 23.75 to 0.75
getFractional:: Double -> Double
getFractional x = x - fromIntegral(floor x)

--Write a function clamp that takes three arguments, lo, hi and x (all of type Double) and returns x if it is between lo and hi (inclusive), lo if x is less than lo, and hi if x is greater than hi. You may assume that lo ≤ hi.
clamp:: Double-> Double -> Double -> Double
clamp lo hi x
  | lo <= x && x <= hi = x
  | x < lo = lo
  | x > hi = hi

--clamp :: Double -> Double -> Double ->Double
--clamp lo hi x = max lo (min x hi)

--7.that converts a character that represents a digit. charToNum '3' = 3
charToNum :: Char -> Int
charToNum c
  | isDigit c = ord c - ord '0'
  | otherwise = 0

--8.
--a.(a) Define an enumerated type Direction with four values, the cardinal compass points.
data Direction = North | South | East | West
  deriving Show --We’ve added deriving Show so that we can display these values.

--b.Define a function that maps each direction to the one immediately to its left. 
turnLeft :: Direction -> Direction
turnLeft a= case a of
  North -> West
  South -> East
  East -> North
  West -> South

--c.Define a function turnRight that does the reverse.
turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight East = South
turnRight West = North

--9.calendar
--a.leap year function
isLeapYear :: Int -> Bool
isLeapYear a
  | a `mod` 400 == 0 = True 
  | a `mod` 100 == 0 = False 
  | a `mod` 4 == 0 = True 
  |otherwise = False

--b.return the number of days in a  year 
daysInYear :: Int -> Int
daysInYear y
  | isLeapYear y = 366
  | otherwise = 365
--c.Define an enumerated type Month for representing the months of the year.
data Month = January | Febuary | March | April | May | June | July | August| September | October | November| December 
  deriving Show

--d.
daysInMonth :: Month -> Int -> Int
daysInMonth January y =31
daysInMonth Feb y
  | isLeapYear y = 29
  | otherwise = 28
daysInMonth March y =31
daysInMonth April y =30
daysInMonth May  y =31
daysInMonth June y =30
daysInMonth July y =31
daysInMonth August y =31
daysInMonth September y =30
daysInMonth October y =31
daysInMonth November y =30
daysInMonth December y =31

--A way of writing the function max:

maxDef :: Int -> Int -> Int
maxDef x y = if x >= y then x else y

--using boolean instead of if statements
maxBool :: Int -> Int -> Int
maxBool x y
  | x >= y = x
  | otherwise = y

--find maximum number from three numbers:
maxThree :: Int -> Int -> Int -> Int
maxThree x y z
  | x >= y && x >= z = x
  | y >= z = y
  | otherwise = z

--simplified maxThree
maxThreeSimple :: Int -> Int -> Int -> Int
maxThreeSimple x y z = max x (max y z)
--infix ver:
--maxThree x y z = x `max` y `max` z

--A function returning the middle of three numbers. (in order)
middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
  | between y x z = x
  | between x y z = y
  | otherwise = z

--auxiliary function: checks if middle number is in the middle 
between :: Int -> Int -> Int -> Bool
between x y z = (x <= y && y <= z) || (z <= y && y <= x)

--max 9 4
--4 `max` 9

--Numbers
--basic types in haskell:
--bool, char, int, integer, float, double  

--ghci:
--minBound::Int = -9223372036854775808 (smallest Int Value)
--maxBound::Int = 9223372036854775807

-- Infix
--(+) :: Int -> Int -> Int 
--(*) :: Int -> Int -> Int
--(+) 1 ((*) 2 5) is equal to (2*5)+1

--inbuilt functions for Int:
--div :: Int -> Int -> Int
--mod :: Int -> Int -> Int
--abs :: Int -> Int (absolute value)
--negate :: Int -> Int (turn negative)
-- floor 3.7 ==3
-- ceiling 3.7 ==4

--Char:
-- :m + Data.Char
-- isDigit '2'= True
-- isDigit 'a' = False
-- isLower 'B' = False
-- isLower 'b' = True
-- isAlpha 'c' = True
-- ord 'a' == 97
-- chr 98 == 'b'