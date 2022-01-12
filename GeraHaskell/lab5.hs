import Data.Char

--1.Define a function that returns the input list with the lower case letters capitalized and the others unchanged.
capitalize :: String -> String
capitalize a = map toUpper a
--selects capital letters from the input String
capitals :: String -> String
capitals a =filter isUpper a

-- function  that does the same, but discards non-letters
capitalizeLetters :: String -> String
capitalizeLetters a = map toUpper(filter isLetter a)

--2 write expressions for:
--(a) the squares of all the numbers up to 20 
--map (^2) [1..20]

--(b) all the square numbers less than 500 
--takeWhile (< 500) (map (^2) [1..])

--(c) all the square numbers between 500 and 1000.
--takeWhile (< 1000) (dropWhile (<= 500) (map (^2)[1..]))

--3 Rewrite count using higher order functions:
-- count takes an element  and a list, and returns how many times that element occours in the list (e.g: count 4 [1,4,4,3] = 2)
count :: Eq a => a -> [a] -> Int
count x ys = length [y | y <- ys, y == x]

countHigher :: Eq a => a -> [a] -> Int
countHigher x ys = length (filter (== x) ys)

--4 the following function returns a list of integers who's squares are less than 20
sqrLessThanTwenty :: [Integer] -> [Integer]
sqrLessThanTwenty = filter ((< 20) . (^2))
--also can be :
--sqrLessThanTwenty= filter p
--    where p n = n^2 < 20

--add one to number, then divided by  2
addOneDivTwo :: [Float] -> [Float]
--addOneDivTwo xs = map (\x -> (x+1)/2) xs
--rewrite using sections:
addOneDivTwo = map ((/2) . (+1))

--5 the following function makes a list with the difference between the elements in the provided list. for example the list 1,2,4 returns, 1,2
differences :: [Int] -> [Int]
differences xs = zipWith (-) (tail xs) xs

--6 write a function that takes a number and returns half if its even, and 3n+1 if its odd

collatz :: Int -> Int
collatz n
  | even n = n `div` 2 
  | otherwise = 3*n + 1

-- takes a positive number and returns the number of times it must be applied (to collatz) to reach 1
collatzSteps :: Int -> Int
collatzSteps n =length (takeWhile(>1)(iterate collatz n))

--takes a positive number and returns the largest number encountered by Repeatedly applying the function
collatzMax :: Int -> Int
collatzMax n 
  | n <= 1 = 1
  --or use maximum insteadof foldl1 max
  | otherwise =foldl1 max( takeWhile(>1)(iterate collatz n))

--7 pascal's triangle adds the two previous numbers
nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

pascal :: [[Int]]
pascal = iterate nextRow [1]

--8 
--(a) scanl (+)0 [1,3..] 
--The list [1,3..] is the in nite list of odd numbers. Plugging this into scanl (+)0 [1,3..]computes the in nite list:
--[0, 0+1, (0+1)+3, ((0+1)+3)+5, ...]
--which evaluates to [0, 1, 4, 9, 16, 25, ...], that is, the in nite list of square numbers.


--(b) scanl (*)1 [1..]
--scanl (*)1 [1..] computes the in nite list
--[1, 1*1, (1*1)*2, ((1*1)*2)*3, ...]
--which evaluatesto [1, 1, 2, 6, 24, ...],that is the in nite list of factorials.

--Higher Order Functions (funtions that take functions as arguments)

--mapping= apply to all elements in the lists

--Generalised mapping:
--map :: (a -> b) -> [a] -> [b]
--map f xs = [f x | x <- xs]

-- map sqrt [2, 3, 4, 5, 6, 7] =[1.4142135623730951,1.7320508075688772,2.0,2.23606797749979,2.449489742783178,2.6457513110645907]
-- map ord "Hello" = [72,101,108,108,111]
--map toUpper "hello"= "HELLO"
-- map length (words "I am not a number") = [1,2,3,1,6]

--mapping vs list comprehension:

ordAll :: [Char] -> [Int]
--ordAll cs = [ord c | c <- cs]
ordAll cs = map ord cs
--as a function that returns a function:
--ordAll = map ord

doubleAll ::[Int] -> [Int]
--doubleAll ns = [2*n | n <- ns]
doubleAll ns = map double ns
  where double n = 2*n

--------------------------
--filter: apply to selective elements in  the list

--generalised filter:
--filter :: (a -> Bool) -> [a] -> [a]
--filter p xs = [x | x <- xs, p x]

--filter odd [3,1,4,5,9,2,6] =[3,1,5,9]
--filter even [3,1,4,5,9,2,6] =[4,2,6]
--filter isUpper "Hello" = "H"
--filter isAlpha "Hello world" = "Helloworld"

--filter vs list comprehension:

pickEven :: [Int] -> [Int]
--pickEven ns = [n | n <- ns, even n]
pickEven ns = filter even ns

letters :: [Char] -> [Char]
--letters cs = [c | c <- cs, isAlpha c]
letters cs = filter isAlpha cs
--as a function thst returns a function:
--letters = filter isAlpha

--------------------------
--splitting lists:
--takeWhile isAlpha "Hello world" = "Hello"
--dropWhile isAlpha "Hello world" = " world"
--filter odd [1..10] = [1,3,5,7,9]
--takeWhile odd [1..10] = [1]
--dropWhile odd [1..10]= [2,3,4,5,6,7,8,9,10]

--Anonymous Functions:
-- backward slash n can be used to represent an anon function 
--doubleAll :: [Int] -> [Int]
--doubleAll ns = map (\n -> 2*n) ns
--doubleAll ns = map double ns
  --where double n = 2*n


--(* e )== \x -> x * e
--(e *) == \x -> e * x

--For example:
-- map (2*) [1..10]= [2,4,6,8,10,12,14,16,18,20]
-- map (+1) [1..10]= [2,3,4,5,6,7,8,9,10,11]
-- map (2^) [1..10]= [2,4,8,16,32,64,128,256,512,1024]
-- map (^2) [1..10]= [1,4,9,16,25,36,49,64,81,100]

--decending cannot use map(-1), must use subtract instead:
-- subtract 1 5 = 4
-- map (subtract 1) [1..10] = [0,1,2,3,4,5,6,7,8,9]

--Partial  application:

--map (max 5) [1..9]= [5,5,5,5,5,6,7,8,9]
-- map (min 5) [1..9]= [1,2,3,4,5,5,5,5,5]

--function composition:

--(.) :: (b -> c) -> (a -> b) -> a -> c
--or:
--(.) :: (b -> c) -> (a -> b) -> (a -> c)

--(f . g) x = f (g x)

--filter (not . isAlpha) "Hello world!"  = " !"

--filter not (map isAlpha "Hello world!") = [False,False]

--map (chr . ord) "Hello world!" = "Hello world!"

--map chr (map ord "Hello world!") = "Hello world!"

--recap:
--zip [1..10] "abcdef"= [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f')]

--zipWith is a higher order version :

-- zipWith (*) [1..] [10,9..1] = [10,18,24,28,30,30,28,24,18,10]
--i.e 1*10, 2*9, etc

-- zipWith (<) [1..] [10,9..1] = [True,True,True,True,True,False,False,False,False,False]
--i.e is 1<10?, etc

-- take 20 (zipWith (*) [1..] [2..])  = [2,6,12,20,30,42,56,72,90,110,132,156,182,210,240,272,306,342,380,420]
--i.e 1*2, 2*3, 3*4... etc

--Repeatedly applying a function:
--use : iterate

--take 20 (iterate (+1) 1) = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

----take 3 (iterate (+1) 2) = [2,3,4]

--takeWhile (< 3000) (iterate (*2) 1) = [1,2,4,8,16,32,64,128,256,512,1024,2048]

--generate pseudo-random numbers from a seed value:
generate :: Int -> [Int]
generate seed = iterate (\ n -> 224149*n + 1) seed

--The ($) operator is equivalent to function application:
--($) :: (a -> b) -> a -> b f$x=fx

--i.e, putStr (unlines (map show (take 9 (iterate (+1) 0)))) 
--Can also be:
--putStr $ unlines $ map show $ take 9 $ iterate (+1) 0

--Folding lists: reduce a list to a single value

--sum [1..5] = 15
--product [1..5] = 120
--and [True, False, True] = False
--or [True, False, True] = True
--concat [[1], [2,3], [], [4,5]] = [1,2,3,4,5]

--Generalized folding
--foldr:: (a->b->b) -> b ->[a]-> b

--sum could be replaced with: foldr (+) 0 [1..5] = 15
--product can be replaced with: foldr (*) 1 [1..5] = 120
--concat can be replaced with: foldr (++) [] [[1], [2,3], [], [4,5]] = [1,2,3,4,5]

--definitions using foldr:
{-
sum xs = foldr (+) 0 xs
product xs = foldr (*) 1 xs
and xs = foldr (&&) True xs
or xs = foldr (||) False xs
concat xs = foldr (++) [] xs
-}

--Foldl: folding but from the left: (same results as foldr)

--foldl (+) 0 [1..5] = 15
--foldl (*) 1 [1..5] = 120> 
--foldl (++) [] [[1], [2,3], [], [4,5]] = [1,2,3,4,5]

--Foldr1: for non empty lists ( when there is no answer in the empty list case)
--foldr1 max [3,1,4,5,9] = 9
--foldr1 min [3,1,4,5,9] = 1

--Foldl1: foldr1 from the left:
--foldl1 max [3,1,4,5,9] = 9
--foldl1 min [3,1,4,5,9] = 1

--Accumulating lists:
--scanning:
--scanl (+) 0 [1..6] = [0,1,3,6,10,15,21]
--scanl (*) 1 [1..6] = [1,1,2,6,24,120,720]
--scanl (+) 0 [1..4] = [0,1,3,6,10]

--Functions like iterate and scanl yield entire histories, as in nite lists.

--Scanning from the right: 
--There is also scanr, but it is mainly useful with  finite lists:

--scanr (+) 0 [1..6] = [21,20,18,15,11,6,0]
--scanr (*) 1 [1..6]= [720,720,360,120,30,6,1]