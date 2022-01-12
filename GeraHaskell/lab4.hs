import Data.Char
import Data.List

--1 Write expressions for :
--numbers from 1 to 100: [n | n <- [1..100]] or just [1..100]
--squares of numbers from 1 to 20: [n*n | n <- [1..20]]
--divisors of 100: [n | n <- [1..100], 100 `mod`n==0]

--2 triple all values of a list:
tripleAll :: [Int] -> [Int]
tripleAll ns = [3*n | n <- ns]

squareAll :: [Int] -> [Int]
squareAll ns = [n*n | n <- ns]

--3 give a definition of a function that returns the input list with the lower case letters capitalized. 
capitalize :: String -> String
capitalize a = [toUpper c | c <- a]

-- function  that does the same, but discards non-letters
capitalizeLetters :: String -> String
capitalizeLetters a = [toUpper c | c <- a, isAlpha c]

--4 write a function that takes a string consisting of words, and returns a string of the same words in the reverse
backwards :: String -> String
backwards s = unwords (reverse (words s))

--Write a function of the same type that takes a string consisting of words, and returns a string of the same words in the same order, but with each word reversed.
backwardsWord :: String -> String
backwardsWord a =unwords[reverse c | c <- words a]

--5 write a function that returns a list of numbers that evenly divide the given number
divisors :: Int -> [Int]
divisors a = [n | n <- [1..a], a `mod`n==0]

--6 compute average of a list 
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)
 
--7 palindrome
palindrome:: String -> Bool
palindrome a =if (reverse a) == a then True else False
--or:
--palindrome w
--  | reverse w == w = True
--  | otherwise = False
-- ignoring non-letters and the distinction between upper and lower case 
palindrome2 :: String -> Bool
palindrome2 w = palindrome (capitalizeLetters w)

--8 Write a function that takes a list of characters and produces a list of unique characters, each paired with the number of times they occur in the original list
frequency :: Ord a => [a] -> [(a, Int)]
frequency a = [(head x, length x) |x <- group(sort a)]

--9 if the word can be rearranged to a palindrome (if more than one letter occurs an odd number of times return false) 

--make a list of the letters that occour an odd number of times. if the length of this list is less than 1 then return True

getOddOccurence:: [Char] ->[Char]
getOddOccurence a = [x | (x, n) <- frequency a, odd n]

palindromic :: [Char] -> Bool
palindromic a = if length(getOddOccurence a) <= 1 then True else False

--11
--:m + Data.List
--tails returns a list of possible "tail" 's a list could return depending on the position of the head.
--(tails returns a list containing the input list, its tail, the tail of that, and so on down to the empty list. So it produces all the suffixes of the input list, longest first)
--tails [1,2,3,4] = [[1,2,3,4],[2,3,4],[3,4],[4],[]]
--tails "abcd" = ["abcd","bcd","cd","d",""]

--inits does the same for init, producing all the prefixes of the input list, but with the shorter lists first.
--inits [1,2,3,4] = [[],[1],[1,2],[1,2,3],[1,2,3,4]]
--inits "abcd" = ["","a","ab","abc","abcd"]

-- all the ways of splitting a list, earliest first
splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

-- all non-empty sublists of a list
sublists :: [a] -> [[a]]
sublists xs =
[ys | ts <- tails xs, ys <- inits ts, not (null
ys)]
--12: that takes a list of bar heights and returns the area of the largest rectangle under that histogram.
--You may assume that the list is non-empty and all the heights are non-negative. For example,largestRectangle [2,1,4,5,1,3,3] (the histogram shown above) should return 8.

largestRectangle :: [Int] -> Int
largestRectangle hs =
maximum [length l * minimum l | l <- sublists hs]


--Functions from the Data.List module
--m +
--Data.List
--Sorting and grouping:

--sort [3,1,4,5,9,2,6] = [1,2,3,4,5,6,9]
--sort "Hello world!" = " !Hdellloorw"
--group [1,2,2,2,3,1,1,2]= [[1],[2,2,2],[3],[1,1],[2]]
--group "abracadabra" = ["a","b","r","a","c","a","d","a","b","r","a"]
--sort "abracadabra" = aaaaabbcdrr
--group (sort "abracadabra") =["aaaaa","bb","c","d","rr"]

--String processing: 
--lines "two words\nline\n" = ["two words","line"]
--unlines ["two words", "line"] = "two words\nline\n"
--words "two words" = ["two","words"]
--unwords ["two", "words"]= "two words"
--unwords (reverse (words "hello world"))= "world hello"
-- unwords [reverse w | w <- words "hello world"]= "olleh dlrow"
--unlines ["line one", "line two"] = "line one\nline two\n"

--putStr:
--putStr "abc\ndef\n"
{-output:
abc
def
-}
--putStr (unlines ["line one", "line two"])
{-output:
line one
line two
-}
-- putStr (unlines ["(" ++ show n ++ ")" | n <- [1..10]])
{-output:
(1)
(2)
(3)
(4)
(5)
(6)
(7)
(8)
(9)
(10)-}
--putStr (unlines [replicate n '*' | n <- [1..10]])
{-output:
*
**
***
****
*****
******
*******
********
*********
**********
-}

--Developing a list function:
--validating card numbers
-- Luhn algorithm for validating card numbers
valid :: String -> Bool
valid s = total `mod` 10 == 0
  where
    total = sum (odds rev_ns) +
            sum [substitute n | n <- evens rev_ns]
    rev_ns = reverse (digits s)

-- the digits in a string
digits :: String -> [Int]
digits s = [digitToInt c | c <- s, isDigit c]

--odds "abcde" = "ace"
-- elements in odd-numbered positions, counting from 1
odds :: [a] -> [a]
odds xs = [x | (i, x) <- zip [1..] xs, odd i]

--evens "abcde" = "bd"
-- elements in even-numbered positions, counting from 1
evens :: [a] -> [a]
evens xs = [x | (i, x) <- zip [1..] xs, even i]

--[(n, substitute n) | n <- [0..9]]
--[(0,0),(1,2),(2,4),(3,6),(4,8),(5,1),(6,3),(7,5),(8,7),(9,9)]
-- substitute digit: same as summing digits of n*2
substitute :: Int -> Int
substitute n
  | n*2 > 9 = n*2 - 9
  | otherwise = n*2

--maximum [3,1,4,5,9,2,6] = 9
--minimum [3,1,4,5,9,2,6] = 1
--maximum [6]= 6
--maximum [] = exception

--ghci: use :m + Data.Char
--lists:
--[1..7]== [1,2,3,4,5,6,7]
--[1,2..7] == [1,2,3,4,5,6,7]
--[1,3..11]== [1,3,5,7,9,11]
--[1,4..20]== [1,4,7,10,13,16,19]
--[12,11..5]== [12,11,10,9,8,7,6,5]

--infinite lists
-- [1..]

--List Comprehensions
--Ten square numbers:
--[n*n | n <- [1..10]]
--[1,4,9,16,25,36,49,64,81,100]

--[n | n <- [1..100], n*n == 49]
--[n | n <- [1..], n*n == 49]
--[7]

--[(x,c) | x <- [1..3], c <- "abcd"]
--[(1,'a'),(1,'b'),(1,'c'),(1,'d'),(2,'a'),(2,'b'),(2,'c'),(2,'d'),(3,'a'),(3,'b'),(3,'c'),(3,'d')]

--[(x,y) | x <- [1..4], y <- [1..3]]
--[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3)]

--same as above but pairs added are less than or equal to 5
--[(x,y) | x <- [1..4], y <- [1..3], x+y <= 5]
--[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(4,1)]

--[x*y | x <- [1..4], y <- [1..3], x+y <= 5]
--[1,2,3,2,4,6,3,6,4]

--[(x, y) | x <- [1..3], y <- [1..x]]
-- 1 pair of 1's , 2 pairs of 2, 3 pairs of 3
-- [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3)]

--[c | c <- "Hello world", isLower c]
--"elloworld"

--[ord c | c <- "Hello world", isLower c]
--numbers for "e,l,l,o,w,o,r,l,d"
--[101,108,108,111,119,111,114,108,100]

--functions for lists:

--length ["Hello", "world"] = 2

--[1,2,3] ++ [4,5] = [1,2,3,4,5]

--concat [[1,2,3], [4,5]] = [1,2,3,4,5]

--null [1..6] == False

--null [6..1] == True

-- head "hello" == 'h'
-- tail "hello" == "ello"
-- last "hello" == 'o'
-- init "hello" == "hell"
-- reverse "hello" =="olleh"

--take 3 "hello" == "hel"
--take 20 [1..5] ==[1,2,3,4,5]
--drop 3 "hello" == "lo"
--drop 20 [1..5] == []

--replicate 5 3 == [3,3,3,3,3]


--zip "abc" [1..4] = [('a',1),('b',2),('c',3)]
--unzip [('a',1),('b',2),('c',3)]= ("abc",[1,2,3])


--zip "computer" "science"= [('c','s'),('o','c'),('m','i'),('p','e'),('u','n'),('t','c'),('e','e')]
--[x | (x, y) <- zip "computer" "science", x == y] = e
