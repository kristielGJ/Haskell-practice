import Data.Char
import System.Directory
import Data.List

--2017
--Question 1:
--a) Give a definition of a function to capitalise the first letter of each word in a string.
capitalize :: String -> String
capitalize a = unwords[toUpper c:cs | (c:cs) <- words a]
--b)Using the library functions getLine and putStrLn, write a program frag- ment to read two lines from the console and print the second line and then the first.
secondThenFirst::IO()
secondThenFirst = do
  putStr "Enter a line: "
  x <- getLine
  putStr "Enter a second line: "
  y <- getLine
  putStrLn (y)
  putStrLn (x)

--c Consider the functions
--foo
removeDupe :: Eq a => [a] -> [a]
removeDupe [] = []
removeDupe (x:xs) = x : deleteElement x xs

--bar
deleteElement :: Eq a => a -> [a] -> [a]
deleteElement x [] = []
deleteElement x (y:ys)
 | x == y = deleteElement x ys
 | otherwise = y : deleteElement y ys

--i) Explain the need for the Eq constraint in the types of foo and bar.
--mark scheme: bar uses == on x and y, so their type must belong to Eq. foo calls bar, and so inherits the constraints on the arguments from bar.

--Eq types that support equality testing, this is needed to identify whether there is a duplicate in a list for the foo function, and whether  a value is equal to the integer given to the bar function .
--i.e , when we call foo, it removes duplicates from a list (using bar) (numbers that occour more than once are equal to eachother)
--when we call bar, we give it a list and a value . it will look through the list and see whether the value is present and remove it from the list if they are equal to eachother. 

--ii) Give the value of foo [1,2,2,1,1,3] 
-- = [1,2,1,3]

--iii) In general, how is the list returned by foo related to its argument?
--mark scheme: It contains the elements of the original list, with adjacent repetitions collapsed to a single element.

--the list returend by foo is has the same values as the list that is given to it, but all values only occour once , rather  than the same value repeating in the list several times. i.e it removes duplicate values.

--d)Consider the definition:
f :: Integer -> [Integer]
f n = n : f (2*n + 1)

--take 5 (f 1) returns  a list of numbers, starting with 1 , and it returns the double of the previous number +1 [1,3,7,15,31]
--i.e 1, (1*2)+1, (3*2)+1, (7*2)+1)...

--takeWhile (<20) (map (*2) (f 1)) returns values that are less than 20 and maps the list so that  the values are even:
--[2,6,14], i.e [1,3,7,15] is mapped,=  (1*2), (3*2), (7*2)

--e) For f as in the previous part, what happens if you evaluate the expression length (f 1)
--it returns the number of elements in the list, in this case it will return nothing (an erron)because the list being computed is infinite ,this is because we have not defined a list to check the length  of, instead we are infinitely computing the next element of the function.

--Question 2:

--a) Give the values of the following expressions:
--[x*x | x <- [4,7,3]] = [16,49,9]
--[x*5-3 | x <- [1..8], even x] =[7,17,27,37]
--[y | x <- [1..3], y <- [x..x*2]]= [1,2,2,3,4,3,4,5,6]

--b)Give a list comprehension (without higher-order functions) that is equiva- lent to the following expression: map (+3) (filter ((> 20) . (*4)) xs)

--[n+3 | n <- ns, n*4 > 20]

-- c) Consider the following function definition:
mystery :: Ord a => a -> [a] -> [a]
mystery x [] = []
mystery x (y:ys)
  | x > y = mystery x ys
  | otherwise = y : ys

--mystery 5 [4,2,7,6] = [7,6]
--mystery 3 [2,7,1,8] = [7,1,8]
-- Define an equivalent function, but without using recursion. 
--this function returns values that are more than the integer given:
morethan :: Ord a => a -> [a] -> [a]
morethan x xs = [ c | c <- xs, x < c]
--mark scheme : morethan x ys = dropWhile (< x) ys

--d)Write a definition of the function that appends together lists with a separator between them, for example "intersperse ’-’ ["rat", "cat", "dog"] = "rat-cat-dog"
interspersea :: a -> [[a]] -> [a]
interspersea a [] = []
interspersea a [x] = x
interspersea a (x:xs) = x ++ (a:interspersea a xs)

{-not recursive:
intersperse x [] = []
intersperse x (y:ys) = y ++ concat (map (x:) ys)
-}


--Question 3: 

--a) Given a definition of a string of one or more words
--text :: String
--with punctuation already removed, write expressions for the following. (If you need to write any extra functions, give those too.)

--i) the number of capital letters in text.
--length (filter isUpper text)
--or: length [c | c <- text, isUpper c]

--ii) the number of words in text.
--length (words text)

--iii) the number of words of at least 10 letters in text.
-- length (filter ((>= 10) . length) (words text)) 

--or:  length [w | w <- words text, length w >= 10]

--iv) the list of words in text that occur twice in succession, e.g. “that” in “know that that had happened”. The output words should be in the original order, and if the same word is repeated more than once, it should appear for each repetition.

--let ws = words text in [w | (w, prev) <- zip ws (tail ws), w == prev]
--or recursive version:
dups :: [String] -> [String]
dups [] = []
dups [x] = []
dups (x1:x2:xs)
  | x1 == x2 = x1 : dups (x2:xs)
  | otherwise = dups (x2:xs)

--b) Define a function such that deleteFirst p xs is obtained from xs by deleting the first element x for which p x is True. If there is no such x, the result should be xs.

deleteFirst :: (a -> Bool) -> [a] -> [a]
{-
deleteFirst p [] = []
deleteFirst p (x:xs) = if p x == True then tail xs else xs
-}
deleteFirst p [] = []
deleteFirst p (x:xs)
 | p x       = xs
 | otherwise = x : deleteFirst p xs

--or:
--deleteFirst p xs = takeWhile (not . p) ++ drop 1 (dropWhile (not . p)

--c) Consider the definition

sums :: [Int] -> [Int]
sums [] = []
sums (x:xs) = x : map (+x) (sums xs)
--Give the values of
--i) take 5 (sums [1..]) = [1,3,6,10,15]
--ii) takeWhile (<20) (map (*3) (sums [1..])) = [3,9,18]

--q4
--data Tree a = Node a [Tree a]
--  deriving Show
--a
--Node 1 [] 
{-  1      

-}
--Node 1 [Node 2 [] ,Node 3[]]
{-     1
     /  \
    2    3
-}

--Node 1 [Node 2[Node 5[]], Node 3[], Node 4[]]
{-     1
     / | \
    2  3  4
    |
    5
-}

--what is the type of Node?
--Node :: a -> [Tree a] -> Tree a

--what is the type of Node True?
--Node True :: [Tree Bool] -> Tree Bool

--returns the value at the root of the tree:
--root :: Tree a -> a
--root (Node a _) = a 

--that constructs a tree of a single node,labelled by the argument.
--leaf :: a -> Tree a
--leaf a = Node a []

-- returns the sum of the integers in a tree
--sumTree :: Tree Int -> Int
--sumTree (Node x ts) = x + sum (map sumTree ts)
--sumTree (Node x ts) = x + sum [sumTree t | t <- ts]

--returns the mirror image, flipped left-to-right, of the original tree.
--flipTree :: Tree a -> Tree a
--flipTree (Node a xs) =Node a (reverse (map flipTree xs))

----------------------------------
--20??
--Question 1 
--a)write a function that returns the number of times its first argument occurs in its second.
count :: Eq a => a -> [a] -> Int

--i) list comprehension:
count x ys = length [y | y <- ys, y == x]
--ii) higher order function:
count x ys = length (filter (== x) ys)
--iii) reccursion:
count x [] = 0
count x (y:ys)
  | x == y = count x ys + 1
  | otherwise = count x ys

--b)In a certain programming language, an identifier is a string consisting of a letter followed by one or more letters, digits or underscore characters (’_’). Write a functionthat tests whether a string is such an identifier.
isIdentifier :: String -> Bool
isIdentifier a = if isAlpha(head a) && and (map isIdChar a)then True else False
  where isIdChar a = isAlpha a || isDigit a || a == '_'

--c 
while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = head (dropWhile p (iterate f x))

--Give the value of while (< 10) (*2) 1 = 16
--The above implementation uses lists internally. Write an alterna- tive definition of this function that uses recursion instead of lists.
while p f x 
  |p x = while p f (f x)
  |otherwise = x

--Question 2:
--a)Explain the difference between the types String and IO String, and give an example of how the second type may be accessed 

{-
A value of type String is a string, while one of type IO String is an I/O action to produce a string. The string may be accessed using do-notation:

do { c <- getLine; putStrLn (map toUpper c) }
or
or the raw version:

getLine >>= \ c -> putStrLn (map toUpper c)

or any similar example using other functions instead of getLine and
putStrLn. The argument of putStrLn is immaterial.
-}

--b)
--input is a list, output returns the same list however every element that has an even index is incremented by one (i.e the second, fourth, sixth (etc..) elements of the lists have 1 added to them)
--e.g the list [2,2,2,2,2] returns [2,3,2,3,2]
incr :: [Int] -> [Int]
incr [] = []
incr [x] = [x]
incr (x1:x2:xs) = x1 : (x2+1) : incr xs

--Same as filter (not.p): select the elements of the input list that don’t satisfy p.
pick :: (a -> Bool) -> [a] -> [a] 
pick p [] = []
pick p (x:xs)
  | p x = pick p xs
  | otherwise = x : pick p xs

--The infinite list of powers of x 
mklist :: Integer -> [Integer] 
mklist x = 1 : map (x*) (mklist x)

--c) total should retyrn the sum of a list
total :: (Int -> Int) -> [Int] -> Int
total x [] = 0
total x (y:ys)= x y + total x ys 

{-
or:
    total f xs = sum (fmap f xs)
or just
    total f = sum . fmap f
or list-comprehension form
    total f xs = sum [f x | x <- xs]
-}

--compose should take a lis and return an expression , e.g [1,2,3]= 1(2(3))
compose :: [a -> a] -> a -> a
compose = foldr (.) id

--compose x [] = 0
--compose (f:fs) x = f (compose fs x)
{-
or:
compose = foldr (.) id
or even
    compose fs x = foldr id x fs
or
Also equally acceptable is the more convoluted
    compose [] x = x
    compose fs x = compose (inits fs) (last fs x)
    -}
--Question 3: 
--a)Give the values of the following expressions:
{-
i) zip "abc" [1..] = [('a',1),('b',2),('c',3)]
ii) [n*(n+1) | n <- [1..4]] = [2,6,12,20]
iii) [toUpper c | c <- "I like Haskell",isLower c] =  "LIKEASKELL"
iv) [[1..n] | n <- [1..3]]= [[1],[1,2],[1,2,3]]
-}
--b)Write a function such that pairs n returns the list of pairs of numbers (x,y) such that 1 ≤ x < y ≤ n.
pairs :: Int -> [(Int, Int)]
pairs n = [(x, y) | x <- [1..n-1], y <- [x+1..n]]
--or pairs n = [(x, y) | x <- [1..n], y <- [1..n], x < y]
--c)Write a function such that replace x y zs, replaces each occurrence of x in zs with y.
replace :: Int -> Int -> [Int] -> [Int]
replace a b = map (\x -> if (a == x) then b else x)
{- or 
replace x y zs = [if z == x then y else z | z <- zs]

 replace x y = map repl
 where
  repl z
    | z == x = y
    | otherwise = z

replace x y [] = []
replace x y (z:zs)
  | z == x = y : replace x y zs
  | otherwise = z : replace x y zs
-}
--d)Consider the following function:
foo :: Integer -> [Integer]
foo n = takeWhile ((< n) . (^2)) [1..]

--i) Describe the relationship between inputs and outputs for this function. 
--foo n returns the list of positive numbers whose squares are less than n.

--ii) How would the behaviour of the function change if we replaced the function takeWhile in the definition with filter?
--The function would return the same numbers , but would not termi- nate returning the end of the list, as filter would continue to examine the infinite input list .

--Question 4
--data Tree a = Leaf a| Branch (Tree a) (Tree a)
--  deriving Show
--a)Branch (Leaf 1) (Leaf 2)
{-  /\
   /  \
  1    2
-}
--Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
{-  /\
   /  \
  1   /\
     /  \
    2    3
-}
example1 :: Tree Int
example1 = Branch (Leaf 1) (Leaf 2)
example2 :: Tree Int
example2 = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
example3 :: Tree Int
example3 =(Leaf 1)

--b) Leaf :: a -> Tree a, Branch :: Tree a -> Tree a -> Tree a
--c) Branch (Leaf 'a') :: Tree Char -> Tree Char

--d)returns True if the tree is a leaf
isLeaf :: Tree a -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

--e)What is the result of applying the function choose to each of the two trees?
choose :: Tree Int -> Int
choose (Leaf x) = x
choose (Branch l r) = choose r
--choose selects the leaf at the end of the right branch ( the furthest right branch at the end of the tree)
--for the first tree it selects 2 (for 1,2), for the second tree it selects 3 (for 1,2,3)

--f) sum of trees 
sumLTree :: Num a=>Tree a-> a
sumLTree (Leaf x) = x
sumLTree (Branch l r ) = sumLTree l +sumLTree r

--g) Generalise sumTree
foldTree :: (a -> a -> a) -> Tree a -> a
foldTree f (Leaf x) = x
foldTree f (Branch l r) = f (foldTree f l)(foldTree f r)

sumTree :: Tree Int -> Int
sumTree = foldTree (+)

-----------------------
--2020
--Question 1:
--frequencies :: [(String, Int)]

--i) the number of different words in the original document. 
--length (frequencies)

--ii) The total number of words in the original document. 
--sum [n | (w, n) <- frequencies]

--iii) The number of words that occur exactly once each in the original document.
--length [n | (w, n) <- frequencies, n == 1]

--Condiser:
--this function finds the locations of the value entered and returns them as a list.
findChar :: Eq a => a -> [a] -> [Int]
findChar x ys = [n | (n, y) <- zip [1..] ys, x == y]

--why is Eq required?
--The common type of x and the elements of the list ys must belong to the Eq class, because the function uses == on this type.

--foo 'a' "abracadabra" = [1,4,6,8,11]

--the list returned by foo is related to its argument as it returns the index(s) (position in the data type [a]) of  where the charecters are located The positions (counting from 1) of occurrence of x in the list ys

--Consider:
bar :: Ord a => a -> [a] -> [a]
{-
bar x [] = [x]
bar x (y:ys)
  | x > y     = y : bar x ys
  | otherwise = x : y : ys
-}

--bar 7 [5,3] -- [5,3,7]
--bar 4 [1,6,2,7] = [1,4,6,2,7]
--the list returned by bar inserts the value into the list where the values surrounding it are accending (sorted in accending order , lower numbers on the right)  by checking in the value is more than the value it's next to. (before the first element greater than or equal to x.)

-- Define an equivalent function, but without using recursion. 
bar x ys = takeWhile (< x) ys ++ [x] ++ dropWhile (< x) ys

--Question 2:
letters :: String -> String
letters s = [c | c <- s, isAlpha c]
--or letters = filter isAlpha

--b) Using the function letters from the previous part, and the library functions getLine and putStrLn, write a program fragment to read a line from the console and print a lin consisting of the letters in that line

lettersInLine::IO()
lettersInLine = do
  putStr "Enter a line: "
  x <- getLine
  putStrLn (letters x)

--c)
mysteryc :: [a] -> [a]
mysteryc [] = []
mysteryc (x:xs) = xs ++ [x]

--give a type signature for mystery: mysteryc :: [a] -> [a]

--mysteryc [4,5,6] = [5,6,4]
--mystery moves the first element of the list to the end of the list.

--d)
f1 :: Integer -> Integer -> [Integer]
f1 a b = a : f1 b (a+b)

ns :: [Integer]
ns = f1 1 2

--take 5 ns = [1,2,3,5,8]
--takeWhile (<30) (map (7*) ns)= [7,14,21]
--filter (<7) ns will cause an infinite wait, You would get [1, 2, 3, 5  but then it waits forever looking for more answers 

--
mapOdds :: (a -> a) -> [a] -> [a]
mapOdds f [] = []
mapOdds f (x:xs) = f x : mapEvens f xs

mapEvens :: (a -> a) -> [a] -> [a]
mapEvens f [] = []
mapEvens f (x:xs) = x : mapOdds f xs
--or 
{-
mapOdds f [] = []
mapOdds f [x] = [f x]
mapOdds f (x1:x2:xs) = f x1 : x2 : mapOdds f xs
-}

--Question 3:
--a)
--w | w <- words "Queen of Hearts", length w > 3]=["Queen","Hearts"]

-- [10*x + y | x <- [1..3], y <- [x..2*x]] = [11,12,22,23,24,33,34,35,36]

--b)
--map (2*) [1..5] = [2,4,6,8,10]
--filter odd (map (+3) [1..6]) = [5,7,9]
--takeWhile (< 100) (iterate (*2) 1) = [1,2,4,8,16,32,64]

--Question 3:
--that adds an element to the input list,if not already present
inserta :: Ord a => a -> [a] -> [a]
inserta x [] = [x]
inserta x (y:ys)
  | x == y  = y:ys
  | x < y = x : y : ys
  | otherwise = y : inserta x ys

--that returns the list of values present in both of the input lists.
intersectiona :: Ord a => [a] -> [a] -> [a]
intersectiona [] _ = []
intersectiona _ [] = []
intersectiona xs ys = filter (\x -> x `elem` xs) ys

--preffered:
intersection [] ys = []
intersection xs [] = []
intersection (x:xs) (y:ys)
  | x == y    = x : intersection xs ys
  | x <  y    = intersection xs (y:ys)
  | otherwise = intersection (x:xs) ys

--Question 4
data Tree a = Empty| Leaf a| Branch (Tree a) (Tree a)
  deriving Show

t1 :: Tree Int
t1 = Branch (Leaf 1) (Leaf 2)
t2 :: Tree Int
t2 = Branch (Branch (Leaf 2) Empty)(Branch (Leaf 3) Empty)
t3 :: Tree Int
t3 =Branch (Branch (Leaf 2) (Leaf 2))(Branch (Leaf 3) Empty)
--a)Branch (Leaf 1) (Leaf 2)
{-  /\
   /  \
  1    2
-}
--Branch (Branch (Leaf 2) Empty ), Branch (Leaf 3)  Empty)
{-  /\ 
   /  \
  /\   /\
 /  \ /  \
2     3
-}

--c) Empty :: Tree a, Branch :: Tree a -> Tree a -> Tree a

-- Branch (Leaf True) :: Tree Bool -> Tree Bool

--d) function that retyrns true if the tree is a Leaf:
isLeaff :: Tree a -> Bool
isLeaff (Leaf _) = True
isLeaff _ = False

--e Consider the following function:
--number of leaves  in the tree:
foof :: Tree a -> Int
foof Empty = 0
foof (Leaf x) = 1
foof (Branch l r) = foof l + foof r

--What is the result of applying the function foo to each of the two trees in the above diagram?
--2 in both cases.


--f)a function producing a list of the values in the tree, from left to right. The above trees would be mapped to the lists [1, 2] and [2, 3] respectively

elements :: Tree a -> [a]
elements Empty = []
elements (Leaf x) = [x]
elements (Branch l r) = elements l ++ elements r

--g)
flipTree :: Tree a -> Tree a
flipTree Empty = Empty --if not empty remove this
flipTree (Leaf x) = Leaf x
flipTree (Branch l r) = Branch (flipTree r) (flipTree l)

--2019
--Question 1 
--[w | w <- words "Haskell is fun", odd (length w)]= ["Haskell","fun"]
--[10*x + y | x <- [1..3], y <- [1..x]] = [11,21,22,31,32,33]

--b that forms a word from the initials of the words in a string, for example acronym "Call of Duty" = "CoD"

acronym :: String -> String
acronym s = map head (words s)

checkAcronym::IO()
checkAcronym = do
  putStr "Enter a line: "
  x <- getLine
  putStrLn (acronym x)

modifyy :: [a] -> [a]
modifyy xs = [x | (n, x) <- zip [1..] xs, even n]
-- modifyy "Haskell" = "akl"
--returns the even elements of the value that it is given  (every second value)

--d)
addLists1 :: Num a => [a] -> [a] -> [a]
addLists1 xs [] = xs
addLists1 (x:xs) (y:ys) = (x+y):addLists1 xs ys

--e)
addLists :: Num a => [a] -> [a] -> [a]
addLists [] [] = []
addLists [] (y:ys) = y:addLists [] ys
addLists (x:xs) [] = x:addLists xs []
addLists (x:xs) (y:ys) = (x+y):addLists xs ys

longZip :: (a -> a -> a) -> [a] -> [a] -> [a]
longZip _ [] ys = ys
longZip _ xs [] = xs
longZip f (x:xs) (y:ys) = f x y:longZip f xs ys
--or acronym s = [c | (c:_) <- words s]

--Question 2

--The type of x, which is also the type of elements of the list ys, must belong to the Eq class, because the function uses == on these. (-2 if no mention of type.)

-- mystery 2 [2,7,1,8,2,8]= [7,1,8,8]
-- mystery x ys returns the list ys minus any elements equal to x.

{-
Ideal version:
c)
    capitalPositions s =
        [n | (c, n) <- zip s [0..], isUpper c]
but long-winded recursive versions are also acceptable (if correct), e.g.
    capitalPositions s = pos 0 s
      where pos :: Int -> String -> [Int]
            pos n [] = []
            pos n (c:cs)
              | isUpper c = n : pos (n+1) cs
              | otherwise = pos (n+1) cs
The type signature [6] should be
    interleave :: [a] -> [a] -> [a]
For the definition, allow anything equivalent to
    interleave [] ys = ys
    interleave xs [] = xs
    interleave (x:xs) (y:ys) = x:y:interleave xs ys
(four cases also fine) or
    interleave [] ys = ys
    interleave (x:xs) ys = x:interleave ys xs

-}

--Question 3:
{-
a) i) ii)
 length (words s)
Anything equivalent to
    takeWhile (< n) (dropWhile (<= m) (map (^2) [1..]))
including variants of
    takeWhile (< n) (filter (> m) [x*x | x <- [1..]])
But only 12 for equivalents of
    filter (< n) (filter (> m) [x^2 | x <- [1..]])
or
because they yield partial lists.
 [w | w <- dictionary, length w >= 10]
Either of
length [w | w <- dictionary, head w == ’a’]
length [1 | (’a’:_) <- dictionary]
or other equivalent.
sum [length w | w <- dictionary]
[x|x<-xs,x>0,x<100]or[x|x<-xs,x>0&&x < 100]
 b) i) ii)
iii) c) i)
d)
[x^2 | x <- [1..], x^2 > m, x^2 < n]
ii)
Recursive version:
Full marks for [x+2 | x <- xs, x*5 > 99]
9 marks for errors like [5*x+2 | x <- xs, x > 99]
    unlines [] = ""
    unlines (x:xs) = x ++ "\n" ++ unlines xs
or non-recursive version:
    unlines = concat . map (++ "\n")
Any equivalent is equally acceptable.
-}

--Question 4:
data Tree a = Empty| Node (Tree a) a (Tree a)

{-
)
a)
i) a tree containing only the value 6.
Node Empty 6 Empty

ii) a tree containing only the values 2 and 6.

Node Empty 2 (Node Empty 6 Empty)
Node (Node Empty 2 Empty) 6 Empty
Node Empty 6 (Node Empty 2 Empty)
Node (Node Empty 6 Empty) 2 Empty

b) What are the types of Empty and Node?
Empty :: Tree a
Node :: Tree a -> a -> Tree a -> Tree a

c)Tree Bool -> Tree Bool

d)that returns True if the tree is empty
-}
isEmpty :: Tree a -> Bool 
isEmpty Empty = True
isEmpty _ = False

--e)that constructs a tree that contains just the single value supplied as an argument
singleton :: a -> Tree a
singleton x = Node Empty x Empty


--f) that returns the number of nodes in the tree.
count :: Tree a -> Int
count Empty = 0
count (Node l k r) = count l + 1 + count r


--g)In a search tree, each subtree Node l x r satisfies
--i) all the keys in l are smaller than x,  
--ii) all the keys in r are greater than x.
--that takes a list (which you may assume does not contain duplicate values) and constructs a search tree with the same elements, such that if the list is non-empty, the key in the top node of the resulting tree is the first element of the list

mkTree :: Ord a => [a] -> Tree a
mkTree [] = Empty
mkTree (x:xs) = Node (mkTree (filter (< x) xs)) x (mkTree (filter (> x) xs))
