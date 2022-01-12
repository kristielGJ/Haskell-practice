--Input and outputs in functional programming uses actions
import System.Directory

--1 Write a program that will prompt the user, read a line of input, and then report on whether the line is a palindrome.

isPalindrome::IO()
isPalindrome = do
  putStr "Enter a line: "
  x <- getLine
  if palindrome x == True then putStrLn (x ++ " is a palindrone")else putStrLn (x ++ " is not a palindrone")

palindrome:: String -> Bool
palindrome a =if (reverse a) == a then True else False

--Use this function to write an IO function that prints the lines of a file in reverse order.
--readFile :: FilePath -> IO String
--test: reverseFile "out.txt"
reverseFile::FilePath -> IO()
reverseFile file = do
  x <- readFile file
  putStrLn(unlines(reverse(lines x)))

--3 Write an IO action to print the contents of the current directory
directoryContent::IO ()
directoryContent  = do
  x <- getDirectoryContents "."
  --or : x  = getDirectoryContents "." >>= putStr . unline
  putStrLn(unlines x)

--4 , write an action to print the contents of all the files in the current directory. Try to do this without using recursion
printFile :: FilePath -> IO ()
printFile file = do
  contents <- readFile file
  putStr contents

printAll :: IO ()
printAll = do
  files <- getDirectoryContents "."
  sequence_ [printFile file | file <- files]

--or printAll = getDirectoryContents "." >>= mapM_ printFile

--5 Write a function that repeats an action n times.
repeatIO :: Int -> IO a -> IO ()
repeatIO 0 action = return ()
repeatIO n action = do
  action
  repeatIO (n-1) action

--a program that asks for user input until a palindrome is entered

isPalindromeProg :: IO ()
isPalindromeProg = do
  putStrLn "This program reads lines until a palindrome is entered"
  readLines

readLines :: IO ()
readLines = do
  putStr "Enter a line: "
  line <- getLine
  if reverse line == line then putStrLn "palindrome"
  else do
    putStrLn "not a palindrome"
    readLines

--import System.Random

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  putStrLn (reverse s1)
  putStrLn (reverse s2)

sumIo :: IO ()
sumIo = do
  putStrLn "Enter two numbers"
  x <- getInt
  y <- getInt
  putStrLn ("The sum is " ++ show (x+y))

getSum :: IO Int
getSum = do
  x <- getInt
  y <- getInt
  return (x+y)

----guessing game---
getInt :: IO Int
getInt = readLn

maxValue :: Int
maxValue = 100

playGame :: IO ()
playGame = do
  guessingGame (13 `mod` maxValue + 1)

guessingGame :: Int -> IO ()
guessingGame target = do
    putStr $ "Guess a number between 1 and " ++ show maxValue ++ ": "
    guesses target 1

guesses :: Int -> Int -> IO ()
guesses target nguesses = do
    guess <- getInt
    if guess == target then
        putStrLn $ "Correct in " ++ show nguesses ++ " guesses"
    else do
        putStr $ if guess > target
            then "Too high! " else "Too low! "
        putStr "Guess again: "
        guesses target (nguesses+1)

--OUTPUT ACTIONS:
--putStr is an action that we use to d "print" any expression typed in as a  prompt, if it has the type IO()

--putStr::String -> IO ()

--"Hello\nworld\n" = "Hello\nworld\n"

-- :t putStr = putStr :: String -> IO ()

-- :t putStr "Hello\nworld\n"  = putStr "Hello\nworld\n" :: IO ()

--putStr "Hello\nworld\n" =
--Hello
--world

{-
putStr :: String -> IO ()
putChar :: Char -> IO ()
type FilePath = String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()
-}

--writing to a txt file:
{-
writeFile "out.txt" "Hello\nworld\n"

appendFile "out.txt" (unlines (map show [1..100]))
-}

--sequencing the actions:
-- >> is used to set the order of  when the actions happen
--e.g:
--putStr "Hello" >> putChar '\n' >> putStr "world\n"

--putStrLn "Hello" >> putStrLn "world"
--print (2+3)

--Input actions:
{-
For example, input actions: 

getChar :: IO Char
getLine :: IO String
readFile :: FilePath -> IO String

We would like to pass these values to other functions, like 
putStrLn :: String -> IO ()
putStrLn . reverse :: String -> IO ()
putChar :: Char -> IO ()

For this, we use another operation 

(>>=) :: IO a -> (a -> IO b) -> IO b

x >>= f says: perform x, and if this yields v, then perform f v.

Sequencing:

x >> y = x >>= \ _ -> y
-}

--examples of inputs using ghci:
--getLine >>= putStrLn
--input: hi
--output: hi

--getLine >>= (putStrLn . reverse)
--input: hello
--output: olleh