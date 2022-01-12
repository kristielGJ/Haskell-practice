import Data.Char
--Polymorphic functions on pairs:

--1. Define a function that takes a pair of values and returns them swapped around.
swap :: (a,b) -> (b,a)
swap a = (snd a,fst a)

--2. Define a function that returns a pair with two copies of its argument.
dup :: a -> (a,a)
dup a = (a,a) 

--see lab3_q3/4

--5.function that prevents a runtime error if div uses 0 as a second argument
safeDiv :: Int -> Int -> Maybe Int
safeDiv i e
  | e/=0 = Just (i `div` e)
  | otherwise = Nothing

--6.define a function that produces a Just result only if both arguments are Just, and a Nothing if either argument is Nothing.
pairMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
pairMaybe (Just x) (Just y) = Just (x,y)
pairMaybe _ _ = Nothing

--7.extracts the value inside a Maybe value, returning a default value (supplied as the first argument) if the second argument is Nothing.
--from Data.Maybe
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe def (Just x) = x

--8. Define a function that returns the value inside an Either value, ignoring how it is tagged.

whatever :: Either a a -> a
whatever (Left a)= a
whatever (Right b)=b

--9.define a function (both) that produces an OK result only if both arguments are OK, and otherwise an Error (but including both messages if both parts failed)
data Err a = OK a | Error String

both :: Err a -> Err b -> Err (a,b)
both (OK x) (OK y) = OK (x,y)
both (OK x) (Err msg) = Err msg
both (Err msg) (OK y) = Err msg
both (Err msg1) (Err msg2) = Err (msg1 ++ "\n" ++ msg2)
--Tuples
--compare (1,True,'c') (1,True,'d') = LT (less than)
--compare (8,'b') (3,'a') = GT (greater than)
--lexicographical ordering, using comparison 

--fst (first value) (a,b)= a
--snd (second value) (a,b)= b

--Adding points
--type Point = (Int, Int)
--record type:
data Point = Point Int Int
  deriving (Show)

origin :: Point
origin = Point 0 0

--plusPoint :: Point -> Point -> Point
--plusPoint (x1, y1) (x2, y2) = (x1+x2, y1+y2)
--for record type definition:
plusPoint :: Point -> Point -> Point
plusPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

--We could use these in an equivalent definition of plusPoint:
--plusPoint :: Point -> Point -> Point
--plusPoint p1 p2 = (fst p1 + fst p2, snd p1 + snd p2)

data PriceTag = Item String Double
--defines a new type 'PriceTag' and a new constructor ( Item:: String ->Double->PriceTag.

--PATTERN matching
showPriceTag :: PriceTag -> String
showPriceTag (Item n price) = n ++ " -- " ++ show price

--using the Item constructor to build on the type when you return PriceTag
addVAT :: PriceTag -> PriceTag
addVAT (Item nm price) = Item nm (1.2*price)

--type vs data
type Point1 = (Int, Int)
data Point2 = Point2 Int Int
--type defines a synonym for an existing type, with which is completely interchangeable (allowing the use of general functions).
--data defines a new type, and a new constructor, which is the only way to build values of that type.

--Sum types
-- Circle 2.5
-- Rectangle 2.0 4.2
data Shape = Circle Double
  | Rectangle Double Double
  deriving (Eq, Show)

area :: Shape -> Double
area (Circle r) = pi*r*r
area (Rectangle w h) = w*h

scale :: Double -> Shape -> Shape
scale x (Circle r) = Circle (r*x)
scale x (Rectangle w h) = Rectangle (w*x) (h*x)

--improving  function from lab 2
-- If the input is an error it flags it up rather than  not working
charToNum :: Char -> Maybe Int
charToNum c
  | isDigit c = Just (ord c - ord '0')
  | otherwise = Nothing

--maybe can also be used to cover for data which may not be present (not nessasary)
--data Person = Person String Date (Maybe Date)
--The standard prelude also defines a general type representing the tagged sum of two types
--data Either a b = Left a | Right b
 -- deriving (Eq, Ord, Show)