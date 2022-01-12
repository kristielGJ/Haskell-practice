--module Geometry where
-- compass points
data Direction = North | South | East | West
  deriving Show
-- the direction immediately to the left
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft East = North
turnLeft West = South
-- the direction immediately to the right
turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight East = South
turnRight West = North
-- x and y coordinates in two-dimensional space
data Point = Point Int Int
  deriving (Eq, Ord, Show)
-- the origin of the two-dimensional space
origin :: Point
origin = Point 0 0

--lab3 q3:

-- add two points
plusPoint :: Point -> Point -> Point
plusPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

--add a function that subtracts two points
minusPoint :: Point -> Point -> Point
minusPoint (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

--add a function that multiplies points by a given number
timesPoint :: Int -> Point -> Point
timesPoint a (Point x1 y1) = Point (x1*a) (y1*a)

--add a function that computes the sum of the absolute values of the two components.
normPoint :: Point -> Int
normPoint (Point x1 y1) =  abs(x1)+abs(y1)

--function tht=at finds the distance between two points
distance :: Point -> Point -> Int
distance p1 p2 = normPoint (minusPoint p1 p2)
--function that maps each direction to the point one unit from the origin in that direction.
oneStep :: Direction -> Point
oneStep North = Point 0 1
oneStep East = Point 1 0
oneStep South = Point 0 (-1)
oneStep West = Point (-1) 0

--lab3 q4:
--module Turtle where
--import Geometry

--define a Turtle record type
data Turtle = Turtle Point Direction PenState
  deriving Show

data PenState = PenUp | PenDown
  deriving Show

--define origin of Turtle
startTurtle :: Turtle
startTurtle = Turtle origin North PenUp

--get location of Turtle
location :: Turtle -> Point
location (Turtle pos dir pen) = pos

--comands data type
data Command = TurnLeft | TurnRight | Move Int | RaisePen |LowerPen
  deriving Show

action :: Turtle -> Command -> Turtle
action (Turtle pos dir pen) TurnLeft = Turtle pos (turnLeft dir) pen
action (Turtle pos dir pen) TurnRight = Turtle pos (turnRight dir) pen
action (Turtle pos dir pen) (Move n) = Turtle (plusPoint pos (timesPoint n (oneStep dir))) dir pen
action (Turtle pos dir _) RaisePen = Turtle pos dir PenUp
action (Turtle pos dir _) LowerPen = Turtle pos dir PenDown

--lab 4:  list comprehension, That takes a piece of text and maps it onto the two-dimensional plane, with the first character at the origin

readGrid :: String -> [(Point, Char)]
readGrid s =[(Point x y, c) | (y, cs) <- zip [0,-1..] (lines s), (x, c) <- zip [0..] cs]

--generate random numbers (see lab 5)
generate :: Int -> [Int]
generate seed = iterate (\ n -> 224149*n + 1) seed

genCommand :: Int -> Command
genCommand n
  | r == 0 = TurnLeft
  | r == 1 = TurnRight
  | r == 2 = RaisePen
  | r == 3 = LowerPen
  | otherwise = Move (r - 3)
  where
    r = n `mod` 11

genCommands :: Int -> [Command]
genCommands seed = map genCommand (generate seed)
--foldl action startTurtle $ take 100 $ genCommands 37
--Output:
--Turtle (Point 52 132) East PenUp

-- a function to construct the history of a turtle executing a list of commands
turtleHistory :: [Command] -> [Turtle]
turtleHistory = scanl action startTurtle

--Given an in nite list of commands, after the turtle reaches 200 steps away from the origin, how many more commands does it take to reach 400 steps from the origin?

further :: [Command] -> Int
further cmds =
    length $
    takeWhile (< 400) $
    dropWhile (< 200) $
    map (normPoint. location) $
    turtleHistory cmds