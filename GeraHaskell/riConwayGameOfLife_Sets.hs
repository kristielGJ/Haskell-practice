-- Conway's Game of Life
module Life where

import Geometry

import Data.Set (Set)
import qualified Data.Set as Set

type LiveCells = Set Point

-- read a configuration
readLife :: String -> LiveCells
readLife s = Set.fromList [p | (p, c) <- readGrid s, c == '@']

-- representation of a cell
showCell :: LiveCells -> Point -> Char
showCell ps p
  | Set.member p ps = '@'
  | otherwise = '.'

-- Display a rectangluar region, given the bottom left corner,
-- the top right corner and a function giving a character for each point.
showGrid :: Point -> Point -> (Point -> Char) -> String
showGrid (Point minx miny) (Point maxx maxy) cell =
    unlines [[cell (Point x y) | x <- [minx..maxx]] |
        y <- reverse [miny..maxy]]

-- the eight neighbours of a point
neighbours :: Point -> Set Point
neighbours p = Set.fromList [plusPoint p d | d <- offsets]

offsets :: [Point]
offsets = [Point dx dy | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

-- Conway's rule:
-- * a live cell with two or three live neighbours survives
-- * live cell with fewer that two or more than three live neighbours dies
-- * an empty cell with exactly three live neighbours becomes live
rule :: Bool -> Int -> Bool
rule True n = n == 2 || n == 3
rule False n = n == 3

-- Will the point will be alive in the next generation?
live :: LiveCells -> Point -> Bool
live ps p =
    rule (Set.member p ps) (Set.size (Set.intersection (neighbours p) ps))

-- the next generation
generation :: LiveCells -> LiveCells
generation ps =
    Set.filter (live ps) $ Set.union ps $ Set.unions $
    map neighbours $ Set.elems ps

testInput :: String
testInput = "\
    \......\n\
    \..@...\n\
    \..@@@.\n\
    \......\n\
    \......\n"