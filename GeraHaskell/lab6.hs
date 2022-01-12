import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

--1 Use Set to implement a function that removes all duplicates from a list.
unique :: Ord a => [a] -> [a]
unique  = Set.toList . Set.fromList

--2. Use Map to implement a function producing a map from each value that occurs in the list to the number of times that it occurs
frequencyMap :: Ord a => [a] -> Map a Int
frequencyMap xs = Map.unionsWith (+)[Map.singleton x 1 | x <- xs]
--or using function composition: 
--Map.unionsWith (+) . map (\ x -> Map.singleton x 1)

--3 Using bfs, write functions:

--returns the set of points that are most distant from the start point.
--furtherest :: Maze -> Set Point

--that returns the set of points that are nearer to the start point than the goal is.
--nearer :: Maze -> Set Point

--that returns the set of points that are more distant from the start point than the goal is.
--further :: Maze -> Set Point

---------------------------
--Sets:
{-
Set.fromList [1, 2, 3]
Set.fromList [3, 2, 1]
Set.fromList [1, 3, 2, 1, 2]
Set.fromList [2, 2, 3, 3, 1, 3, 1, 2, 1]
Set.union (Set.singleton 1) (Set.fromList [3, 2])
Set.union (Set.singleton 3) (Set.fromList [1, 3, 2])

== fromList [1,2,3]
-}

---------------Breadth-first search-------------------
-- graph represented as a function from nodes to neighbours
type Graph a = a -> Set a
-- breadth-first search: the set in position i consists of
-- the elements reachable from s in i steps and no fewer.
bfs :: Ord a => Graph a -> a -> [Set a]
bfs f s =
  takeWhile (not . Set.null) $
  map snd $
  iterate (expand2 f) $
  (Set.empty, Set.singleton s)
-- Given a pair of (nodes visited earlier, nodes just visited),
-- expand to nodes reachable from those.
expand2 :: Ord a => Graph a -> (Set a, Set a) -> (Set a, Set a)
expand2 f (old, new) =
    (seen, Set.difference (unionAll f new) seen)
  where
    seen = Set.union old new
-- the set of nodes reachable in one step from a set
unionAll :: Ord a => (a -> Set a) -> Set a -> Set a
unionAll f s = Set.unions (map f (Set.elems s))
{-Maps:
-------------------------
The Map type

For some applications sets are not enough: we want more data associated with each value, e.g. we
might want the path from the start node to each reachable node of the graph.

The module Data.Map defines a type:

'data Map k v'

where k is the type of keys (most operations require this to be an Ord type) and a type of values.

There are functions to construct maps: 

Map.empty :: Map k a
Map.singleton :: k -> a -> Map k a
Map.fromList :: Ord k => [(k, a)] -> Map k a
------------------------
Data.Map module
There are several functions for extracting data from maps: 

Map.null :: Map k a -> Bool
Map.member :: Ord k => k -> Map k a -> Bool
Map.lookup :: Ord k => k -> Map k a -> Maybe a
Map.assocs :: Map k a -> [(k, v)]

------------------------
The Map type also has a higher-order function analogous to map on lists, building a new map by applying
f to each value in the map, leaving the keys unchanged: 
Map.map :: (a -> b) -> Map k a -> Map k b
------------------------
Maps and sets

We can get a set from a map by discarding the value associated with each key: 
Map.keysSet :: Map k a -> Set k

Conversely, we can make a map from a set by specifying a value for each key using a function: 

Map.fromSet :: (k -> a) -> Set k -> Map k a

We can also make submaps with analogues of Set.intersection and Set.difference:

Map.restrictKeys :: Ord k => Map k a -> Set k -> Map k a
Map.withoutKeys :: Ord k => Map k a -> Set k -> Map k a

------------------------
Combining maps

Maps can be combined with analogues of set operations: 

Map.union :: Ord k => Map k a -> Map k a -> Map k a
Map.unions :: Ord k => [Map k a] -> Map k a
Map.intersection :: Ord k => Map k a -> Map k b -> Map k a
Map.difference :: Ord k => Map k a -> Map k b -> Map k a

There are also versions that take a combining function: 

Map.unionWith :: Ord k =>
(a -> a -> a) -> Map k a -> Map k a -> Map k a
Map.intersectionWith :: Ord k =>
(a -> b -> c) -> Map k a -> Map k b -> Map k c
-}