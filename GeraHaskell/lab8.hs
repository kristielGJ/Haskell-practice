--Trees
import Data.Maybe (fromMaybe) -- cf exercise 3.7
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char

--1 consider the type of leaf trees:
data LTree a = Leaf a | Branch (LTree a) (LTree a)
  deriving (Show)

--Define some values of this type to use in testing functions on it.
example1 :: LTree Int
example1 = Branch (Leaf 2) (Leaf 3)
example2 :: LTree Int
example2 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

--Write functions to
--return the size (number of leaves) of a leaf tree.
size :: LTree a -> Int
size (Leaf x) = 1
size (Branch l r) = size l + size r
--return the depth of a leaf tree.
depth :: LTree a -> Int
depth (Leaf x) = 0
depth (Branch l r) = (depth l `max` depth r) + 1
--return the list of leaf values of a tree.
leaves :: LTree a -> [a]
leaves (Leaf x) = [x]
leaves (Branch l r) = leaves l ++ leaves r
--return the mirror image of an input leaf tree.
revLTree :: LTree a -> LTree a
revLTree (Leaf x) = Leaf x
revLTree (Branch l r) = Branch (revLTree r) (revLTree l)
--Trees with data in the leaves:
--data LeavesTree a = Leaf a | Branch (LeavesTree a) (LeavesTree a)

--2
--Generalize the sumLTree function from the lecture to obtain a higher-order function that reduces a tree to a summary value by combining the values of branches using the supplied function.
--foldLTree :: (a -> a -> a) -> LTree a -> a
foldLTree :: (a -> a -> a) -> LTree a -> a
foldLTree f (Leaf x) = x
foldLTree f (Branch l r) = f (foldLTree f l)(foldLTree f r)

--Redefine sumLTree using foldLTree.
--sumLTree :: Num a=>LTree a-> a
--sumLTree (Leaf x) = x
--sumLTree (Branch l r ) = sumLTree l +sumLTree r
sumLTree :: Num a=>LTree a-> a
sumLTree = foldLTree (+)

--Define the function returning the size of a leaf tree as a composition of the form sumLTree mapLTree g for some suitable function g. (You might also find the standard function const useful here.)
sizeComposition :: LTree a -> Int
sizeComposition = sumLTree . mapLTree (const 1)
--sizeComposition = sumLTree . mapLTree (\ x -> 1)

--Redefine each of your functions from the first question as a composition of the form foldLTree f . mapLTree g for some functions f and g (different in each case). (You might also find the standard function flip useful here.)
size_Fold_Map :: LTree a -> Int
size_Fold_Map = foldLTree (+) . mapLTree (\ x -> 1)

depth_Fold_Map :: LTree a -> Int
depth_Fold_Map  = foldLTree (\ n m -> max n m + 1) . mapLTree (\x -> 0)

leaves_Fold_Map :: LTree a -> [a]
leaves_Fold_Map  = foldLTree (++) . mapLTree (\ x -> [x])

revLTree_Fold_Map :: LTree a -> LTree a
revLTree_Fold_Map  = foldLTree (\ l r -> Branch r l) . mapLTree Leaf
{-Some of these can be simplified using the standard functions const and flip:

size = foldLTree (+) . mapLTree (const 1)
depth = foldLTree (\n m -> max n m + 1) . mapLTree (const 0)
revLTree = foldLTree (flip Branch) . mapLTree Leaf
-}
--3 Write a function to convert a value of the XML Element type from the lecture to its string representation. (optional extra challenge: use XML escaped characters to handle characters that cause problems in strings.)

-- We have mutually defined types Element and Content, so it is appropriate to use mutually defined functions:
printElement :: Element -> String
printElement (Element n attrs []) = "<" ++ unwords (n : map printAttr attrs) ++ "/>"
printElement (Element n attrs body) = "<" ++ unwords (n : map printAttr attrs) ++ ">" ++
  concat (map printContent body) ++ "</" ++ n ++ ">"

printContent :: Content -> String
printContent (Text s) = encodeString s
printContent (Child e) = printElement e
--The function printAttr converts a single attribute-value pair to a string:
printAttr :: Attribute -> String
printAttr (n, val) = n ++ "=\"" ++ encodeString val ++"\""

encodeString :: String -> String
encodeString = concat . map encodeChar

encodeChar :: Char -> String
encodeChar '<' = "&lt;"
encodeChar '>' = "&gt;"
encodeChar '&' = "&amp;"
encodeChar '"' = "&quot;"
encodeChar c
  | isAscii c = [c]
  | otherwise = "&#" ++ show (ord c) ++ ";"

--5 The following type can be used to describe a path from the root of a leaf tree to one of its leaves:
type Path = [Step]
data Step = L | R
  deriving Show

paths :: LTree Bool -> [Path]
paths (Leaf b)
  | b = [[]]
  | otherwise = []
  paths (Branch l r) = map (L:) (paths l) ++ map (R:)
  (paths r)
--This function can also be written using out higher-order functions:
pathsh :: LTree Bool -> [Path]
pathsh = foldLTree branch . mapLTree leaf
  where
    leaf b = if b then [[]] else []
    branch lps rps = map (L:) lps ++ map (R:) rps
--Trees with data in the nodes:
data NodesTree a = Empty | Node a (NodesTree a) (NodesTree a)

--Trees with both:
--data LNTree a b
--  = Empty
--  | Leaf a
--  | Node (LNTree a b) b (LNTree a b)

--Multiway trees (“Rose trees”):
data RoseTree a = RNode a [RoseTree a]

--Trees with data in their leaves
--A tree of integers:
--data LTree = Leaf Int | Branch LTree LTree
--  deriving Show

--Leaf 3
--Branch (Leaf 3) (Leaf 7)
{-  /\
   /  \
  3    7
-}
--Branch (Leaf 2) (Branch (Leaf 3) (Leaf 7))
{-  /\
   /  \
  2   /\
     /  \
    3    7
-}
{-
data LTreer = Leafr Int | Branchr LTreer LTreer
  deriving Show
--this funcion makes the tree above
sumLTree :: LTreer -> Int
sumLTree (Leafr x) = x
sumLTree (Branchr l r) = sumLTree l + sumLTree r

--Polymorphism and recursion
--Trees with anything in their leaves:
--data LTreerecur a = Leaf a | Branch (LTreerecur a) (LTreerecur a)
--  deriving Show

--A general function on trees:
sumLTreerecur :: Num a => LTreer a -> a
sumLTreerecur (Leafr x) = x
sumLTreerecur (Branchr l r) = LTreer l + LTreer r
-}
--Another list type:
data List a = Nil | Cons a (List a)

--Another recursive function on trees
--Recall general trees:
--data LTree a = Leaf a | Branch (LTree a) (LTree a)
--  deriving Show

--Doubling each element in a tree of numbers:
doubleTree :: LTree Int -> LTree Int
doubleTree (Leaf x) = Leaf (2*x)
doubleTree (Branch l r) = Branch (doubleTree l) (doubleTree r)
{-doubleTree doubles each element in the tree:
      /\          /\
     /  \        /  \
    2   /\      4   /\
       /  \        /  \
      3    7      6     14
-}

--higher order function:
--Applying an arbitrary function to each value in a general tree (defined as LTreegeneral):
mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf x) = Leaf (f x)
mapLTree f (Branch l r) = Branch (mapLTree f l) (mapLTree f r)

--Other tree types:
--Trees with data in the nodes:
--data NTree a = Empty | Node1 a (NTree a) (NTree a)
 -- deriving Show
--examples: 
--Node 10 Empty Empty
{-  10  
   /  \      

-}
--Node 17 (Node 14 Empty Empty) (Node 20 Empty Empty)
{-    17
     /  \
   14    20

-}

--Search trees
--If we keep these trees ordered using ord , we can use them as search trees:
{-
member :: Ord a => a -> NTree a -> Bool
member x Empty = False
member x (Node k l r)
  | x < k = member x l
  | x > k = member x r
  | otherwise = True
-}
--XML documents can be represented with:
data Element = Element Name [Attribute] [Content]
type Name = String
type Attribute = (Name, String)
data Content = Text String | Child Element

{-
For example, an XHTML fragment
<p><img src="warning.png"/> A paragraph with <em>emphasis</em>.</p>
could be represented by:

Element "p" [] [
Child (Element "img" [("src", "warning.png")] []),
Text " A paragraph with ",
Child (Element "em" [] [Text "emphasis"]),
Text "."]

-}

--Abstract syntax
type Variable = String

data Expr
  = Const Double
  | Var Variable
  | Minus Expr Expr
  | Plus Expr Expr
  | Mult Expr Expr
  deriving (Eq, Show)

{-data Expr: 
      Mult
     /     \
   Plus     Const
  /  \         \
 Var  Var      5.6
  |    |
 "y"  "x"

-}
  --for example an expression like "(x+y) * 5.6" is represented by the value:
  --Mult (Plus (Var "x") (Var "y")) (Const 5.6)
  