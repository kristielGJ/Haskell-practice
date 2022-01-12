module Ratings where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Distribution.Simple.Command (OptDescr(BoolOpt))

type Customer = String
type Product = String
type Score = Int

--  A single rating of a product by a customer
type Rating = (Customer, Product, Score)

--  Helper functions:

--  A function that removes the duplicates from a list
removeDupe :: Ord a => [a] -> [a]
removeDupe = Set.toList . Set.fromList

--  Checks if two values are equal to eachother and returns a boolean
eqlTo :: Eq a => [a]-> Bool--The Eq class defines equality (==) and inequality (/=)
eqlTo[] = True 
eqlTo(x:y) = all (== x) y-- if == , then Eq is True (x == x = True)

--  Finds the average from a list of integers and returns a double
getAverage :: [Int] -> Double
getAverage avg = sum (map fromIntegral avg) / fromIntegral (length(map fromIntegral avg))

--  Customers and the number of products they have rated
customerProducts :: Customer -> [Rating] -> (Customer, Int)
customerProducts acustomer lst = (acustomer, length (filter (\(c, p, s) -> c == acustomer) lst))

--  A getter function for customers
customer :: Rating -> Customer
customer (c, p, s) = c

--  A getter function for products
aproduct :: Rating -> Customer
aproduct (c, p, s) = p

--  A getter function for scores
scores :: Rating -> Score
scores (c, p, s) = s 

--  Q1:
--  A list of all customers who submitted a rating, without duplicates
customers :: [Rating] -> [Customer]
customers = removeDupe . map customer 

--  Q2:
--  A list of all products that have been rated, without duplicates
products :: [Rating] -> [Product]
products = removeDupe . map aproduct

--  Q3:
--  Customers and the number of products they have rated
numScores :: [Rating] -> [(Customer, Int)]
numScores lst = removeDupe (map (\(c, p, s) -> customerProducts c lst) lst)

--  Q4:
--  A list of the customers who give the same score in all their ratings
consistent :: [Rating] -> [Customer]
consistent = map fst --returns the first fst
            .Map.toList.Map.filter eqlTo -- check if values are the same 
            .foldr(\(c,p,s)-> Map.insertWith(++)c [s])Map.empty --reduce list
--  Q5:
--  The average score of each product
averageScore :: [Rating] -> [(Product, Double)]
averageScore = Map.toList.Map.map getAverage
            .foldr(\(c,p,s)->Map.insertWith (++) p [s]) Map.empty 

--  Q6:
--  The products that have each been rated by every customer
popular :: [Rating] -> [Product]
popular pl = map fst
        $Map.toList $Map.filter (== Set.fromList(customers pl))--get all the customers
        $foldr(\(c,p,s)->Map.insertWith(\x y ->Set.insert c y )p(Set.singleton c))Map.empty pl