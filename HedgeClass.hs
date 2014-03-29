----------------------------------------------------
{-#LANGUAGE ScopedTypeVariables#-}
----------------------------------------------------
-----------------------
--Hedge algebra class:
--defining lists of
--all hedges, pos hedge,
--neg hedges, relation
--b/w hedges (either <+>
--or <-> to each other)
--and a generic comparing
--function for Ord implementation
--of Ha types
module HedgeClass (
        Ha(..),
        module Data.List
        
) where

import Data.List
class (Eq a, Ord a, Enum a, Show a, Read a) => Ha a where

      --positivity, negativity, order of Hedges 
        hedgeLs :: [a]
        hedgeLs = enumFrom (toEnum 0)

        posLs :: [a]
        posLs = [h | h <- hedgeLs, not $ negative h]

        negLs :: [a]
        negLs = [h | h <- hedgeLs, not $ positive h]

        positive :: (Ha a) => a -> Bool
        negative :: (Ha a) => a -> Bool

        positive x = elem x posLs        
        negative x = elem x negLs

     --relation between hedges
        allRel :: [(a,a)]
        allRel = [(x,y) | x <- hedgeLs, y <- hedgeLs, x /= y]

        posRel :: [(a,a)]
        posRel = [(x,y) | (x,y) <- allRel, not (x <-> y)]

        negRel :: [(a,a)]
        negRel = [(x,y) | (x,y) <- allRel, not (x <+> y)]

        (<+>) :: a -> a -> Bool
        a <+> b = elem (a,b) posRel

        (<->) :: a -> a -> Bool
        a <-> b = elem (a,b) negRel

      --generic comparing function 
        compare':: a -> a -> Ordering
        compare' h1 h2 
                | h1 == h2 = EQ
                | (positive h1) && (negative h2) = GT
                | (positive h1) && (positive h2) && (position posLs h1) < (position posLs h2) = GT
                | (negative h1) && (negative h2) && (position negLs h1) < (position negLs h2) = GT
                | otherwise = LT
                where position :: Eq a => [a] -> a -> Int
                      position xs x = head [i | (i,y) <- zip [1..] xs, x == y]

----------------------------------------------------
----------------------------------------------------








        
