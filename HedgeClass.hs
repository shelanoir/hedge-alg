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
-- * Hedge typeclass        
-- |A type class of linear symmetrical hedge algebras. 
--
-- Properties:
--
-- > h `elem` hedgeLs => h `elem` posLs OR h `elem` negLs
-- > h, k `elem` posLs/negLs => h >= k OR k >= h
-- > h, k `elem` hedgeLs => h <+> k OR h <-> k

class (Eq a, Ord a, Enum a, Show a, Read a) => Ha a where
        -- |List of every hedge in the structure
        hedgeLs :: [a]
        hedgeLs = enumFrom (toEnum 0)

        -- |List of positive hedges
        posLs :: [a]
        posLs = [h | h <- hedgeLs, not $ negative h]

        -- |List of negative hedges
        negLs :: [a]
        negLs = [h | h <- hedgeLs, not $ positive h]

        -- |Test for the positivity of a hedge
        positive :: (Ha a) => a -> Bool
        -- |Test for the negativity of a hedge
        negative :: (Ha a) => a -> Bool

        positive x = elem x posLs        
        negative x = elem x negLs

        -- |Every relations in the structures. This list is used
        -- in the construction of the lists of positive and
        -- negative relations.        
        allRel :: [(a,a)]
        allRel = [(x,y) | x <- hedgeLs, y <- hedgeLs]--, x /= y]

        -- |List of positive relations between hedges
        posRel :: [(a,a)]
        posRel = [(x,y) | (x,y) <- allRel, not (x <-> y)]

        -- |List of negative relations between hedges
        negRel :: [(a,a)]
        negRel = [(x,y) | (x,y) <- allRel, not (x <+> y)]

        -- |isPositive operation: h \<+> k iff h is positive
        -- w.r.t k
        (<+>) :: a -> a -> Bool
        a <+> b = elem (a,b) posRel

        -- |isNegative operation: h \<-> k iff h is negative
        -- w.r.t k
        (<->) :: a -> a -> Bool
        a <-> b = elem (a,b) negRel

        -- |Generic comparing function for the structure. This is
        -- used in the Ord instance declaration of the structure
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








        
