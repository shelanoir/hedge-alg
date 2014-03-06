----------------------------------------------------
----------------------------------------------------
--A hedge algebra:
--implementations for ord
--and show went here
-- Hedge is Ord, Eq and Enum
-- Hedge is Enum in the sense
-- that one can list it from
-- Possibly to Less
module Ahedge (
        Hedge(..),
        module HedgeClass,
        module Data.List
) 
where
import HedgeClass
import Data.List

data Hedge = Possibly | Very | More | Less deriving (Eq, Show, Enum)

instance (Ha Hedge) where
        posLs = [Very, More]
        posRel = [(Very,More), (Very,Less), (More,Very), (Very,Very), (More, More)]

--Eq Hedge = default

--Ord Hedge
instance Ord Hedge where
        compare = compare' 

--instace Show Hedge = default

----------------------------------------------------
----------------------------------------------------

