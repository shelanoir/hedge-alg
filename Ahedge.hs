----------------------------------------------------
----------------------------------------------------
-- A hedge algebra:
-- Hedge is Ord, Eq and Enum
-- Hedge is Enum in the sense
-- that one can list it from
-- Possibly to Less
{-#LANGUAGE TemplateHaskell#-}
module Ahedge (
        Hedge(..),
        module HedgeClass,
) 
where
import Hgen
import HedgeClass
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
 
{-data Hedge = Possibly | Very | More | Less deriving (Eq, Show, Enum, Read)

instance (Ha Hedge) where
        posLs = [Very, More]
        posRel = [(Very,More), (Very,Less), (More,Very), (Very,Very), (More, More)]

--Eq Hedge = default-}
--Generating the type Hedge:
q

--Ord Hedge
instance Ord Hedge where
        compare = compare' 

--instace Show Hedge = default

----------------------------------------------------
----------------------------------------------------

