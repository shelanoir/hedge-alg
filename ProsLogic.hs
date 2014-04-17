module ProsLogic (
        Lit(..),
        CNF(..),
        truthLit,
        stringLit,
        lsCNF,
        smartCNF,
        sortCNF,
        sortLits,
        negateLit,
        module HedgeTruth,
        module HedgeClass,
        module Data.List
) where
import HedgeTruth
import HedgeClass
import Data.List
----------------------------------------------------
----------------------------------------------------
-- Literal and CNF clause:
-- Literal = atomic sentence with degree of truth
-- CNF clause of literals = list of literals

---Literal:
data Lit hedge = Lit String (Truth hedge) deriving (Eq, Read)
instance Show hedge => Show (Lit hedge) where
        show (Lit x truth) = "<" ++ x ++ ": " ++ show truth ++ ">"

truthLit (Lit string truth) = truth 
stringLit (Lit string truth) = string

data CNF a = CNF [a] 
instance Show a => Show (CNF a) where
        show (CNF [x]) = show x ++ "\n"
        show (CNF (x:xs)) = show x ++  " OR " ++ (show (CNF xs)) 

lsCNF (CNF a) = a
negateLit (Lit str truth) = Lit str (notH truth)
smartCNF a = CNF (sortLits a)
sortCNF (CNF a) = CNF (sortLits a)
-- Lists of Lits need to be sorted for ease of comparison
sortLits :: (Ha hedge) => [Lit hedge] -> [Lit hedge] 
sortLits = sortBy (\(Lit string1 t1) (Lit string2 t2) ->
                        case () of  
                            _   | string1 == string2 -> compare t1 t2
                                | otherwise -> compare string1 string2)
       


