module AlphaResolution (
        module HedgeClass,
        module HedgeTruth,
        module ProsLogic,
        module Data.List,
        smartClause,
        confidence,
        resolvent,--resolvent :: (Ha hedge) => ([Lit hedge], Truth hedge) -> ([Lit hedge], Truth hedge) -> Maybe [([Lit hedge], Truth hedge)]
        resolution,
        nilH,
        Clause(..)
)
where
import Data.List
import HedgeClass
import HedgeTruth
import ProsLogic
--alpha-resolution
--
type Clause hedge = ([Lit hedge], Truth hedge)
nilH :: (Ha hedge) => Lit hedge
nilH = Lit "" MaxT

smartClause :: (Ha hedge) => CNF (Lit hedge) -> Truth hedge -> Clause hedge
smartClause (CNF lits) confi = (lits, confi)

confidence conf1 conf2 t1 t2 
        | t1 >< t2 = conf1 `andH` conf2 `andH` (notH (t1 `andH` t2)) `andH` (t1 `orH` t2)


resolvent :: (Ha hedge) => Clause hedge -> Clause hedge -> Maybe [(Clause hedge)]
resolvent ([Lit s1 t1], conf1) ([Lit s2 t2], conf2)
        | s1 == s2, t1 >< t2 = Just [([nilH], confidence conf1 conf2 t1 t2)]
        | otherwise = Nothing

resolvent (lits1, conf1) (lits2, conf2) 
        | null result = Nothing
        | otherwise = Just result
        where resPairs
                = [(lit1, lit2) | lit1 <- lits1, lit2 <- lits2, truthLit lit1 >< truthLit lit2,
                        stringLit lit1 == stringLit lit2]
              step (lit1, lit2) = ((delete lit1 lits1) ++ (delete lit2 lits2),
                                   confidence conf1 conf2 (truthLit lit1) (truthLit lit2))     
              result = map (\(a,b)->(sortLits a, b)) . map step $ resPairs                


resolution :: (Ha hedge) =>
                [Clause hedge] -> Maybe (Truth hedge)
resolution allClauses  
        | saturizedRes == allClauses = lookup [nilH] saturizedRes
        | otherwise = resolution saturizedRes
        where rawRes = concat (allClauses : [a |Just a <- [res | clause1 <- allClauses, 
                                        clause2 <- allClauses,
                                        let res = resolvent clause1 clause2
                                        ]])
              saturizedRes = nub [(l1, maximum ls2) | 
                                  (l1,c1) <- rawRes,
                                  let ls2 = [c2| (l2,c2) <- rawRes, l2 == l1]]
                                                                

{-nonAlpha :: (Ha hedge) =>
                [Clause hedge] -> Maybe [(Truth hedge)]
nonAlpha allClauses  
        | saturizedRes == allClauses = filter (\([nilH] saturizedRes
        | otherwise = resolution saturizedRes
        where rawRes = concat (allClauses : [a |Just a <- [res | clause1 <- allClauses, 
                                        clause2 <- allClauses,
                                        let res = resolvent clause1 clause2
                                        ]])
              saturizedRes = nub res 
-}
