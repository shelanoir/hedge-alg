module AlphaResolution (
        module HedgeClass,
        module HedgeTruth,
        module ProsLogic,
        module Data.List,
        confidence,
        resolvent,
        resolution,
        nilH
)
where
import Data.List
import HedgeClass
import HedgeTruth
import ProsLogic
--alpha-resolution
--
confidence conf1 conf2 t1 t2 
        | t1 >< t2 = conf1 `andH` conf2 `andH` (notH (t1 `andH` t2)) `andH` (t1 `orH` t2)

nilH :: (Ha hedge) => Lit hedge
nilH = Lit "" MaxT

resolvent :: (Ha hedge) => 
                ([Lit hedge], Truth hedge) 
                -> ([Lit hedge], Truth hedge) 
                -> Maybe [([Lit hedge], Truth hedge)]

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
              result = map (\(a,b)->(sortCNF a, b)) . map step $ resPairs                

resolution :: (Ha hedge) =>
                [([Lit hedge], Truth hedge)] -> Maybe (Truth hedge)
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
                                                                

