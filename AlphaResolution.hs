module AlphaResolution (
        module HedgeClass,
        module HedgeTruth,
        module ProsLogic,
        module Data.List,
        smartClause,
        confidence,
        resolvent',--resolvent :: (Ha hedge) => ([Lit hedge], Truth hedge) -> ([Lit hedge], Truth hedge) -> Maybe [([Lit hedge], Truth hedge)]
        resolution,
--        resolution',
        nilH,
        Debug.Trace.trace,
        Clause(..)
)
where
import Data.List
import HedgeClass
import HedgeTruth
import ProsLogic
import Debug.Trace
--alpha-resolution
--
type Clause hedge = ([Lit hedge], Truth hedge)
nilH :: (Ha hedge) => Lit hedge
nilH = Lit "" MaxT

smartClause :: (Ha hedge) => CNF (Lit hedge) -> Truth hedge -> Clause hedge
smartClause (CNF lits) confi = (nub . sortLits $ lits, confi)

confidence conf1 conf2 t1 t2 
        | t1 >< t2 = conf1 `andH` conf2 `andH` (notH (t1 `andH` t2)) `andH` (t1 `orH` t2)




resolution :: (Ha hedge) =>
                [Clause hedge] -> Maybe (Truth hedge)
resolution xs = resolution' xs xs []
                
resolution' allClauses allGen resolved        
        {-| trace ("[\n\n\nRESOLUTION]\n All = \n"
                 ++ show (nub allClauses)
                 ++"\n Raw = Saturized? \n" 
                 ++ show (rawRes == saturizedRes)
                 ++ "\n Saturized = \n" 
                 ++ show allClauses                 
                 ) False = undefined-}
        | saturizedRes == allClauses = lookup [nilH] saturizedRes        
        | resolved == nextResolved = lookup [nilH] saturizedRes
        | otherwise = resolution' saturizedRes nextGen nextResolved
        where {-imm = [a |Just a <- [res | clause1 <- allClauses, 
                                        clause2 <- allClauses,
                                        let res = resolvent clause1 clause2
                                        ]]-}
              als = [(res,clause1,clause2) | clause1 <- allClauses, 
                                        clause2 <- allClauses,
                                        let res = resolvent clause1 clause2
                                        ]
              imm = [a | (Just a,clause1,clause2) <- als]
              nextResolved = nub $ [((nub . sortLits $ lits1, conf1),(nub . sortLits $ lits2, conf2))| 
                                                (res,(lits1,conf1),(lits2,conf2)) <- als] ++ resolved
                                      -- ++ [((sortLits lits2, conf2),(sortLits lits1, conf1))|
                                      --           (res,(lits1,conf1),(lits2,conf2)) <- als]
                                                                                 
              nextGen = concat (allGen : imm)                          
              rawRes = concat (allClauses : imm)  
              saturizedRes = nub [(l1, maximum ls2) | 
                                  (l1,c1) <- rawRes,
                                  let ls2 = [c2| (l2,c2) <- rawRes, l2 == l1]]
              resolvent a b -- | trace ((show (a,b)) ++ " -> " ++ (show $ (a,b) `elem` resolved) ++ "\n\n" ++ show resolved ++ "\n\n\n\n")
--                                 False = undefined
                            | (a,b) `elem` resolved || (b,a) `elem` resolved = Nothing  
              resolvent ([Lit s1 t1], conf1) ([Lit s2 t2], conf2)        
                      | s1 == s2, t1 >< t2 = Just [([nilH], confidence conf1 conf2 t1 t2)]
                      | otherwise = Nothing

              resolvent (lits1, conf1) (lits2, conf2) 
--                      | trace ("\n\n\n" ++ (show $ length resolved)) False = undefined 
                      | null result = Nothing
                      | otherwise = Just result
                      where resPairs
                              = [(lit1, lit2) | lit1 <- lits1, lit2 <- lits2, truthLit lit1 >< truthLit lit2,
                                      stringLit lit1 == stringLit lit2]
                            step (lit1, lit2) = ((delete lit1 lits1) ++ (delete lit2 lits2),
                                                 confidence conf1 conf2 (truthLit lit1) (truthLit lit2))
                            result = filter (not . generated) $ 
                                        map (\(a,b)->let (ls2,conf) = (nub . sortLits $ a, b)
                                                         grpByLit = groupBy (\(Lit str1 truth1) (Lit str2 truth2)
                                                                                -> str1 == str2) ls2
                                                                       
                                                         lsfinmax = ($grpByLit) $ map $ maximumBy 
                                                          (\(Lit str1 truth1) (Lit str2 truth2)
                                                                        -> compare truth1 truth2)
                                                         lsfinmin =  ($grpByLit) $ map $ minimumBy 
                                                          (\(Lit str1 truth1) (Lit str2 truth2)
                                                                        -> compare truth1 truth2)
                                                         lsfin = sortLits . nub $ (lsfinmax ++ lsfinmin)          
              
                                                        in (lsfin,conf)) . map step $ resPairs
                            generated x = x `elem` allGen

--sortListofclause ls = sortBy (\(l1,c1) (l2,c2) -> compare c1 c2) ls
                        

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
resolvent' :: (Ha hedge) => Clause hedge -> Clause hedge -> Maybe [(Clause hedge)]
resolvent' ([Lit s1 t1], conf1) ([Lit s2 t2], conf2)        
        | s1 == s2, t1 >< t2 = Just [([nilH], confidence conf1 conf2 t1 t2)]
        | otherwise = Nothing

resolvent' (lits1, conf1) (lits2, conf2) 
        | null result = Nothing
        | otherwise = Just result
        where resPairs
                = [(lit1, lit2) | lit1 <- lits1, lit2 <- lits2, truthLit lit1 >< truthLit lit2,
                        stringLit lit1 == stringLit lit2]
              step (lit1, lit2) = ((delete lit1 lits1) ++ (delete lit2 lits2),
                                   confidence conf1 conf2 (truthLit lit1) (truthLit lit2))     
              result = nub $ map (\(a,b)->(nub . sortLits $ a, b)) . map step $ resPairs                
{-resolvent a b | trace ("\n\n\n[RESOLVENT]: a = \n" ++ show a
                        ++ " b = \n" ++ show b
                        ) False = undefined-}

