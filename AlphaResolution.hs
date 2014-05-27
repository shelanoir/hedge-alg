{-#LANGUAGE BangPatterns#-}
module AlphaResolution (
        module HedgeClass,
        module HedgeTruth,
        module ProsLogic,
        module Data.List,
        smartClause,
        confidence,
        resolvent',--resolvent :: (Ha hedge) => ([Lit hedge], Truth hedge) -> ([Lit hedge], Truth hedge) -> Maybe [([Lit hedge], Truth hedge)]
        resolution,
        resolution',
        resolution'',
        nilH,
        rmTau,
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
nilH = Lit "" Maxt

smartClause :: (Ha hedge) => CNF (Lit hedge) -> Truth hedge -> Clause hedge
smartClause (CNF lits) confi = (nub . sortLits $ lits, confi)

confidence conf1 conf2 t1 t2 
        | t1 >< t2 = conf1 `andH` conf2 `andH` (notH (t1 `andH` t2)) `andH` (t1 `orH` t2)




--resolution :: (Ha hedge) =>
--                [Clause hedge] -> Maybe (Truth hedge)
resolution xs = resolution' xs {-xs-} [] [] sympair
        where litsyms = nub [str|(lits, truth)<-xs, (Lit str tr) <- lits]
              litsymstrue = map (\x-> ([Lit x Mint],Maxt)) litsyms
              sympair = map (\x@([Lit str tr], maxt)-> (str, (resolution'' (x:kb) []))) litsymstrue               
              kb = init xs

                
resolution' allClauses resolved trace sympair 
        | saturizedRes == allClauses = (lookup [nilH] saturizedRes, trace)        
        | resolved == nextResolved = (lookup [nilH] saturizedRes, trace)
        | otherwise = resolution' saturizedRes nextResolved nextTrace sympair
        where 
              als = [(res,clause1,clause2) | clause1 <- allClauses, 
                                        clause2 <- allClauses,
                                        --(not $ (clause1,clause2) `elem` resolved) 
                                        -- && (not $ (clause2,clause1) `elem` resolved),
                                        let res = --trace("resolved" ++ show clause1++" (r$) "++ show clause2 ++ "\n")
                                               resolvent clause1 clause2
                                        ]                                        
              imm = [a | (Just a,clause1,clause2) <- als]
              rawRes = concat (allClauses : imm)  

              addTrace = [( res, (nub . sortLits $ lits1,conf1), (nub . sortLits $ lits2,conf2))
                         | (Just resL, (lits1,conf1), (lits2,conf2)) <- als, res <- resL]
              nextTrace = nub $ addTrace ++ trace           
              nextResolved = nub $ [((nub . sortLits $ lits1, conf1),(nub . sortLits $ lits2, conf2))| 
                                                (res,(lits1,conf1),(lits2,conf2)) <- als] ++ resolved
                                      -- ++ [((sortLits lits2, conf2),(sortLits lits1, conf1))|
                                      --           (res,(lits1,conf1),(lits2,conf2)) <- als]
                                                                                 
              {-nextGen = concat (allGen : imm)-}                          
              saturizedRes = nub [(l1, maximum ls2) | 
                                  (l1,c1) <- rawRes,
                                  let ls2 = [c2| (l2,c2) <- rawRes, l2 == l1]]
              resolvent ([Lit s1 t1], conf1) ([Lit s2 t2], conf2)        
                      | s1 == s2, t1 >< t2 = Just [([nilH], confidence conf1 conf2 t1 t2)]
                      | otherwise = Nothing

              resolvent (lits1, conf1) (lits2, conf2) 
                      | null result = Nothing
                      | otherwise = Just $ result
                      where resPairs
                              = [(lit1, lit2) | lit1 <- lits1, lit2 <- lits2, truthLit lit1 >< truthLit lit2,
                                      stringLit lit1 == stringLit lit2]
                            step (lit1, lit2) = ((delete lit1 lits1) ++ (delete lit2 lits2),
                                                 confidence conf1 conf2 (truthLit lit1) (truthLit lit2))
                            result = 
                                        map (\(a,b)->let (ls2,conf) = (nub . sortLits $ a, b)
                                                         grpByLit = groupBy (\(Lit str1 truth1) (Lit str2 truth2)
                                                                                -> str1 == str2) ls2
                                                                       
                                                         lsfinmax = ($grpByLit) $ map $ maximumBy 
                                                          (\(Lit str1 truth1) (Lit str2 truth2)
                                                                        -> case (istruelit str1) of
                                                                         True -> compare truth1 truth2
                                                                         False -> compare (notH truth1) (notH truth2))
                                                         --lsfinmin =  ($grpByLit) $ map $ minimumBy 
                                                         -- (\(Lit str1 truth1) (Lit str2 truth2)
                                                         lsfin = sortLits . nub $ lsfinmax           
                                                         
                                                        in (lsfin,conf)) . map step $ resPairs
                            {-generated x = x `elem` allGen-}
                            istruelit litsym = case res of                    
                                                     (Just (Just x)) -> True
                                                     (Just Nothing) -> False
                                                     _       -> False   
                                where res = lookup litsym sympair   



rmTau clauses = filter notTau clauses
        where notTau (ls,conf) = redundant == 0
                where redundant = length [str1 |(Lit str1 truth1)<-ls, (Lit str2 truth2) <-ls, str1==str2, truth1><truth2]
--sortListofclause ls = sortBy (\(l1,c1) (l2,c2) -> compare c1 c2) ls

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
resolution'' allClauses resolved        
        | saturizedRes == allClauses = lookup [nilH] saturizedRes        
        | resolved == nextResolved = lookup [nilH] saturizedRes
        | otherwise = resolution'' saturizedRes nextResolved
        where 
              als = [(res,clause1,clause2) | clause1 <- allClauses, 
                                        clause2 <- allClauses,
                                        let res = resolvent clause1 clause2
                                        ]
              imm = [a | (Just a,clause1,clause2) <- als]
              nextResolved = nub $ [((nub . sortLits $ lits1, conf1),(nub . sortLits $ lits2, conf2))| 
                                                (res,(lits1,conf1),(lits2,conf2)) <- als] ++ resolved
              rawRes = concat (allClauses : imm)  
              saturizedRes = nub [(l1, maximum ls2) | 
                                  (l1,c1) <- rawRes,
                                  let ls2 = [c2| (l2,c2) <- rawRes, l2 == l1]]
              resolvent ([Lit s1 t1], conf1) ([Lit s2 t2], conf2)        
                      | s1 == s2, t1 >< t2 = Just [([nilH], confidence conf1 conf2 t1 t2)]
                      | otherwise = Nothing

              resolvent (lits1, conf1) (lits2, conf2) 
                      | null result = Nothing
                      | otherwise = Just $ result
                      where resPairs
                              = [(lit1, lit2) | lit1 <- lits1, lit2 <- lits2, truthLit lit1 >< truthLit lit2,
                                      stringLit lit1 == stringLit lit2]
                            step (lit1, lit2) = ((delete lit1 lits1) ++ (delete lit2 lits2),
                                                 confidence conf1 conf2 (truthLit lit1) (truthLit lit2))
                            result = 
                                        map (\(a,b)->let (ls2,conf) = (nub . sortLits $ a, b)
                                                         grpByLit = groupBy (\(Lit str1 truth1) (Lit str2 truth2)
                                                                                -> str1 == str2) ls2
                                                                       
                                                         lsfinmax = ($grpByLit) $ map $ maximumBy 
                                                          (\(Lit str1 truth1) (Lit str2 truth2)
                                                                        -> compare truth1 truth2)
                                                         lsfinmin =  ($grpByLit) $ map $ minimumBy 
                                                          (\(Lit str1 truth1) (Lit str2 truth2)
                                                                        -> compare truth1 truth2)
                                                         lsfin = sortLits . nub $ (lsfinmax) -- ++ lsfinmin)          
                                                         
                                                        in (lsfin,conf)) . rmTau . map step $ resPairs

