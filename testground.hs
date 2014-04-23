import Util
import KBio
----------------------------------------------------
--- TODO:
--- tracing/explaining feature
--- user interface
----------------------------------------------------
----------------------------------------
----------------------------------------
truth1 = Fals [Very,Very,Very,More]
truth2 = Fals [Very,More,Possibly]
res1 = compare truth1 truth2        
res2 = compare truth2 truth1
kbUnionGoal k g = zipWith smartClause (k ++ [g]) (repeat MaxT)


---------testing data-----------
litA1 = Lit "America is behind the Ukraine's crisis" (Fals [More])
litB2 = Lit "Russia's intervention does not violate the International Law" (Fals [])
litC3 = Lit "Russia's intervention is justified" (Tru [Very, More])
litB4 = Lit "Russia's intervention does not violate the International Law" (Tru [Less])
litC5 = Lit "Russia's intervention is justified" (Tru [Possibly])
litA6 = Lit "America is behind the Ukraine's crisis" (Tru [Possibly])
litB7 = Lit "Russia's intervention does not violate the International Law" (Tru [Very])

litC8 = Lit "Russia's intervention is justified" (Fals [Very]) --negated goal
litC9 = Lit "America is behind the Ukraine's crisis" (Fals [Possibly])
litB10 = Lit "Russia's intervention does not violate the International Law" (Fals [More])

cnf1 = smartCNF [litA1, litB2, litC3]
cnf2 = smartCNF [litB4, litC5]
cnf3 = smartCNF [litA6]
cnf4 = smartCNF [litB7]
cnf5 = smartCNF [litC8] -- negated goal
cnf6 = smartCNF [litC9]
cnf7 = smartCNF [litC8,litC9,litB10]
clause::Clause Hedge
clause = ([litC8, litC5, litC3, Lit "Russia's intervention is justified" $ Tru [Very, Possibly, More]
          , Lit "Russia's intervention is justified" $ Fals [Very, More]
          , Lit "Russia's intervention is justified" $ Fals [Possibly, More], litA1, litB2, litB4, litB7, litB10], MaxT)
--------------------------------
kb = [cnf1, cnf2, cnf3, cnf4]
goal = cnf6
initialClauses = kbUnionGoal kb goal

destructive :: IORef Int -> IO ()
destructive io = modifyIORef io (+1)



-------------------------------------------------
prove kb goal = resolution $ toClause kb ++ [smartClause goal MaxT]                                       
     
goalStr = "   Russia's intervention is justified   ::   Very True   " 

main = test1
test2 = do
        --(dbname:rest) <- getArgs
        let dbname = "../cnf.db"
        putStrLn "Please choose:"
        command <- readline'
        case command of
                "prove" -> do
                        putStrLn "What do you want me to prove for you?"
                        putStrLn "-- Please enter it in the format\n\
                        \ <statement> :: <truth-value> [AND|,|;] <statement> :: <truth-value> [AND|,|;]..."
                        input <- return parseGoals `ap` readline'
                        when ([Lit "" MaxT] `elem` (map lsCNF input)) $ putStrLn 
                                "Seems like your proposition is ill-formatted"                                
                        putStrLn $ show input
                        knowledgebase <- getCNF dbname
                        putStrLn $ show knowledgebase
                        let res = map (\x-> prove knowledgebase x) input
                        putStrLn $ show res
                        return ()                                        
                "print" -> do
                        kb <- getCNF dbname
                        putStrLn $ show kb                               
                _ -> putStrLn "please enter something meaningful"
        

test1 = do
        io <- newIORef 3
        destructive io
        a <- readIORef io
        putStr $ show a ++ "\n"
        putStrLn $ show $ resolution $ kbUnionGoal kb goal
        putStrLn "Enter something, anything!"
        str <- getLine
        arg <- getArgs
        putStrLn $ "arguments: " ++ concat arg
        putStrLn "Commencing restart..."
        selfRestart

optimize :: (Ha hedge) => Clause hedge -> Clause hedge
optimize = (\(a,b)->let (ls2,conf) = (nub . sortLits $ a, b)
                        grpByLit = groupBy (\(Lit str1 truth1) (Lit str2 truth2)
                         -> str1 == str2) ls2 
                        lsfinmax = ($grpByLit) $ map $ maximumBy 
                          (\(Lit str1 truth1) (Lit str2 truth2)
                                        -> compare truth1 truth2)
                        lsfinmin =  ($grpByLit) $ map $ minimumBy 
                          (\(Lit str1 truth1) (Lit str2 truth2)
                                        -> compare truth1 truth2)
                        lsfin = nub (lsfinmax ++ lsfinmin)          

                      in (sortLits lsfin,conf)) 
