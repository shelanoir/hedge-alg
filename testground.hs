import Util
import Triv
import KBio
import Hio
import ProsLogic
import Ahedge
import AlphaResolution
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
kbUnionGoal k g = zipWith smartClause (k ++ [g]) (repeat Maxt)


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
          , Lit "Russia's intervention is justified" $ Fals [Possibly, More], litA1, litB2, litB4, litB7, litB10], Maxt)
--------------------------------
kb = [cnf1, cnf2, cnf3, cnf4]
goal = cnf6
initialClauses = kbUnionGoal kb goal

destructive :: IORef Int -> IO ()
destructive io = modifyIORef io (+1)


--------------------------------------------------------------
printHedges dbname = do
        putStrLn ""
        putStrLn "Every hedges in the database:"
        conn <- connectSqlite3 dbname
        qQ <- quickQuery' conn "SELECT hedge FROM hedges" []
        let q = map (fromSql . head) qQ :: [String]
        print q
        putStrLn ""
        putStrLn ""
        putStrLn "Positive hedges:"
        print (posLs::[Hedge])
        putStrLn ""
        printPosH conn        
        putStrLn ""
        putStrLn ""
        putStrLn "Negative hedges:"
        print (negLs::[Hedge])
        putStrLn ""
        printNegH conn        
        putStrLn ""
        putStrLn ""
        putStrLn "Hedge actually in used:"
        print (hedgeLs::[Hedge])
        putStrLn ""
        putStrLn ""
        putStrLn "Positive relations:"                   
        print (posRel::[(Hedge,Hedge)])
        putStrLn ""
        putStrLn ""
        putStrLn "Negative relations:"                   
        print (negRel::[(Hedge,Hedge)])
        putStrLn ""
        disconnect conn

-------------------------------------------------
prove kb goal = resolution $ toClause kb ++ [smartClause goal Maxt]                                       
     
goalStr = "   Russia's intervention is justified   ::   Very True   " 

main = do
        arg <- getArgs
        case arg of
            ["--cli",dbname] -> do putStrLn "Command line mode starting..."
                                   cli dbname
            ("--gui":_)      -> do putStrLn "GUI mode starting..."                                       
            _                -> --putStrLn "usage: runhaskell <prog> [--cli|--gui] [dbname]"                     
                                cli "../test.db"
                
cli dbname = do
        --(dbname:rest) <- getArgs
        --let dbname = "../test.db"
        let menu = ["\n",
                    "==============================================================",
                    ">>= prove - prove the clause w.r.t. the knowledge base using\
                    \\n    Alpha Resolution",
                    ">>= print - print the knowledge base",
                    ">>= check - check for the consistency of the knowledge base",
                    ">>= add clause - add a new clause to the knowledge base",
                    ">>= delete clause - remove a clause from the knowledge base",
                    ">>= change clause - change a clause in the knowledge base",
                    ">>= hedge structure - print the information about the under-\
                     \\n    lying hedge algebra",
                    ">>= add positive - add a positive hedge",
                    ">>= add negative - add a negative hedge",
                    ">>= rm positive - remove a positive hedge",
                    ">>= rm negative - remove a negative hedge",  
                    ">>= rm hedge - completely remove a hedge",  
                    ">>= mv positive - move a positive hedge to another precedence",
                    ">>= mv negative- move a negative hedge to another precedence",
                    ">>= quit - exit program",
                    ">>= menu - print this menu",
                    "==============================================================",                    
                    "\n"
                    ]     
        mapM_ putStrLn menu

        forever $ do 
          putStrLn "\n\nYour wish is my command: \n"
          command <- readline'
   
          case command of
                  "menu"  -> mapM_ putStrLn menu
                  "quit"  -> exitImmediately ExitSuccess
                  "check" -> do
                          putStrLn "Checking ..."
                          knowledgebase <- getCNF dbname
                          let res = resolution'' (toClause knowledgebase) []
                          print res
                          case res of
                                  Nothing -> putStrLn "KB is consistent"
                                  Just x  -> putStrLn "[WARNING!!] KB is inconsistent"
                  "prove" -> do
                          putStrLn "What do you want me to prove for you?"
                          putStrLn "-- Please enter it in the format\n\
                          \ <statement> :: <truth-value> [AND|,|;] <statement> :: <truth-value> [AND|,|;]..."
                          input <- return parseGoals `ap` readline'
                          when ((Lit "" Maxt) `elem` input) $ putStrLn 
                                  "Seems like your proposition is ill-formatted"                                
                          print (CNF input)
                          knowledgebase <- getCNF dbname
                          putStrLn $ show knowledgebase
                          let res = prove knowledgebase (CNF input)
                          putStrLn $ show res
                          return ()                                        
                  "print" -> do
                          kb <- getCNF dbname
                          putStrLn $ show kb                               
                          
                  "add clause" -> do
                          putStrLn "Enter a clause: "
                          putStrLn "-- Please enter it in the format\n \
                            \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."
                          line <- readline'
                          addClause dbname line
                  "delete clause" -> do
                          conn <- connectSqlite3 dbname
                          putStrLn "Enter the clause to be deleted: "
                          line <- readline'
                          let inp = parseInpClause line
                          let sqlval = map (map toSql) inp 
                          print inp
                          q <- forM sqlval (\x -> quickQuery' conn "SELECT cid FROM conjLits where conjLits.lid in \
                                                  \ (SELECT lid FROM literal \
                                                   \ WHERE lstring = ? AND hedges = ? AND truthval = ?)" $ x)
                          let tq = map (map (fromSql . head)) q :: [[String]]
                          print tq
                          let cid = foldl1 intersect tq
                          unless (null tq || (length tq /= length inp)) $ do
                            if (cid /= [] && length cid == 1) then do
                              lids <- quickQuery' conn "SELECT lid FROM conjLits where conjLits.cid = ?" $ map toSql cid    
                              disconnect conn
                              if (length lids == length inp) then
                                       do deleteByCID (head . head $ tq :: String) dbname
                                   else putStrLn "Nothing has been done: no exact clause matched"            
                              else if (cid == []) then putStrLn "Nothing has been done: no clause matched"
                                      else if (length cid /= 1) then do
                                               putStrLn "Nothing has been done: multiple clause matched:"
                                               print cid
                                               putStrLn "Please try again with the Delete by id option"    
                                              else putStrLn "Nothing has been done"
                          disconnect conn                            
                          when (null tq || (length tq /= length inp)) $ putStrLn "Nothing has been done"            
                  "change clause" -> do
                          conn <- connectSqlite3 dbname
                          putStrLn "Enter the clause to be changed: "
                          line <- readline'
                          let inp = parseInpClause line
                          let sqlval = map (map toSql) inp 
                          print inp
                          q <- forM sqlval (\x -> quickQuery' conn "SELECT cid FROM conjLits where conjLits.lid in \
                                                  \ (SELECT lid FROM literal \
                                                   \ WHERE lstring = ? AND hedges = ? AND truthval = ?)" $ x)
                          let tq = map (map (fromSql . head)) q :: [[String]]
                          print tq
                          let cid = foldl1 intersect tq
                          unless (null tq || (length tq /= length inp)) $ do
                            if (cid /= [] && length cid == 1) then do
                              lids <- quickQuery' conn "SELECT lid FROM conjLits where conjLits.cid = ?" $ map toSql cid    
                              disconnect conn
                              if (length lids == length inp) then
                                       do putStrLn "Enter new clause: "
                                          putStrLn "-- Please enter it in the format\n \
                                                \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."               
                                          newclause <- readline'
                                          changeByCID (head . head $ tq :: String) newclause dbname
                                   else putStrLn "Nothing has been done: no exact clause matched"            
                              else if (cid == []) then putStrLn "Nothing has been done: no clause matched"
                                      else if (length cid /= 1) then do
                                               putStrLn "Nothing has been done: multiple clause matched:"
                                               print cid
                                               putStrLn "Please try again with the Change by id option"    
                                              else putStrLn "Nothing has been done"
                          disconnect conn                            
                          when (null tq || (length tq /= length inp)) $ putStrLn "Nothing has been done"            
                  "add positive" -> do putStrLn "Please enter the name of the hedge:"
                                       hedge' <- readline'
                                       putStrLn "What would this hedge's precedence value be?"
                                       pred <- readline'
                                       putStrLn "Remove from negative list if the hedge were already there? [y/n]"
                                       yN <- readline'
                                       addPosH dbname hedge' pred yN        
                                       selfRestart
                  "remove positive" -> do putStrLn "Please enter the name of the hedge:"
                                          hedge' <- readline'
                                          removePosH dbname hedge'         
                                          selfRestart
                  "add negative" -> do putStrLn "Please enter the name of the hedge:"
                                       hedge' <- readline'
                                       putStrLn "What would this hedge's precedence value be?"
                                       pred <- readline'
                                       putStrLn "Remove from positive list if the hedge were already there? [y/n]"
                                       yN <- readline'
                                       addNegH dbname hedge' pred yN        
                                       selfRestart
                  "rm negative" -> do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      removeNegH dbname hedge'         
                                      selfRestart
                  "rm hedge" ->    do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      removeH dbname hedge'         
                                      selfRestart
                  "mv positive" -> do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      putStrLn "What would this hedge's precedence value be?"
                                      pred <- readline'
                                      changePosOrd hedge' pred dbname
                                      selfRestart
                  "mv negative" -> do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      putStrLn "What would this hedge's precedence value be?"
                                      pred <- readline'
                                      changeNegOrd hedge' pred dbname
                                      selfRestart
                  "hedge structure" -> printHedges dbname                       
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
