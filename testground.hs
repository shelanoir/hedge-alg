import Util
import UtilB
import KBio
import Hio
import ProsLogic
import Ahedge
import AlphaResolution
import Control.Exception
----------------------------------------------------
--- TODO:
--- tracing/explaining feature
--- user interface
----------------------------------------------------
----------------------------------------
----------------------------------------
{-truth1 = Fals [Very,Very,Very,More]
truth2 = Fals [Very,More,Possibly]
res1 = compare truth1 truth2        
res2 = compare truth2 truth1-}
{-kbUnionGoal k g = zipWith smartClause (k ++ [g]) (repeat Maxt)


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
goal = cnf7
initialClauses = kbUnionGoal kb goal-}

destructive :: IORef Int -> IO ()
destructive io = modifyIORef io (+1)
--(Just res,traceL) = prove kb goal
--------------------------------------------------------------

-------------------------------------------------
     
goalStr = "   Russia's intervention is justified   ::   Very True   " 

main = do
        arg <- getArgs
        print arg
        case arg of          
          ("--gui":dbname:_) -> do putStrLn "GUI mode starting..."
                    --             catch_cli dbname                    
          ("--cli":dbname:_) -> catch_cli dbname
          (x:_) | x == "--gui" || x == "--cli" -> catch_cli "../test.db"
          (dbname:_)         -> catch_cli dbname     
          _ -> do --putStrLn "Usage: runghc Main.hs <intefacing mode> <dbname>"
                  --exitImmediately $ ExitFailure (-1)                  
                  catch_cli "../test.db"
         where catch_cli dbname = do 
                                   putStrLn "CLI mode starting..."                                     
                                   cli dbname `catch` 
                                      ((\e ->do print e
                                                putStrLn "Exception raised"
                                                putStrLn "back to main menu or quit? [m/q]"
                                                ansM <- readline'
                                                let ans = map toLower ansM
                                                case ans of
                                                  "m" -> selfRestart
                                                  "q" -> exitImmediately $ ExitFailure (-1)
                                                  _   -> do putStrLn "default: exit program"
                                                            exitImmediately $ ExitFailure (-1)
                                        ):: SomeException->IO ())
cli dbname = do
        --(dbname:rest) <- getArgs
        --let dbname = "../test.db"
        let menu = ["\n",
                    "==============================================================",
                    ">>= prove - prove the clause w.r.t. the knowledge base using\
                    \\n    Alpha Resolution",
                    ">>= print - print the knowledge base",
                    ">>= check - check for the consistency of the knowledge base",
                    ">>= hedge structure - print the information about the under-\
                     \\n    lying hedge algebra",
                    "--------------------------------------------------------------",
                    ">>= add clause - add a new clause to the knowledge base",
                    ">>= delete clause - remove a clause from the knowledge base",
                    ">>= change clause - change a clause in the knowledge base",
                    "--------------------------------------------------------------",
                    ">>= hedge manager - changing hedges",
                    "--------------------------------------------------------------",
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

                          putStrLn "Explain the result? [y/n]"
                          inputM <- (liftM $map toUpper) readline'

                          knowledgebase <- getCNF dbname
                          putStrLn $ show knowledgebase
                          let (res,traceL) = prove knowledgebase (CNF input)
                          putStrLn $ show res
                          unless (res == Nothing || inputM /= "Y") $
                            do let (Just ress) = res
                               let tracL = retrace (nil,ress) traceL []
                               putStrLn tracL                                     
                          --print trace
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
                          putStrLn "Enter the clause to be deleted: "
                          line <- readline'
                          deleteClause dbname line
                  "change clause" -> do
                          putStrLn "Enter the clause to be changed: "
                          line <- readline'
                          putStrLn "Enter new clause: "
                          putStrLn "-- Please enter it in the format\n \
                           \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."             
                          newclause <- readline'
                          changeClause dbname line newclause
                  "hedge structure" -> printHedges dbname                       
                  "hedge manager" -> do
                                       mapM_ putStrLn hMMenu
                                       hManager dbname
                  _ -> putStrLn "please enter something meaningful"
                        

