module Main where

import Util.UtilA
import Util.UtilB
import DBio.KBio
import DBio.Hio
import ProsLogic
import Hedgen.Ahedge
import AlphaResolution
import Control.Exception

main = do
        arg <- getArgs
        print arg
        case arg of          
          ("--gui":dbname:_) -> do putStrLn "GUI mode starting..."
                    --             catch_cli dbname                    
          ("--cli":dbname:_) -> catch_cli dbname
          (x:_) | x == "--gui" || x == "--cli" -> return ()
          (dbname:_)         -> catch_cli dbname     
          _ -> do --putStrLn "Usage: runghc Main.hs <intefacing mode> <dbname>"
                  --exitImmediately $ ExitFailure (-1)                  
                  return ()
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
        putStrLn "\n\n[MAIN LOOP]"
        let menu = [
                    "==============================================================",
                    ">>= prove - prove the clause w.r.t. the knowledge base using\
                    \\n    Alpha Resolution",
                    ">>= kb manager - managing knowledge base",
                    ">>= hedge manager - managing hedges",
                    "--------------------------------------------------------------",
                    ">>= quit - exit program",
                    ">>= menu - print this menu",
                    "==============================================================",                    
                    "\n"
                    ]     
        mapM_ putStrLn menu

        forever $ do 
          putStrLn "\n\nYour wish is my command: \n"
          command <- readline'' "[MAIN LOOP]> "
   
          case command of
                  "menu"  -> mapM_ putStrLn menu
                  "kb manager" -> putStrLn "\n\n[KB MANAGER]" >> mapM_ putStrLn cmenu >> kbManager dbname
                  "prove" -> do
                          putStrLn "What do you want me to prove for you?"
                          putStrLn "-- Please enter it in the format\n\
                          \ <statement> :: <truth-value> [AND|,|;] <statement> :: <truth-value> [AND|,|;]..."
                          input <- return parseGoals `ap` readline'' "[PROVE]> "
                          when ((Lit "" Maxt) `elem` input) $ putStrLn 
                                  "Seems like your proposition is ill-formatted"                                
                          print (CNF input)

                          putStrLn "Explain the result? [y/n]"
                          inputM <- (liftM $map toUpper) $ readline'' "[EXPLAIN?]> "
                          knowledgebase <- getCNF dbname
                          putStrLn $ show knowledgebase
                          let (res,traceL) = prove knowledgebase (CNF input)
                          case res of
                            Nothing -> putStrLn $ show res ++ ": the proposition is not provable"
                            (Just x)   -> putStrLn $ "The confidence is: " ++ show x
                          --putStrLn $ show res
                          unless (res == Nothing || inputM /= "Y") $
                            do let (Just ress) = res
                               let tracL = retrace (nil,ress) traceL []
                               putStrLn tracL                                     
                          --print trace
                          return ()                                        
                  "quit"  -> exitImmediately ExitSuccess
                  "hedge manager" -> do
                                       putStrLn "[HEDGE MANAGER]" 
                                       mapM_ putStrLn hMMenu
                                       hManager dbname
                  _ -> putStrLn "please enter something meaningful"
                        
cmenu = [
         "==============================================================",
         ">>= cmenu- print this menu",
         ">>= print - print the knowledge base",
         ">>= check - check for the consistency of the knowledge base",
         ">>= hedge structure - print the information about the under-\
          \\n    lying hedge algebra",
         "--------------------------------------------------------------",
         ">>= add clause - add a new clause to the knowledge base",
         ">>= delete clause - remove a clause from the knowledge base",
         ">>= change clause - change a clause in the knowledge base",
         "--------------------------------------------------------------",
         ">>= back - go back to main menu",
         "==============================================================",
         "\n"                    
         ]
kbManager dbname = do
          putStrLn "\n\nYour wish is my command: \n"
          command <- readline'' "[KB MANAGER]> "   
          case command of
                  "cmenu" -> mapM_ putStrLn cmenu >> getLine >> kbManager dbname         
                  "check" -> do
                          putStrLn "Checking ..."
                          knowledgebase <- getCNF dbname
                          let res = resolution'' (toClause knowledgebase) []
                          --print res
                          case res of
                                  Nothing -> putStrLn "\nKB is consistent"
                                  Just x  -> putStrLn "\n[WARNING!!] KB is inconsistent"
                          kbManager dbname        
                  "print" -> do
                          kb <- getCNF dbname
                          let kbb = map (\(CNF x) -> x) kb
                          let res = map (map (\x -> show x)) kbb
                          let ress = map (concat . intersperse " OR ") res
                          mapM_ (\x-> print x >> putStrLn "") ress
                          --print kb                               
                          q <- getLine
                          kbManager dbname        
                  "add clause" -> do
                          putStrLn "Enter a clause: "
                          putStrLn "-- Please enter it in the format\n \
                            \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."
                          line <- readline'' "[ADD CLAUSE]> "   
                          addClause dbname line
                          kbManager dbname        
                  "delete clause" -> do
                          putStrLn "Enter the clause to be deleted: "
                          line <- readline'' "[DELETE CLAUSE]> "   
                          deleteClause dbname line
                          kbManager dbname        
                  "change clause" -> do
                          putStrLn "Enter the clause to be changed: "
                          line <- readline'' "[CHANGE CLAUSE]> "  
                          putStrLn "Enter new clause: "
                          putStrLn "-- Please enter it in the format\n \
                           \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."             
                          newclause <- readline'' "[CHANGE CLAUSE]> " 
                          changeClause dbname line newclause
                          kbManager dbname        
                  "hedge structure" -> printHedges dbname >> getLine >> kbManager dbname        
                  "back" -> return ()                     
                  _ -> putStrLn "Please enter something meaningful" >> kbManager dbname
