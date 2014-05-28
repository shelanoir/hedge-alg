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
                  "quit"  -> exitImmediately ExitSuccess
                  "check" -> do
                          putStrLn "Checking ..."
                          knowledgebase <- getCNF dbname
                          let res = resolution'' (toClause knowledgebase) []
                          print res
                          case res of
                                  Nothing -> putStrLn "KB is consistent"
                                  Just x  -> putStrLn "[WARNING!!] KB is inconsistent"
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
                        

