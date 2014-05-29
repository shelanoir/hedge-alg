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
                        
