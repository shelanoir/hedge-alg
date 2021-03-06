module Util.UtilA (
module Util.UtilA,
--module ProsLogic,
--module Ahedge,
--module AlphaResolution,

module Data.IORef,
module Data.List.Split,
module Data.Char,

module Database.HDBC,
module Database.HDBC.Sqlite3,

module System.Environment,
module System.Console.Readline,

module Control.Concurrent,
module Control.Monad,
module SelfRestart,
module Util.UtilB,
module DBio.Hio,
) 
where
import ProsLogic
import Hedgen.Ahedge
import AlphaResolution

import Data.IORef
import Data.List.Split
import Data.Char

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Environment
import System.Console.Readline

import Control.Concurrent
import Control.Monad
import Util.UtilB
import DBio.Hio
import SelfRestart (selfRestart, forkSelfRestartExePollWithAction, exitImmediately, ExitCode(..))

okTruthStr str = case (hedges,seed) of 
                  (_,"Maxt") -> True
                  (_,"Mint") -> True
                  (x,y)      -> ok_hedges && ok_seed
        where ls = map show (hedgeLs::[Hedge])
              truths = ["True", "False", "Maxt", "Mint"]
              truth_string = properTruthString str   
              hedges = init truth_string
              seed = last truth_string
              ok_hedges = foldl step True hedges
                where step bool h = bool && h `elem` ls
              ok_seed = seed `elem` truths  

toClause lits = zipWith smartClause lits $ repeat Maxt

printClauses xs = do
                let clauses = map (\(a,b) -> CNF a) xs
                putStrLn ""
                forM_ clauses (\x -> do
                                        let str = show x
                                        putStrLn str)
printLits xs = do
                let ys = toClause xs
                printClauses ys                


prove kb goal = resolution $ toClause kb ++ [smartClause goal Maxt]                                       
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
        

nil = [nilH :: Lit Hedge]
headIs h (a,b,c) = h == a
findHead h tracL = find (headIs h) tracL

retrace h tracL visited
   | h `elem` visited = ""
   | otherwise = 
      case findM of
        Nothing -> ""
        Just (a,b,c)
          -> r1 ++ r2 ++ r0
           where r0 = "\n\n\n"++(show b) ++ "\n`resolvedWith`\n" ++ (show c) ++ "\n\n  => " ++ (show a)
                 r1 = retrace b tracL nextV
                 r2 = retrace c tracL nextV
                 nextV = h:visited                            
       where findM = findHead h tracL
