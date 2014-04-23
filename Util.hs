module Util (
module Util,
module ProsLogic,
module Ahedge,
module AlphaResolution,

module Data.IORef,
module Data.List.Split,
module Data.Char,

module Database.HDBC,
module Database.HDBC.Sqlite3,

module System.Environment,
module System.Console.Readline,

module Control.Concurrent,
module Control.Monad,
module SelfRestart
) 
where
import ProsLogic
import Ahedge
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

import SelfRestart (selfRestart, forkSelfRestartExePollWithAction)

toClause lits = zipWith smartClause lits $ repeat MaxT

printClauses xs = do
                let clauses = map (\(a,b) -> CNF a) xs
                putStrLn ""
                forM_ clauses (\x -> do
                                        let str = show x
                                        putStrLn str)
printLits xs = do
                let ys = toClause xs
                printClauses ys                

--require Data.Char
properFormat [] = []
properFormat hedgestr = toUpper (head hedgestr) : map toLower (tail hedgestr)

properTruthString [] = []
properTruthString (x:xs) = map properFormat . splitOn " " $ (x:xs)

--convert SQL query result to haskell String
fromQuery :: [[SqlValue]] -> [[String]]
fromQuery res = map (map (fromSql :: SqlValue->String)) res

readline' = do maybesmt <- readline ">>= "        
               case maybesmt of
                Nothing -> return ""
                Just x -> addHistory x >> return x   

        

