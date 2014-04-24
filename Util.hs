module Util (
module Util,
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
module Triv
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
import Triv
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


        

