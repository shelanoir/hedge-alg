module Util.UtilB where
import Data.IORef
import Data.List.Split
import Data.Char

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Environment
import System.Console.Readline

import Control.Concurrent
import Control.Monad

--require Data.Char
properFormat :: String -> String
properFormat [] = []
properFormat hedgestr = toUpper (head hedgestr) : map toLower (tail hedgestr)

properTruthString :: String -> [String]
properTruthString [] = []
properTruthString (x:xs) = map properFormat . splitOn " " $ (x:xs)

--convert SQL query result to haskell String
fromQuery :: [[SqlValue]] -> [[String]]
fromQuery = map (map (fromSql :: SqlValue->String)) 

readline' :: IO String
readline' = do maybesmt <- readline ">>= "        
               case maybesmt of
                Nothing -> return ""
                Just x -> addHistory x >> return x   

