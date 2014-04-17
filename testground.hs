import ProsLogic
import Ahedge
import AlphaResolution
import Data.IORef

import Data.List.Split
import Data.Char

import Control.Concurrent
import Control.Monad
import SelfRestart (selfRestart, forkSelfRestartExePollWithAction)

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Environment
import System.Console.Readline
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

toClause lits = zipWith smartClause kb $ repeat MaxT
        
              
kbUnionGoal k g = zipWith smartClause (k ++ [g]) (repeat MaxT)
printClauses xs = do
                let clauses = map (\(a,b) -> CNF a) xs
                putStrLn ""
                forM_ clauses (\x -> do
                                        let str = show x
                                        putStrLn str)
printLits xs = do
                let ys = toClause xs
                printClauses ys                

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
cnf6 = smartCNF [litC9,litB10]
cnf7 = smartCNF [litC8,litC9,litB10]
--------------------------------
kb = [cnf1, cnf2, cnf3, cnf4]
goal = cnf6
initialClauses = kbUnionGoal kb goal

destructive :: IORef Int -> IO ()
destructive io = modifyIORef io (+1)
--require Data.Char
properFormat [] = []
properFormat hedgestr = toUpper (head hedgestr) : map toLower (tail hedgestr)

properTruthString [] = []
properTruthString (x:xs) = map properFormat . splitOn " " $ (x:xs)

--convert SQL query result to haskell String
fromQuery :: [[SqlValue]] -> [[String]]
fromQuery res = map (map (fromSql :: SqlValue->String)) res

--get the cids of CNF in db
getCid :: String -> IO [String]
getCid db = do
        conn <- connectSqlite3 db
        quer <- quickQuery' conn "SELECT cid FROM conj" []
        let res = join . fromQuery $ quer
        disconnect conn
        return res

--get the list of CNF
getCNF :: String -> IO [CNF (Lit Hedge)]
getCNF db = do
        cids <- getCid db
        conn <- connectSqlite3 db
        ret<-forM cids $ \x -> do 
                rawLits <-liftM fromQuery $ quickQuery' 
                        conn 
                        "SELECT lstring, truthval, hedges \
                        \FROM literal JOIN conjLits ON conjLits.lid = literal.lid \
                        \WHERE conjLits.cid = ?" 
                        [toSql x]
                let step [lstring, lseed, lhedges] =
                        Lit lstring $ truthval
                        where 
                              seed = properFormat lseed
                              hedges = properTruthString lhedges  
                              hList = map (read :: String -> Hedge) hedges
                              truthval = case seed of
                                        "Tru" -> Tru hList
                                        "Fals" -> Fals hList
                                        "MaxT" -> MaxT
                                        _ -> MinT
                return $ smartCNF  $ map step rawLits
        disconnect conn
        return ret
prove kb goal = resolution $ toClause kb ++ [smartClause goal MaxT]                                       
     
goalStr = "   Russia's intervention is justified   ::   Very True   " 

parseGoal:: String -> CNF (Lit Hedge)
parseGoal inp = case seed of
                        "True" -> CNF [Lit lstring (Fals hedges)]
                        "False" -> CNF [Lit lstring (Tru hedges)]
                        "MaxT" -> CNF [Lit lstring MinT]
                        _ -> CNF [Lit lstring MaxT]                
        where [lstring,truthstring] = case (map (reverse . dropWhile (\x->x==' ') . reverse . dropWhile (\x->x==' '))
                                          $ splitOn "::" inp) of
                                          [lstring, truthstring] -> [lstring,truthstring]
                                          _ -> ["", "MaxT"]
              hstring = properTruthString truthstring                
              hedges = map read (init hstring) ::(Ha h) => [h]
              seed = last hstring                                                      
parseGoals :: String -> [CNF (Lit Hedge)]              
parseGoals inp =  map parseGoal ls
        where ls = join . map (splitOn ";") . join . map (splitOn ",") . splitOn "AND" $ inp                


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
                        kb <- getCNF dbname
                        let res = map (\x-> prove kb x) input
                        putStrLn $ show res
                        return ()                                        
                _ -> putStrLn "please enter something meaningful"
        where readline' = do maybesmt <- readline ">>= "        
                             case maybesmt of
                                Nothing -> return ""
                                Just x -> addHistory x >> return x   
        

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

