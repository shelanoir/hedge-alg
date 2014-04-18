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

toClause lits = zipWith smartClause lits $ repeat MaxT
        
              
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
cnf6 = smartCNF [litC9]
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


------------------------
--get all conj from db
------------------------
--get the cids of CNF in db
getCid :: String -> IO [String]
getCid db = do
        conn <- connectSqlite3 db
        quer <- quickQuery' conn "SELECT cid FROM conj" []
        let res = concat . fromQuery $ quer
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
                                        "True" -> Tru hList
                                        "False" -> Fals hList
                                        "MaxT" -> MaxT
                                        _ -> MinT
                return $ smartCNF  $ map step rawLits
        disconnect conn
        return ret
-----------------
--end get conj
-----------------

-----------------
--process goal 
-----------------        
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
---------------
--end parsegoal
---------------

----------------
--add new clause
--to db
----------------

parseInpClause inp = map stepLit ls
        where 
            ls = join . map (splitOn ";") . join . map (splitOn ",") . splitOn "OR" $ inp                
            stepLit lit = [lstring,hedges,seed]
              where [lstring,truthstring] = case 
                                      (map (reverse . dropWhile (\x->x==' ') . 
                                      reverse . dropWhile (\x->x==' '))  $ splitOn "::" lit) of
                                            [lstring, truthstring] -> [lstring,truthstring]
                                            _ -> ["", "MaxT"]
                    hstring =  properTruthString truthstring                
                    hedges = concat . intersperse " " . init $ hstring
                    seed = last hstring                                                      
changeByCID :: String -> String -> IO ()
changeByCID id dbname = do
        conn <- connectSqlite3 dbname
        putStrLn "Enter new clause: "
        putStrLn "-- Please enter it in the format\n \
                \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."               
        line <- readline'
        let inp = parseInpClause line
        let sqlval = map (map toSql) inp 
        q <- forM sqlval (\x -> quickQuery' conn "SELECT lstring, hedges, truthval FROM literal \
                                \ WHERE lstring = ? AND hedges = ? AND truthval = ?" $ x)
        let tq = concat . map fromQuery $ q
        let the_rest' = map (map toSql) $ dropWhile (\x-> x `elem` tq) inp
        unless (null the_rest') $ do
          q <- forM the_rest' (\x -> run conn "INSERT INTO literal (lstring, hedges, truthval) \
                                \ VALUES (?, ?, ?)" x)                                   
          putStrLn "New literals inserted."
        unless (not $ null the_rest') $ putStrLn "No new literal inserted"
        
        lids1 <- forM sqlval (\x -> quickQuery' conn "SELECT lid FROM literal WHERE \
                                                \ lstring = ? AND hedges = ? AND truthval = ?"
                                               x)
        cid1 <- (\x-> quickQuery' conn "SELECT conjLits.cid FROM \ 
                        \ conjLits JOIN literal ON conjLits.lid = literal.lid \
                        \ WHERE literal.lid = ?" $ concat x) $ head lids1
        putStrLn $ show cid1
        lids2 <- forM cid1 (\x -> quickQuery' conn "SELECT conjLits.lid FROM \
                        \ conjLits WHERE conjLits.cid = ?" x)
        let lids1' = sort . map head . concat . map (map $ map fromSql) $ lids1 :: [Int]
            lids2' = sort . map head . concat . map (map $ map fromSql) $ lids2 :: [Int]   
        putStrLn $ show lids1' ++ show lids2'    
        if (lids1' /= lids2') then do
                q <- run conn "DELETE FROM conj WHERE cid = ?" [toSql id]
                q <- run conn "DELETE FROM conjLits WHERE cid = ?" [toSql id]                
                q <- run conn "INSERT INTO conj (cid, name) VALUES (?,?)" [toSql id, toSql id]

                lids <- forM sqlval (\x -> quickQuery' conn "SELECT lid FROM literal WHERE \
                                                        \ lstring = ? AND hedges = ? AND truthval = ?"
                                                       x)
                let lids' = concat lids
                q <- forM lids' (\x -> run conn "INSERT INTO conjLits (cid,lid) VALUES (?,?)"
                                          $ (toSql id):x)
                q <- quickQuery' conn "SELECT lstring, hedges, truthval \
                        \FROM literal JOIN conjLits ON conjLits.lid = literal.lid \
                        \WHERE conjLits.cid = ?" [toSql id]
                putStrLn $ show $ map (map toSql) q
                commit conn
            else putStrLn "Duplicated clause. No modification has been made."    

        disconnect conn
                    
deleteByCID :: String -> String -> IO ()
deleteByCID id dbname = do
        conn <- connectSqlite3 dbname
        q <- run conn "DELETE FROM conj WHERE cid = ?" [toSql id]
        putStrLn $ "Remove " ++ show q ++ " clauses"
        q <- run conn "DELETE FROM conjLits WHERE cid = ?" [toSql id]
        putStrLn $ "Remove " ++ show q ++ " conj-lits relations"
        commit conn
        disconnect conn 

addClause dbname = do
        putStrLn "Enter a clause: "
        putStrLn "-- Please enter it in the format\n \
                \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."               
        line <- readline'
        let inp = parseInpClause line
        --putStrLn $ show inp
        ----Insert new literals if there were any
        let sqlval = map (map toSql) inp 
        conn <- connectSqlite3 dbname
        q <- forM sqlval (\x -> quickQuery' conn "SELECT lstring, hedges, truthval FROM literal \
                                \ WHERE lstring = ? AND hedges = ? AND truthval = ?" $ x)
        let tq = concat . map fromQuery $ q
        --putStrLn $ show tq
        let the_rest' = map (map toSql) $ dropWhile (\x-> x `elem` tq) inp
        --let the_rest = dropWhile (\x-> x `elem` tq) inp
        --putStrLn $ show the_rest'
        unless (null the_rest') $ do
          q <- forM the_rest' (\x -> run conn "INSERT INTO literal (lstring, hedges, truthval) \
                                \ VALUES (?, ?, ?)" x)                                   
          putStrLn "New literals inserted."
        unless (not $ null the_rest') $ putStrLn "No new literal inserted"
        
        ----Check if there were any cnf exactly the same as the
        ----one being added
        lids1 <- forM sqlval (\x -> quickQuery' conn "SELECT lid FROM literal WHERE \
                                                \ lstring = ? AND hedges = ? AND truthval = ?"
                                               x)
        cid1 <- (\x-> quickQuery' conn "SELECT conjLits.cid FROM \ 
                        \ conjLits JOIN literal ON conjLits.lid = literal.lid \
                        \ WHERE literal.lid = ?" $ concat x) $ head lids1
        --putStrLn $ show cid1
        lids2 <- forM cid1 (\x -> quickQuery' conn "SELECT conjLits.lid FROM \
                        \ conjLits WHERE conjLits.cid = ?" x)
        let lids1' = sort . map head . concat . map (map $ map fromSql) $ lids1 :: [Int]
            lids2' = sort . map head . concat . map (map $ map fromSql) $ lids2 :: [Int]   
        --putStrLn $ show lids1' ++ show lids2'    
        if (lids1' /= lids2') then do
                q <- run conn "INSERT INTO conj (name) VALUES (?)" [toSql "Just inserted"]
                cid' <- quickQuery' conn "SELECT cid FROM conj WHERE name = ?" [toSql "Just inserted"]
                let [[cid]] = cid'
                q <- run conn "UPDATE conj SET name = ? WHERE cid = ?" [toSql cid, toSql cid]

                lids <- forM sqlval (\x -> quickQuery' conn "SELECT lid FROM literal WHERE \
                                                        \ lstring = ? AND hedges = ? AND truthval = ?"
                                                       x)
                putStrLn $ show lids
                putStrLn $ show cid
                let lids' = concat lids
                q <- forM lids' (\x -> run conn "INSERT INTO conjLits (cid,lid) VALUES (?,?)"
                                          $ cid :x)
                q <- quickQuery' conn "SELECT lstring, hedges, truthval \
                        \FROM literal JOIN conjLits ON conjLits.lid = literal.lid \
                        \WHERE conjLits.cid = ?" [cid]
                putStrLn $ show $ map (map toSql) q
                commit conn
            else putStrLn "No new clause added"    
        disconnect conn

-------------------------------------------------

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
                        knowledgebase <- getCNF dbname
                        putStrLn $ show knowledgebase
                        let res = map (\x-> prove knowledgebase x) input
                        putStrLn $ show res
                        return ()                                        
                "print" -> do
                        kb <- getCNF dbname
                        putStrLn $ show kb                               
                _ -> putStrLn "please enter something meaningful"
        
readline' = do maybesmt <- readline ">>= "        
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

