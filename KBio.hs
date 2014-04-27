module KBio 
where
import ProsLogic
import Ahedge
import AlphaResolution
import Util
-----old _test-------------------------------------------
--sid_to_hstring:: String->SqlValue->IO (Maybe [String])
sid_to_hstring dbname sid = do
        conn <- connectSqlite3 dbname 
        let query = "with recursive sids as (select sid,hid,tail from hstring where hstring.sid = ?\
                     \ UNION select hstring.sid,hstring.hid,hstring.tail from hstring, sids\
                     \  where hstring.sid IS NOT NULL AND hstring.sid = sids.tail)\
                     \ select hedge from sids JOIN hedges on sids.hid = hedges.hid;"
        q <- quickQuery' conn query [sid]
        disconnect conn
        return $ map fromSql $ concat q :: IO [String]

hStringtoHid:: String->[String]->IO (Maybe [SqlValue])
hStringtoHid dbname hstring = do
        conn <- connectSqlite3 dbname
        q <- forM (map toSql hstring) (\x->quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [x])
        disconnect conn
        case q of
           _ | [] `elem` q -> return Nothing      
             | otherwise ->  return $ Just (concat . concat $ q)

find_sid conn ( [], acc, (x:xs)) = do
        return ([], acc, (x:xs))
find_sid conn ( (x:xs), acc, processed) = do        
        sid <- quickQuery' conn "SELECT sid FROM hstring WHERE tail is ? AND hid = ?"
                [acc, x]
        if (null sid) then return ( (x:xs), acc, processed)        
                else find_sid conn ( xs, head . head $ sid, x:processed)

insert_if_not_exist_sid dbname hString = do
        let hstring = properTruthString hString
        hids <- (liftM $ fmap reverse) $ hStringtoHid dbname hstring                                
        case hids of
          Nothing -> return Nothing
          (Just x)
           -> do conn <- connectSqlite3 dbname 
                 (remained, acc, processed) <- find_sid conn (x, SqlNull, [])
                 disconnect conn
                 case (remained,acc,processed) of
                        ([], acc, processed) | processed == reverse x -> return $ Just acc
                                             | otherwise -> return Nothing
                        ((x:xs), acc, processed)
                          -> do q <- foldM step (Just acc) remained
                                case q of (Just SqlNull) -> return Nothing
                                          _       -> return q 
                                  where step sidd hid = 
                                          case sidd of
                                           (Just sid) ->
                                              do conn <- connectSqlite3 dbname
                                                 q <- run conn "INSERT INTO hstring(hid,tail) VALUES \
                                                        \ (?,?)" [hid,sid]
                                                 print q
                                                 commit conn
                                                 nextsidQ <- quickQuery' conn "SELECT sid FROM hstring \
                                                              \ WHERE tail is ? AND hid = ?" 
                                                              [sid,hid]
                                                 disconnect conn             
                                                 if (null nextsidQ) then 
                                                      return Nothing
                                                   else do let nextsid = head . head $ nextsidQ
                                                           return (Just nextsid)     
                                           Nothing -> return Nothing                        
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
                                        "Maxt" -> Maxt
                                        _ -> Mint
                return $ smartCNF  $ map step rawLits
        disconnect conn
        return ret
-----------------
--end get conj
-----------------

-----------------
--process goal 
-----------------        

parseGoal:: String -> Lit Hedge
parseGoal inp = case seed of
                        "True" -> Lit lstring (Fals hedges)
                        "False" -> Lit lstring (Tru hedges)
                        "Maxt" -> Lit lstring Mint
                        _ -> Lit lstring Maxt                
        where [lstring,truthstring] = case (map (reverse . dropWhile (\x->x==' ') . reverse . dropWhile (\x->x==' '))
                                          $ splitOn "::" inp) of
                                          [lstring, truthstring] -> [lstring,truthstring]
                                          _ -> ["", "Mint"]
              hstring = properTruthString truthstring                
              hedges = map read (init hstring) ::(Ha h) => [h]
              seed = last hstring                                                      
parseGoals :: String -> [Lit Hedge]              
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
                                            _ -> ["", "Mint"]
                    hstring =  properTruthString truthstring                
                    hedges = concat . intersperse " " . init $ hstring
                    seed = last hstring                                                      
changeByCID :: String -> String -> String -> IO ()
changeByCID id line{-newclause-} dbname = do
        conn <- connectSqlite3 dbname
        {-putStrLn "Enter new clause: "
        putStrLn "-- Please enter it in the format\n \
                \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."               
        line <- readline'-}
        let inp = parseInpClause line{-newclause-}
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

addClause dbname line = do
        {-putStrLn "Enter a clause: "
        putStrLn "-- Please enter it in the format\n \
            \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."
        line <- readline'-}
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



