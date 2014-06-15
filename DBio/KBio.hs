module DBio.KBio (
        cmenu,
        kbManager,
        parseGoals,
        getCNF
        )
where
import ProsLogic
import Hedgen.Ahedge
import AlphaResolution
import Util.UtilA
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
      if (hstring/=[]) then do
        conn <- connectSqlite3 dbname
        q <- forM (map toSql hstring) (\x->quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [x])
        disconnect conn
--        print q
        case q of
           _ | [] `elem` q -> return Nothing      
             | otherwise ->  return $ Just (concat . concat $ q)
      else return (Just [SqlNull])
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
  --               print $ (remained, acc, processed)
                 disconnect conn
                 case (remained,acc,processed) of
                        ([], acc, processed) | processed == reverse x -> return $ Just acc
                                             | otherwise -> return Nothing
                        ([SqlNull], acc, processed) | processed == reverse x -> return $ Just acc
                                             | otherwise -> return $ Just SqlNull
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
    --                                             print q
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
        ret<-forM cids $ \x -> do 
                conn <- connectSqlite3 db
                rawQ <- quickQuery' 
                        conn 
                        "SELECT lstring, truthval, sid\
                        \ FROM literal JOIN conjLits ON conjLits.lid = literal.lid\
                        \ WHERE conjLits.cid = ?" 
                        [toSql x]
                disconnect conn                        
                rawLits <- forM rawQ (\[string,seed,sid] -> do 
                                         qQ <- sid_to_hstring db sid
                                         let q = concat $ intersperse " " qQ
                                             lstring = (fromSql string) :: String
                                             lseed = (fromSql seed) :: String
                                         --putStrLn $ "hstring: " ++ (show sid)   
                                         return [lstring,lseed,q])
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
magicNil = "ioejhfiSUH>DN)#(*&YWHDSKJNCS(@#*YSAKJCN(*@Y#EASKJCNNC<WAO*EY@(*HOIASLKJ(*WASKDJH"
----------------
--add new clause
--to db
----------------

parseInpClause inp = map stepLit ls
        where 
            ls = join . map (splitOn ";") . join . map (splitOn ",") . splitOn "OR" $ inp                
            stepLit lit = case ok_hedges of
                                True -> [lstring,hedges,seed]
                                False -> [magicNil,"","Mint"]
              where [lstring,truthstring] = case 
                                      (map (reverse . dropWhile (\x->x==' ') . 
                                      reverse . dropWhile (\x->x==' '))  $ splitOn "::" lit) of
                                            [_,""] -> [magicNil,"Mint"]
                                            [lstring, truthstring] -> [lstring,truthstring]
                                            _ -> [magicNil, "Mint"]
                    hstring =  properTruthString truthstring                
                    hedges = concat . intersperse " " . init $ hstring
                    seed = last hstring                                                     
                    ok_hedges = seed `elem` ["True","False","Maxt","Mint"] && (and $ map (\x -> x `elem` hlist) 
                                $ initHstring)
                    initHstring = init hstring
                    hlist = map show (hedgeLs::[Hedge])
changeByCID :: String -> String -> String -> IO ()
changeByCID id line{-newclause-} dbname = do
        --conn <- connectSqlite3 dbname
        {-putStrLn "Enter new clause: "
        putStrLn "-- Please enter it in the format\n \
                \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."               
        line <- readline'-}
       let inp = parseInpClause line{-newclause-}        
       when ([magicNil,"","Mint"] `elem` inp) $ putStrLn "Please enter a valid clause!" >> return ()
       unless ([magicNil,"","Mint"] `elem` inp) $ do
        sqlval <- forM inp
                       (\[lstring, hedges, lseed]
                          -> do (Just sid) <- insert_if_not_exist_sid dbname hedges                            
                                let string = toSql lstring
                                    seed = toSql lseed
                                return [string,sid,seed])                              
        conn <- connectSqlite3 dbname 
        q <- forM sqlval (\x -> quickQuery' conn "SELECT lstring, sid, truthval FROM literal \
                                 \ WHERE lstring = ? AND sid IS ? AND truthval = ?" $ x)
        let tq = concat q
        let the_rest' = dropWhile (\x-> x `elem` tq) sqlval
        unless (null the_rest') $ do
          q <- forM the_rest' (\x -> run conn "INSERT INTO literal (lstring, sid, truthval) \
                                \ VALUES (?, ?, ?)" x)                                   
          putStrLn "New literals inserted."
        unless (not $ null the_rest') $ putStrLn "No new literal inserted"
        
        lids1 <- forM sqlval (\x -> quickQuery' conn "SELECT lid FROM literal WHERE \
                                                \ lstring = ? AND sid IS ? AND truthval = ?"
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
                q <- run conn "DELETE FROM conj WHERE cid = ?" [toSql id]
                q <- run conn "DELETE FROM conjLits WHERE cid = ?" [toSql id]                
                q <- run conn "INSERT INTO conj (cid, name) VALUES (?,?)" [toSql id, toSql id]

                lids <- forM sqlval (\x -> quickQuery' conn "SELECT lid FROM literal WHERE \
                                                        \ lstring = ? AND sid IS ? AND truthval = ?"
                                                       x)
                let lids' = concat lids
                q <- forM lids' (\x -> run conn "INSERT INTO conjLits (cid,lid) VALUES (?,?)"
                                          $ (toSql id):x)
                q <- quickQuery' conn "SELECT lstring, sid, truthval \
                        \FROM literal JOIN conjLits ON conjLits.lid = literal.lid \
                        \WHERE conjLits.cid = ?" [toSql id]
                --putStrLn $ show $ map (map toSql) q
                putStrLn "Clause changed"
                commit conn
            else putStrLn "Duplicated clause. No modification has been made."    

        disconnect conn
                    

changeClause dbname line newclause = do
       let inp = parseInpClause line
       when ([magicNil,"","Mint"] `elem` inp) $ putStrLn "Please enter a valid clause!" >> return ()
       unless ([magicNil,"","Mint"] `elem` inp) $ do
                          sqlval <- forM inp
                                     (\[lstring, hedges, lseed]                                          
                                          -> do (Just hids) <- hStringtoHid dbname (properTruthString hedges)
                                                conn <- connectSqlite3 dbname
                                                let string = toSql lstring
                                                    seed = toSql lseed
                                                    rhids = reverse hids
                                                (_,sid,_) <- find_sid conn (rhids, SqlNull, [])
                                                disconnect conn
                                                return [string,sid,seed])
                          --print sqlval
                          conn <- connectSqlite3 dbname
                          q <- forM sqlval (\x -> quickQuery' conn "SELECT cid FROM conjLits where conjLits.lid in \
                                                  \ (SELECT lid FROM literal \
                                                   \ WHERE lstring = ? AND sid IS ? AND truthval = ?)" $ x)
                          --print q
                          let tq = map (map (fromSql . head)) q :: [[String]]
                          --print tq
                          let cid = foldl1 intersect tq
                          --print cid
                          disconnect conn
                          unless (null tq || (length tq /= length inp)) $ do
                            if (cid /= [] && length cid == 1) then do
                              conn <- connectSqlite3 dbname      
                              lids <- quickQuery' conn "SELECT lid FROM conjLits where conjLits.cid = ?" $ map toSql cid    
                              disconnect conn
                              if (length lids == length inp) then
                                       do 
                                          changeByCID (head . head $ tq :: String) newclause dbname
                                   else putStrLn "Nothing has been done: no exact clause matched"            
                              else if (cid == []) then putStrLn "Nothing has been done: no clause matched"
                                      else if (length cid /= 1) then do
                                               conn <- connectSqlite3 dbname
                                               lids <- mapM (\x -> quickQuery' 
                                                        conn 
                                                        "SELECT lid FROM conjLits where conjLits.cid = ?" 
                                                         $ map toSql x) cid    
                                               q <- forM sqlval 
                                                         (\x -> 
                                                          quickQuery' conn 
                                                          "SELECT lid FROM literal \
                                                           \ WHERE lstring = ? AND sid IS ? AND truthval = ?" $ x)
                                               disconnect conn
                                               let tq = map (map head) q          
                                               --putStrLn $ "330: " ++ show lids ++ "   " ++ show tq        
                                               let ind = findIndex (\x -> x == tq) lids
                                               --print ind
                                               case ind of
                                                Nothing ->  putStrLn "Nothing has been done: multiple clause matched:" 
                                                Just x -> do
                                                 let theCid =  cid !! x
                                                 --print theCid
                                                 changeByCID theCid newclause dbname 
                                                 return ()
           --                                    print cid
                                               --putStrLn "Please try again with the Change by id option"    
                                              else putStrLn "Nothing has been done"
                          disconnect conn                            
                          when (null tq || (length tq /= length inp)) $ putStrLn "Nothing has been done"            


deleteByCID :: String -> String -> IO ()
deleteByCID id dbname = do
        conn <- connectSqlite3 dbname
        q <- run conn "DELETE FROM conj WHERE cid = ?" [toSql id]
        putStrLn $ "Remove " ++ show q ++ " clauses"
        q <- run conn "DELETE FROM conjLits WHERE cid = ?" [toSql id]
        putStrLn $ "Remove " ++ show q ++ " conj-lits relations"
        putStrLn "Clause deleted"
        commit conn
        disconnect conn 

deleteClause dbname line = do                          
       let inp = parseInpClause line
       when ([magicNil,"","Mint"] `elem` inp) $ putStrLn "Please enter a valid clause!" >> return ()
       unless ([magicNil,"","Mint"] `elem` inp) $ do
                          sqlval <- forM inp
                                     (\[lstring, hedges, lseed]                                          
                                          -> do (Just hids) <- hStringtoHid dbname (properTruthString hedges)
                                                conn <- connectSqlite3 dbname
                                                let string = toSql lstring
                                                    seed = toSql lseed
                                                    rhids = reverse hids
                                                --putStrLn $ "hids: " ++ (show hids) ++ "\n"    
                                                (_,sid,_) <- find_sid conn (rhids, SqlNull, [])
                                                disconnect conn
                                                return [string,sid,seed])                             
                          --print sqlval
                          conn <- connectSqlite3 dbname
                          q <- forM sqlval (\x -> quickQuery' conn "SELECT cid FROM conjLits where conjLits.lid in \
                                                  \ (SELECT lid FROM literal \
                                                   \ WHERE lstring = ? AND sid IS ? AND truthval = ?)" $ x)
                          let tq = map (map (fromSql . head)) q :: [[String]]
                          --putStrLn "312: "
                          --print tq
                          let cid = foldl1 intersect tq
                          --putStrLn "314: "
                          --print cid
                          --print inp
                          disconnect conn
                          unless (null tq || (length tq /= length inp)) $ do
                            if (cid /= [] && length cid == 1) then do
                              conn <- connectSqlite3 dbname      
                              lids <- quickQuery' conn "SELECT lid FROM conjLits where conjLits.cid = ?" $ map toSql cid    
                              disconnect conn
                              if (length lids == length inp) then
                                       do deleteByCID (head $ cid :: String) dbname
                                   else putStrLn "Nothing has been done: no exact clause matched"            
                              else if (cid == []) then putStrLn "Nothing has been done: no clause matched"
                                      else if (length cid /= 1) then do
                                               conn <- connectSqlite3 dbname
                                               lids <- mapM (\x -> quickQuery' 
                                                        conn 
                                                        "SELECT lid FROM conjLits where conjLits.cid = ?" 
                                                         $ map toSql x) cid    
                                               q <- forM sqlval 
                                                         (\x -> 
                                                          quickQuery' conn 
                                                          "SELECT lid FROM literal \
                                                           \ WHERE lstring = ? AND sid IS ? AND truthval = ?" $ x)
                                               disconnect conn
                                               let tq = map (map head) q          
                                               --putStrLn $ "330: " ++ show lids ++ "   " ++ show tq        
                                               let ind = findIndex (\x -> x == tq) lids
                                               --print ind
                                               case ind of
                                                Nothing ->  putStrLn "Nothing has been done: multiple clause matched:" 
                                                Just x -> do
                                                 let theCid =  cid !! x
                                                 --print theCid
                                                 deleteByCID theCid dbname 
                                                 return ()
                                               --print cid
                                               --putStrLn "Please try again with the Delete by id option"    
                                              else putStrLn "Nothing has been done"
                          --disconnect conn                            
                          when (null tq || (length tq /= length inp)) $ putStrLn "Nothing has been done"            


addClause dbname line = do
        {-putStrLn "Enter a clause: "
        putStrLn "-- Please enter it in the format\n \
            \  <statement> :: <truth-value> [OR|,|;] <statement> :: <truth-value> [OR|,|;]..."
        line <- readline'-}
       let inp = parseInpClause line{-newclause-}        
       when ([magicNil,"","Mint"] `elem` inp) $ putStrLn "Please enter a valid clause!" >> return ()
       unless ([magicNil,"","Mint"] `elem` inp) $ do
        sqlval <- forM inp
                       (\[lstring, hedges, lseed]
                          -> do (Just sid) <- insert_if_not_exist_sid dbname hedges                            
                                let string = toSql lstring
                                    seed = toSql lseed
                                return [string,sid,seed])                              
       -- print sqlval               
--        putStrLn ""
        conn <- connectSqlite3 dbname
        q <- forM sqlval (\x -> quickQuery' conn "SELECT lstring, sid , truthval FROM literal \
                                \ WHERE lstring = ? AND sid IS ? AND truthval = ?" $ x)
        let tq = concat q
        let the_rest' = dropWhile (\x-> x `elem` tq) sqlval
        --let the_rest = dropWhile (\x-> x `elem` tq) inp
        --putStrLn $ show the_rest'
        unless (null the_rest') $ do
          --print the_rest'
          q <- forM the_rest' (\x -> run conn "INSERT INTO literal (lstring, sid, truthval) \
                                \ VALUES (?, ?, ?)" x)                                   
          putStrLn "New literals inserted."
        unless (not $ null the_rest') $ putStrLn "No new literal inserted"
        
        ----Check if there were any cnf exactly the same as the
        ----one being added
        print sqlval
        lids1 <- forM sqlval (\x -> quickQuery' conn "SELECT lid FROM literal WHERE \
                                                \ lstring = ? AND sid IS ? AND truthval = ?"
                                               x)
        print lids1
{-        let lids1 = case lidss1 of
                        [[]] -> [[[SqlNull]]]
                        _ -> lidss1-}
        print $ concat $ map (map head) lids1                
        cid1 <- (\x-> quickQuery' conn "SELECT conjLits.cid FROM \ 
                        \ conjLits JOIN literal ON conjLits.lid = literal.lid \
                        \ WHERE literal.lid IS ?" $ head x) $ map (map head) lids1
  --      putStrLn $ "cids "++ show cid1
        lids2 <- forM cid1 (\x -> quickQuery' conn "SELECT conjLits.lid FROM \
                        \ conjLits WHERE conjLits.cid = ?" x)
    --    putStrLn $ show lids1 ++ show lids2    
        let lids1'' = map (head . head) lids1
            lids2'' = map (map $ head) lids2
            lids1' = map fromSql lids1'' :: [Int]
            lids2' = map (map fromSql) lids2'' :: [[Int]]
        {-let lids1' = sort . map head . concat . map (map $ map fromSql) $ lids1 :: [Int]
            lids2' = sort . map head .  map (map $ map fromSql) $ lids2 :: [[Int]]   -}
        putStrLn $ show lids1' ++ show lids2'    
        if (not (lids1' `elem` lids2')) then do
                q <- run conn "INSERT INTO conj (name) VALUES (?)" [toSql "Just inserted"]
                cid' <- quickQuery' conn "SELECT cid FROM conj WHERE name = ?" [toSql "Just inserted"]
                let [[cid]] = cid'
                q <- run conn "UPDATE conj SET name = ? WHERE cid = ?" [toSql cid, toSql cid]

                lids <- forM sqlval (\x -> quickQuery' conn "SELECT lid FROM literal WHERE \
                                                        \ lstring = ? AND sid IS ? AND truthval = ?"
                                                       x)
      --          print lidss
    {-            let lids = case lidss of
                        [[]] -> [[[SqlNull]]]
                        _ -> lidss-}
        --        putStrLn $ show lids
          --      putStrLn $ show cid
                let lids' = concat lids
                q <- forM lids' (\x -> run conn "INSERT INTO conjLits (cid,lid) VALUES (?,?)"
                                          $ cid :x)
                q <- quickQuery' conn "SELECT lstring, sid, truthval \
                        \FROM literal JOIN conjLits ON conjLits.lid = literal.lid \
                        \WHERE conjLits.cid = ?" [cid]
                --print ( map (map fromSql) q :: [[String]])
            --    print q
                putStrLn "New clause added"    
                commit conn
            else putStrLn "No new clause added"    
        disconnect conn



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
