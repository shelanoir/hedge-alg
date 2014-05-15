module DBio.Hio(
  hManager,
  hMMenu,
  printPosH,
  printNegH,
{-  removeH,
  removePosH,
  removeNegH,
  addNegH,
  addPosH,      
  changePosOrd,
  changeNegOrd,
  addPosRel, 
  addNegRel, 
  removePosRel,
  removeNegRel,
  renameHedge,-}
  ) where
import SelfRestart
import Util.UtilB
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.Char
import SelfRestart(selfRestart,exitImmediately,ExitCode(..))
import System.Environment(getProgName)
-------------------------------------
--TODO: add a hedge to posl, to negl
--TODO: remove ...
--TODO: change precedence of posl/negl
--TODO: remove a relationship constraint
--TODO: add a rel constraint
printlH :: IConnection conn => String -> conn -> IO ()
printlH tabname conn = do
        qQ <- quickQuery' conn ("SELECT "++tabname++".hid,hedge,pred FROM hedges, "++tabname++" WHERE "++tabname++".hid = hedges.hid ORDER BY pred") []
        let q = ($qQ) $ map $ map (fromSql::SqlValue->String)
        putStrLn "[ HID | Hedge | Precedence ]"
        print q

printPosH :: IConnection conn => conn -> IO ()
printPosH conn = printlH "posl" conn       
printNegH :: IConnection conn => conn -> IO ()
printNegH conn = printlH "negl" conn       

removeH :: String -> String -> IO ()
removeH dbname hedge' = do
        let hedge = properFormat hedge'
        conn <- connectSqlite3 dbname
        q <- run conn "DELETE FROM posl WHERE posl.hid =\
                \ (SELECT hid FROM hedges WHERE hedges.hedge = ?) " [toSql hedge]
        printlH "posl" conn
        q <- run conn "DELETE FROM negl WHERE negl.hid =\
                \ (SELECT hid FROM hedges WHERE hedges.hedge = ?) " [toSql hedge]
        printlH "negl" conn
        q <- run conn "DELETE FROM posrel WHERE posrel.hid1 =\
                \ (SELECT HID FROM hedges WHERE hedges.hedge = ?)" [toSql hedge]
        q <- run conn "DELETE FROM posrel WHERE posrel.hid2 =\
                \ (SELECT HID FROM hedges WHERE hedges.hedge = ?)" [toSql hedge]
        q <- run conn "DELETE FROM negrel WHERE negrel.hid1 =\
                \ (SELECT HID FROM hedges WHERE hedges.hedge = ?)" [toSql hedge]
        q <- run conn "DELETE FROM negrel WHERE negrel.hid2 =\
                \ (SELECT HID FROM hedges WHERE hedges.hedge = ?)" [toSql hedge]
        q <- run conn "DELETE FROM hedges WHERE hedges.hedge = ?" [toSql hedge]
        commit conn
        disconnect conn

removelH:: String->String->String->IO ()
removelH tabname dbname hedge' = do
        let hedge = properFormat hedge'
        conn <- connectSqlite3 dbname
        q <- run conn ("DELETE FROM " ++tabname++" WHERE "++tabname++".hid =\
                \ (SELECT hid FROM hedges WHERE hedges.hedge = ?) ") [toSql hedge]
        print q        
        printlH tabname conn
        q <- run conn "DELETE FROM posrel WHERE posrel.hid1 =\
                \ (SELECT HID FROM hedges WHERE hedges.hedge = ?)" [toSql hedge]
        q <- run conn "DELETE FROM posrel WHERE posrel.hid2 =\
                \ (SELECT HID FROM hedges WHERE hedges.hedge = ?)" [toSql hedge]
        q <- run conn "DELETE FROM negrel WHERE negrel.hid1 =\
                \ (SELECT HID FROM hedges WHERE hedges.hedge = ?)" [toSql hedge]
        q <- run conn "DELETE FROM negrel WHERE negrel.hid2 =\
                \ (SELECT HID FROM hedges WHERE hedges.hedge = ?)" [toSql hedge]
        commit conn
        disconnect conn

removePosH:: String->String->IO ()
removePosH = removelH "posl"
removeNegH:: String->String->IO ()
removeNegH = removelH "negl"

addlH::String->String->String->String->String->IO ()
addlH table dbname hedge' pred yN= do
        conn <- connectSqlite3 dbname
        --get posl and negl lists
        qR <- quickQuery' conn "SELECT hedge FROM hedges, posl WHERE posl.hid = hedges.hid" []
        let posL = map ((fromSql::SqlValue->String) . head) qR
        print posL
        qR <- quickQuery' conn "SELECT hedge FROM hedges, negl WHERE negl.hid = hedges.hid" []
        let negL = map ((fromSql::SqlValue->String) . head) qR
        print negL

        let hed = properFormat hedge' 

        let thisL = case table of
                    "posl" ->  posL                                     
                    "negl" ->  negL                             
                    _      ->  posL          

        let thatL = case table of
                    "posl" ->  negL                                     
                    "negl" ->  posL                             
                    _      ->  negL          

        if (hed `elem` thisL)
          then do putStrLn "Already in this list"
                  disconnect conn
          else if (hed `elem` thatL)
            then do let cmd = map toUpper yN
                    putStrLn $ "Warning: already in "++table++ " list, decision is: "++yN
                    case cmd of
                      "Y" -> do disconnect conn
                                case table of
                                  "posl" -> removeNegH dbname hed
                                  "negl" -> removePosH dbname hed
                                  _      -> removeNegH dbname hed 
                                conn <- connectSqlite3 dbname
                                q <- run conn ("INSERT INTO "++table++"(hid,pred)\ 
                                               \ SELECT hid, ? FROM hedges WHERE hedge = ?") [toSql pred, toSql hed]
                                printPosH conn                  
                                print q
                                commit conn
                                disconnect conn
--                                selfRestart         
                      _   -> do putStrLn "Nothing done"
                                disconnect conn     
            else do q <- quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [toSql hed]
                    when (null q) $ 
                      do putStrLn $ hed ++ " is not already in the database"
                         putStrLn $ "...Adding " ++ hed     
                         q <- run conn "INSERT INTO hedges(hedge) VALUES (?)" [toSql hed]                         
                         print q
                    {-putStrLn "What would this hedge's precedence value be?"
                    pred <- readline'-}
                    q <- run conn ("INSERT INTO "++table++"(hid,pred)\ 
                                   \ SELECT hid, ? FROM hedges WHERE hedge = ?") [toSql pred, toSql hed]
                    printPosH conn                  
                    print q
                    commit conn
                    disconnect conn    
  --                  selfRestart                    

addNegH:: String->String->String->String->IO ()
addNegH = addlH "negl"
addPosH:: String->String->String->String->IO ()
addPosH = addlH "posl"                   


changeOrd :: String->String->String->String->IO ()
changeOrd table hedge' newpred dbname = do
        conn <- connectSqlite3 dbname
        let hedge = properFormat hedge'        
        printlH table conn
        q <- quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [toSql hedge]
        let hid = case q of
                    [] -> SqlNull
                    _  ->  head . head $ q-- hedge's hid
        q <- run conn ("DELETE FROM " ++ table ++ " WHERE hid = ?") [hid];                
        putStrLn $ "rows removed: " ++ show q
        q <- run conn ("INSERT INTO " ++ table ++ "(hid,pred) VALUES (?,?)") [hid, toSql newpred]
        putStrLn $ "rows inserted: " ++ show q
        printlH table conn
        commit conn
        disconnect conn
    --    selfRestart

changePosOrd :: String->String->String->IO ()
changePosOrd = changeOrd "posl"
changeNegOrd :: String->String->String->IO ()
changeNegOrd = changeOrd "negl"

renameHedge :: String -> String -> String -> IO ()
renameHedge dbname hedge' new' = do
        conn <- connectSqlite3 dbname
        let hedge = properFormat hedge'
            new = properFormat new'    
        q <- run conn "UPDATE hedges SET hedge = ? WHERE hedge = ?" [toSql new, toSql hedge]
        print q
        commit conn
        disconnect conn

addlRel :: String -> String -> String -> String -> IO ()
addlRel table dbname hedge1' hedge2' = do
        conn <- connectSqlite3 dbname
        let hedge1 = properFormat hedge1'
            hedge2 = properFormat hedge2'
        putStrLn $ hedge1 ++" "++ hedge2    
        putStrLn ""            
        let thatT = case table of
                      "posrel" -> "negrel"
                      _        -> "posrel"
            thisT = table                 
        q <- quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [toSql hedge1]
        let hid1 = case q of
                    [] -> SqlNull
                    _  ->  head . head $ q-- hedge's hid
        print q
        putStrLn ""            
        q <- quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [toSql hedge2]
        let hid2 = case q of
                    [] -> SqlNull
                    _  ->  head . head $ q-- hedge's hid
        print q
        putStrLn ""            
        qQ <- run conn ("DELETE FROM "++thatT) []
        qQ <- run conn ("INSERT OR IGNORE INTO "++thisT++"(hid1,hid2) VALUES(?, ?)") [hid1,hid2]
        commit conn
        qQ <- quickQuery' conn "SELECT hid FROM posl UNION SELECT hid FROM negl" []
        let q = concat qQ
        print q
        putStrLn ""
        qQ <- quickQuery' conn ("SELECT * FROM "++thisT) []  
        print qQ
        putStrLn ""
        let negRel = [[h1,h2]| h1 <- q, h2 <- q, [h1,h2] `notElem` qQ]
        q <- forM negRel $ run conn ("INSERT OR IGNORE INTO "++thatT++"(hid1,hid2) VALUES(?, ?)")
        --print $ foldr1 (+) q
        q <- quickQuery' conn ("SELECT * FROM "++ thatT) []
        putStrLn ""
        print q
        commit conn
        disconnect conn

addPosRel :: String -> String -> String -> IO ()
addPosRel = addlRel "posrel"        
addNegRel :: String -> String -> String -> IO ()
addNegRel = addlRel "negrel"        



removelRel :: String -> String -> String -> String -> IO ()
removelRel table dbname hedge1' hedge2' = do
        conn <- connectSqlite3 dbname
        let hedge1 = properFormat hedge1'
            hedge2 = properFormat hedge2'
        let thatT = case table of
                      "posrel" -> "negrel"
                      _        -> "posrel"
            thisT = table                 
        q <- quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [toSql hedge1]        
        let hid1 = case q of
                    [] -> SqlNull
                    _  ->  head . head $ q-- hedge's hid
        q <- quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [toSql hedge2]
        let hid2 = case q of
                    [] -> SqlNull
                    _  ->  head . head $ q-- hedge's hid
        
        qQ <- run conn ("DELETE FROM "++thatT) []
        qQ <- run conn ("DELETE FROM "++thisT++" WHERE hid1=? AND hid2=?") [hid1,hid2]
        
        qQ <- quickQuery' conn "SELECT hid FROM posl UNION SELECT hid FROM negl" []
        let q = concat qQ
        print q
        putStrLn ""
        qQ <- quickQuery' conn ("SELECT * FROM "++thisT) []  
        print qQ
        putStrLn ""
        let negRel = [[h1,h2]| h1 <- q, h2 <- q, [h1,h2] `notElem` qQ]
        q <- forM negRel $ run conn ("INSERT OR IGNORE INTO "++thatT++"(hid1,hid2) VALUES(?, ?)")
        --print $ foldr1 (+) q
        q <- quickQuery' conn ("SELECT * FROM "++ thatT) []
        putStrLn ""
        print q
        commit conn
        disconnect conn

removePosRel :: String -> String -> String -> IO ()
removePosRel = removelRel "posrel"        
removeNegRel :: String -> String -> String -> IO ()
removeNegRel = removelRel "negrel"        

hMMenu :: [String]
hMMenu = ["=============================================================",
          ">>= print  - print the hedge structure",    
          ">>= add positive - add a positive hedge",
          ">>= add negative - add a negative hedge",
          "-------------------------------------------------",      
          ">>= rm positive - remove a positive hedge",
          ">>= rm negative - remove a negative hedge",  
          ">>= rm hedge - completely remove a hedge",  
          "-------------------------------------------------",      
          ">>= mv positive - move a positive hedge to another precedence",
          ">>= mv negative - move a negative hedge to another precedence",
          ">>= rename hedge - rename a hedge",
          "-------------------------------------------------",      
          ">>= add positive relation - add a new positive relation",
          ">>= add negative relation - add a new negative relation",
          ">>= rm positive relation - add a new positive relation",
          ">>= rm negative relation - add a new negative relation",
          "-------------------------------------------------",      
          ">>= hmenu - print this menu",
          ">>= main - reload hedge algebra and go to main menu",                    
          ">>= quit - exit without reload HA",                    
          "============================================================="
          ]
hManager :: String -> IO ()
hManager dbname = do
          progname <- getProgName
          let menu = hMMenu
--          mapM_ putStrLn menu 
          putStrLn "\n\nPlease choose : \n"
          command <- readline'
          case command of
                  "hmenu" -> do 
                              mapM_ putStrLn menu 
                              hManager dbname
                  "main"  -> do selfRestart
                                return ()                                
                  "quit"  -> case progname of
                                "ghc" -> putStrLn "HA not loaded. Exit program..." 
                                          >> exitImmediately ExitSuccess
                                _     -> return ()              
                  "add positive" -> do putStrLn "Please enter the name of the hedge:"
                                       hedge' <- readline'
                                       putStrLn "What would this hedge's precedence value be?"
                                       pred <- readline'
                                       putStrLn "Remove from negative list if the hedge were already there? [y/n]"
                                       yN <- readline'
                                       addPosH dbname hedge' pred yN        
                                       --selfRestart
                                       hManager dbname
                  "rm positive" -> do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      removePosH dbname hedge'         
                                      --selfRestart
                                      hManager dbname
                  "add negative" -> do putStrLn "Please enter the name of the hedge:"
                                       hedge' <- readline'
                                       putStrLn "What would this hedge's precedence value be?"
                                       pred <- readline'
                                       putStrLn "Remove from positive list if the hedge were already there? [y/n]"
                                       yN <- readline'
                                       addNegH dbname hedge' pred yN        
                                       --selfRestart
                                       hManager dbname
                  "rm negative" -> do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      removeNegH dbname hedge'         
                                      --selfRestart
                                      hManager dbname
                  "rm hedge" ->    do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      removeH dbname hedge'         
                                      --selfRestart
                                      hManager dbname
                  "mv positive" -> do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      putStrLn "What would this hedge's precedence value be?"
                                      pred <- readline'
                                      changePosOrd hedge' pred dbname
                                      hManager dbname
                                      --selfRestart
                  "mv negative" -> do putStrLn "Please enter the name of the hedge:"
                                      hedge' <- readline'
                                      putStrLn "What would this hedge's precedence value be?"
                                      pred <- readline'
                                      changeNegOrd hedge' pred dbname
                                      --selfRestart
                                      hManager dbname

                  "rename hedge" -> do putStrLn "Please enter the name of the hedge:"          
                                       hedge' <- readline'
                                       putStrLn "What would this hedge's new name be?"
                                       name' <- readline'
                                       renameHedge dbname hedge' name'
                                       hManager dbname
                                       --selfRestart
                  "add positive relation" -> do putStrLn "Please enter the name of the affecting hedge:"          
                                                hedge1' <- readline'
                                                putStrLn "Please enter the name of the affected hedge" 
                                                hedge2' <- readline'
                                                addPosRel dbname hedge1' hedge2'
                                                hManager dbname
                                                --selfRestart
                  "add negative relation" -> do putStrLn "Please enter the name of the affecting hedge:"          
                                                hedge1' <- readline'
                                                putStrLn "Please enter the name of the affected hedge" 
                                                hedge2' <- readline'
                                                addNegRel dbname hedge1' hedge2'
                                                hManager dbname
                                                --selfRestart
                  "rm positive relation" -> do  putStrLn "Please enter the name of the affecting hedge:"          
                                                hedge1' <- readline'
                                                putStrLn "Please enter the name of the affected hedge" 
                                                hedge2' <- readline'
                                                removePosRel dbname hedge1' hedge2'
                                                hManager dbname
                                                --selfRestart
                  "rm negative relation" -> do  putStrLn "Please enter the name of the affecting hedge:"          
                                                hedge1' <- readline'
                                                putStrLn "Please enter the name of the affected hedge" 
                                                hedge2' <- readline'
                                                removeNegRel dbname hedge1' hedge2'
                                                hManager dbname
                                                --selfRestart
                  "print"           -> do printHedges dbname                     
                                          q <- getLine
                                          hManager dbname 
                  _ -> do putStrLn "please enter something meaningful"
                          q <- getLine
                          hManager dbname


printHedges :: String -> IO ()                          
printHedges dbname = do
        putStrLn ""
        putStrLn "Every hedges in the database:"
        conn <- connectSqlite3 dbname
        qQ <- quickQuery' conn "SELECT hid,hedge FROM hedges" []
        let q = map (map fromSql) qQ :: [[String]]
        print q
        putStrLn ""
        putStrLn ""
        putStrLn "Positive hedges:"
--        print (posLs::[Hedge])
        putStrLn ""
        printPosH conn        
        putStrLn ""
        putStrLn ""
        putStrLn "Negative hedges:"
--        print (negLs::[Hedge])
        putStrLn ""
        printNegH conn        
        putStrLn ""
        putStrLn ""
        putStrLn "Hedge actually in used:"
--        print (hedgeLs::[Hedge])
        qQ <- quickQuery' conn "SELECT hedge FROM (\
         \SELECT hid,hedge FROM posl NATURAL JOIN hedges\
         \ UNION SELECT hid,hedge FROM negl NATURAL JOIN hedges ORDER BY hid)" []
        let q = map fromSql $ concat qQ :: [String]
        print q
        putStrLn ""
        putStrLn ""
        putStrLn ""
        putStrLn "Positive relations:"                   
        qQ <- quickQuery' conn "SELECT h1.hedge, h2.hedge\
         \ FROM (SELECT hid1,hid2 FROM posrel) h12, hedges h1, hedges h2\
         \ WHERE h1.hid = h12.hid1 AND h2.hid = h12.hid2" []  
        let q = map (map fromSql) $ qQ :: [[String]]
        print q
--        print (posRel::[(Hedge,Hedge)])
        putStrLn ""
        putStrLn ""
        putStrLn "Negative relations:"                   
        qQ <- quickQuery' conn "SELECT h1.hedge, h2.hedge\
         \ FROM (SELECT hid1,hid2 FROM negrel) h12, hedges h1, hedges h2\
         \ WHERE h1.hid = h12.hid1 AND h2.hid = h12.hid2" []  
        let q = map (map fromSql) $ qQ :: [[String]]
        print q
--        print (negRel::[(Hedge,Hedge)])
        putStrLn ""
        disconnect conn
        

