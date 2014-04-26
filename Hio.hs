module Hio(
  printPosH,
  printNegH,
  removeH,
  removePosH,
  removeNegH,
  addNegH,
  addPosH,      
  changePosOrd,
  changeNegOrd,
) where
import SelfRestart
import Triv
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.Char

-------------------------------------
--TODO: add a hedge to posl, to negl
--TODO: remove ...
--TODO: change precedence of posl/negl
--TODO: remove a relationship constraint
--TODO: add a rel constraint

printlH tabname conn = do
        qQ <- quickQuery' conn ("SELECT "++tabname++".hid,hedge,pred FROM hedges, "++tabname++" WHERE "++tabname++".hid = hedges.hid ORDER BY pred") []
        let q = ($qQ) $ map $ map (fromSql::SqlValue->String)
        putStrLn "[ HID | Hedge | Precedence ]"
        print q

printPosH conn = printlH "posl" conn       
printNegH conn = printlH "negl" conn       

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
removePosH = removelH "posl"
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

addNegH = addlH "negl"
addPosH = addlH "posl"                   


changeOrd :: String->String->String->String->IO ()
changeOrd table hedge' newpred dbname = do
        conn <- connectSqlite3 dbname
        let hedge = properFormat hedge'        
        printlH table conn
        q <- quickQuery' conn "SELECT hid FROM hedges WHERE hedge = ?" [toSql hedge]
        let hid = head . head $ q-- hedge's hid
        q <- run conn ("DELETE FROM " ++ table ++ " WHERE hid = ?") [hid];                
        putStrLn $ "rows removed: " ++ show q
        q <- run conn ("INSERT INTO " ++ table ++ "(hid,pred) VALUES (?,?)") [hid, toSql newpred]
        putStrLn $ "rows inserted: " ++ show q
        printlH table conn
        commit conn
        disconnect conn
    --    selfRestart

changePosOrd = changeOrd "posl"
changeNegOrd = changeOrd "negl"

