module Hgen  (
        q,
)
where
{-#LANGUAGE TemplateHaskell#-}
--{-#LANGUAGE XBangPatterns#-}
--import System.IO.Unsafe
import Language.Haskell.TH
import System.Console.Readline
import System.Environment
import Database.HDBC
import Database.HDBC.Sqlite3
import UtilB
import Hio
import System.Directory(doesFileExist)
import Control.Monad
import Control.Exception
import SelfRestart(ExitCode(..),selfRestart,exitImmediately)
import Data.Char(toLower)
-- Ctx: ClassP Name [Type]
-- Type: ConT Name(for type constructor)
-- <Ha Hedge> = AppT (ConT $ mkName "HA") (ConT $ mkName "Hedge")
-- InstanceD: Cxt Type [Dec]
-- e.g.: InstanceD [] (AppT (ConT $ mkName "HA") (ConT $ mkName "Hedge")) [Dec...]
-- [Dec...]: [ ValD (VarP $mkName posLs) (NormalB (ListE [ConE $ mkName...])) [],
--        ValD (VarP $mkName negLs) (NormalB (ListE [ConE $ mkName...])) [] ,    
--        ValD (VarP $mkName posRel) (NormalB (ListE 
--          [TupE [ConE $mkName h1, ConE $mkName h2], 
--           TupE [ConE $mkName k1, ConE $mkName k2], ..])) []]
-- to make pos/negLs:  (1) read the name from posls table,
--                      convert to a [String], export via
--                      unsafePerformIO
--                     (2) map (ConE . mkName) to it, and plug to
--                     the ListE expr
--to make posRel:      (1) read the pairs from posrel table,
--                      convert to a [[String,String]], export
--                      via unsafePerformIO
--                     (2) map (TupE . (map $ ConE . mkName)) to it,
--                     and plug to the ListE expr                     
--          
--
--
--

hedgeDef :: [String] -> [String] -> [String] -> [[String]] -> [[String]] -> [Dec]
hedgeDef cons pos neg rel negrel = [DataD [] (mkName "Hedge") [] 
                                (map (\x -> NormalC (mkName x) []) cons) 
                                 [mkName "Eq",
                                  mkName "Show",
                                  mkName "Enum", 
                                  mkName "Read"],
                        InstanceD [] (AppT (ConT $ mkName "Ha") (ConT $ mkName "Hedge")) 
                         [ValD (VarP $ mkName "posLs") (NormalB (ListE posConE)) [],
                          ValD (VarP $ mkName "negLs") (NormalB (ListE negConE)) [],
                          ValD (VarP $ mkName "posRel") (NormalB (ListE prel)) []
                          {-,ValD (VarP $ mkName "negRel") (NormalB (ListE nrel)) []-}
                         ]]
        where posConE = map (ConE . mkName) pos
              negConE = map (ConE . mkName) neg
              prel     = map (TupE . (map $ ConE . mkName)) rel  
              nrel     = map (TupE . (map $ ConE . mkName)) negrel  



mm::IO [Dec]
cli_gen dbname = do
                                let querycons = "select hedge \
                                        \from hedges join posl on hedges.hid = posl.hid \
                                        \union select hedge \
                                        \from hedges join negl on hedges.hid = negl.hid"
                                exist <- doesFileExist dbname
                                unless exist $ do 
                                  putStrLn "Initializing new knowledgebase..."
                                  bulkQuery dbname initSchema
                                  putStrLn "Hedge algebra definition is a mandatory\
                                   \ requirement for knowledge base. Define new HA? [y/n]"
                                  ans <- (liftM $map toLower) readline'
                                  case ans of
                                        "y" -> do mapM_ putStrLn hMMenu
                                                  hManager dbname              
                                                  putStrLn "Done initialization"
                                        _ -> do putStrLn "HA is not yet defined"
                                                exitImmediately ExitSuccess          
                                conn <- connectSqlite3 dbname
                                consQ <- quickQuery' conn querycons []
                                disconnect conn
                                when (null consQ) $ do
                                  putStrLn "No hedge algebra defined. Define new HA? [y/n]" 
                                  ans <- (liftM $map toLower) readline'
                                  case ans of
                                        "y" -> do mapM_ putStrLn hMMenu
                                                  hManager dbname              
                                                  putStrLn "Done initialization"
                                        _ -> do putStrLn "HA is not yet defined"
                                                exitImmediately ExitSuccess          
                                conn <- connectSqlite3 dbname
                                let cons = map head $ fromQuery consQ
                                print cons           
                                 
                                let queryposLs = "select hedge \
                                        \from hedges join posl on hedges.hid = posl.hid order by pred"
                                posQ <- quickQuery' conn queryposLs []
                                let posLs = map head $ fromQuery posQ 
                                let querynegLs = "select hedge \
                                        \from hedges join negl on hedges.hid = negl.hid order by pred"
                                negQ <- quickQuery' conn querynegLs []
                                let negLs = map head $ fromQuery negQ

                                let idsposRel = "select hid1, hid2 from posrel"
                                idQ <- quickQuery' conn idsposRel []
                                posrelQ <- forM idQ (quickQuery' conn 
                                         "select h1.hedge,h2.hedge from hedges h1, hedges h2 \
                                          \where h1.hid = ? AND h2.hid = ?")
--                                print posrelQ
                                let posRel = map head $ map fromQuery posrelQ
                                print posRel

                                let idsnegRel = "select hid1, hid2 from negrel"
                                idQ <- quickQuery' conn idsnegRel []
                                negrelQ <- forM idQ (quickQuery' conn 
                                         "select h1.hedge,h2.hedge from hedges h1, hedges h2 \
                                          \where h1.hid = ? AND h2.hid = ?")
--                                print negrelQ
                                let negRel = map head $ map fromQuery negrelQ
                                print negRel
                                {-let cons = ["Possibly", "Very", "More", "Less"]
                                    posLs = ["Very", "More"]
                                    negLs = ["Less","Possibly"]
                                    posRel = [["Very","More"],
                                              ["Very","Less"],
                                              ["More","Very"],
                                              ["Very","Very"],
                                              ["More","More"]]-}                                   
                                return (hedgeDef cons posLs negLs posRel negRel)
        where loop acc latest = if (latest=="") then return (acc)
                                  else do str2 <- readline'
                                          let acc2 = str2 ++ " " ++ acc
                                          loop acc2 str2

       
mm = do
        args <- getArgs
        --let trueArgs = args
        let dummyDec = [DataD [] (mkName "") [] [] []]
        let x = words $ args !! (length args - 2)
        let trueArgs = case x of
                        [_, trueargs] -> read trueargs :: [String]
                        _             -> ["--cli", "../test.db"]  
        putStrLn "Arguments are: "
        print trueArgs        
        case trueArgs of
                ("--cli":dbname:_) -> do putStrLn "CLI"
                                         cli_gen dbname
                ("--gui":_)       -> do putStrLn "GUI"
                                        cli_gen "../test.db"
                (dbname:_)        -> cli_gen dbname                           
                _                 -> cli_gen "../test.db"       
         `catch` ((\e ->do print e
                           putStrLn "Exception raised"
                           putStrLn "back to main menu or quit? [m/q]"
                           ansM <- readline'
                           let ans = map toLower ansM
                           case ans of
                             "m" -> do selfRestart
                                       return dummyDec    
                             "q" -> do exitImmediately $ ExitFailure (-1)
                                       return dummyDec    
                             _   -> do putStrLn "default: exit program"
                                       exitImmediately $ ExitFailure (-1)
                                       return dummyDec
                     ):: SomeException->IO [Dec])
q = runIO mm


bulkQuery dbname ls = do conn <- connectSqlite3 dbname
                         q <- mapM (\x-> run conn x []) ls
                         commit conn
                         disconnect conn
                         return q
initSchema =
        ["PRAGMA recursive_triggers=0",
         "CREATE TABLE hedges (hid Integer primary key NOT NULL, hedge Varchar(30) unique)",
         "CREATE TABLE posl(hid Integer references hedges(hid) NOT NULL, pred Integer, primary key (hid))",
         "CREATE TABLE negl(hid Integer references hedges(hid) NOT NULL, pred Integer, primary key (hid))",
         "CREATE TABLE posrel (hid1 Integer references hedges(hid) NOT NULL, hid2 Integer references hedges(hid) NOT NULL, primary key (hid1,hid2))",
         "CREATE TABLE negrel (hid1 Integer references hedges(hid) NOT NULL, hid2 Integer references hedges(hid) NOT NULL, primary key (hid1,hid2))",

         "CREATE TABLE hstring (sid Integer primary key, hid integer references hedges(hid) NOT NULL, tail Integer references hstring(sid))", 
         "CREATE TRIGGER posl_delete \
         \ AFTER DELETE ON posl \
         \ begin \
         \         update posl set pred = pred - 1 \
         \         where posl.pred >= OLD.pred and posl.hid != OLD.hid; \
         \ end;",
         "CREATE TRIGGER posl_insert  \
         \ AFTER INSERT ON posl  \
         \ BEGIN  \
         \   UPDATE posl set pred = pred + 1  \
         \      WHERE posl.pred >= NEW.pred and posl.hid != NEW.hid; \
         \ END;",
         "CREATE TRIGGER negl_delete \
         \ AFTER DELETE ON negl \
         \ begin \
         \         update negl set pred = pred - 1 \
         \         where negl.pred >= OLD.pred and negl.hid != OLD.hid; \
         \ end;",
         "CREATE TRIGGER negl_insert  \
         \ AFTER INSERT ON negl  \
         \ BEGIN  \
         \   UPDATE negl set pred = pred + 1  \
         \      WHERE negl.pred >= NEW.pred and negl.hid != NEW.hid; \
         \ END;",
         "CREATE TABLE literal (lid Integer, lstring Varchar(256), \
         \ sid Integer REFERENCES hstring(sid), truthval Varchar(20), PRIMARY KEY (lid))",
         "CREATE TABLE conjLits (cid Integer REFERENCES conj(cid), \
         \ lid Integer REFERENCES literal(lid), primary key (cid, lid))",
         "CREATE TABLE conj (cid Integer, name Varchar(256) DEFAULT\
         \ \"a conjunctive normal form clause\", PRIMARY KEY (cid))"]
--mmm :: () -> Q [Dec]
--mmm () = unsafePerformIO mm                 
--q = unsafePerformIO mm
dataDef :: String -> [String] -> {-Q-} [Dec]
dataDef name cons = {-return-} [DataD [] (mkName name) [] 
                                (map (\x -> NormalC (mkName x) []) cons) 
                                 [mkName "Eq",
                                  mkName "Show",
                                  mkName "Enum", 
                                  mkName "Read"] ]

dataDef' :: String -> [String] -> IO {-Q-} [Dec]
dataDef' a b = return (dataDef a b)                

                                {-putStr "Enter type: \n"
                                typ <- readline'
                                putStrLn "Enter data constructors"
                                str <- loop [] " "
                                let strr = words str
                                dataDef' typ strr-}
