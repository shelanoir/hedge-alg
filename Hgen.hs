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
readline' = do maybesmt <- readline ">>= "        
               case maybesmt of
                Nothing -> return ""
                Just x -> addHistory x >> return x   

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

hedgeDef :: [String] -> [String] -> [String] -> [[String]] -> [Dec]
hedgeDef cons pos neg rel = [DataD [] (mkName "Hedge") [] 
                                (map (\x -> NormalC (mkName x) []) cons) 
                                 [mkName "Eq",
                                  mkName "Show",
                                  mkName "Enum", 
                                  mkName "Read"],
                        InstanceD [] (AppT (ConT $ mkName "Ha") (ConT $ mkName "Hedge")) 
                         [ValD (VarP $ mkName "posLs") (NormalB (ListE posConE)) [],
                          ValD (VarP $ mkName "negLs") (NormalB (ListE negConE)) [],
                          ValD (VarP $ mkName "posRel") (NormalB (ListE prel)) []
                         ]]
        where posConE = map (ConE . mkName) pos
              negConE = map (ConE . mkName) neg
              prel     = map (TupE . (map $ ConE . mkName)) rel  
                                



mm::IO [Dec]
cli_gen dbname = do
                                {-putStr "Enter type: \n"
                                typ <- readline'
                                putStrLn "Enter data constructors"
                                str <- loop [] " "
                                let strr = words str
                                dataDef' typ strr-}
                                 
                                let cons = ["Possibly", "Very", "More", "Less"]
                                    posLs = ["Very", "More"]
                                    negLs = ["Less","Possibly"]
                                    posRel = [["Very","More"],
                                              ["Very","Less"],
                                              ["More","Very"],
                                              ["Very","Very"],
                                              ["More","More"]]                                   
                                return (hedgeDef cons posLs negLs posRel)
        where loop acc latest = if (latest=="") then return (acc)
                                  else do str2 <- readline'
                                          let acc2 = str2 ++ " " ++ acc
                                          loop acc2 str2

       
mm = do
        args <- getArgs
        let trueArgs = args
        --let [_, trueargs] = words $ args !! (length args - 2)
        --let trueArgs = read trueargs :: [String]
        --putStrLn "Arguments are: "
        --print trueArgs
        case trueArgs of
                ["--cli", dbname] -> do putStrLn "CLI"
                                        cli_gen dbname
                ("--gui":_)       -> do putStrLn "GUI"
                                        cli_gen " "
                _                 -> cli_gen " "       
q = runIO mm


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

