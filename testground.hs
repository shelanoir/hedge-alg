import ProsLogic
import Ahedge
import AlphaResolution
import Data.IORef
import Control.Monad
import Data.List.Split

----------------------------------------------------
--- TODO:
--- tracing/explaining feature
--- user interface
----------------------------------------------------

kbUnionGoal k g = zipWith smartClause (k ++ [g]) (repeat MaxT)
printClauses = map (\(a,b) -> CNF a)

---------testing data-----------
litA1 = Lit "America is behind the Ukraine's crisis" (Fals [More])
litB2 = Lit "Russia's intervention does not violate the International Law" (Fals [])
litC3 = Lit "Russia's intervention is justified" (Tru [Very, More])
litB4 = Lit "Russia's intervention does not violate the International Law" (Tru [Less])
litC5 = Lit "Russia's intervention is justified" (Tru [Possibly])
litA6 = Lit "America is behind the Ukraine's crisis" (Tru [Possibly])
litB7 = Lit "Russia's intervention does not violate the International Law" (Tru [Very])

litC8 = Lit "Russia's intervention is justified" (Fals [Very]) --negated goal

cnf1 = smartCNF [litA1, litB2, litC3]
cnf2 = smartCNF [litB4, litC5]
cnf3 = smartCNF [litA6]
cnf4 = smartCNF [litB7]
cnf5 = smartCNF [litC8] -- negated goal

--------------------------------
kb = [cnf1, cnf2, cnf3, cnf4]
negatedGoal = cnf5
initialClauses = kbUnionGoal kb negatedGoal
----------------------------------------
----------------------------------------
truth1 = Fals [Very,Very,Very,More]
truth2 = Fals [Very,More,Possibly]
res1 = compare truth1 truth2        
res2 = compare truth2 truth1

destructive :: IORef Int -> IO ()
destructive io = modifyIORef io (+1)

main = do
        io <- newIORef 3
        destructive io
        a <- readIORef io
        putStr $ show a ++ "\n"
