import ProsLogic
import Ahedge
import AlphaResolution

----------------------------------------------------
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

cnf1 = smartCNF [lit1, lit2, lit3]
cnf2 = smartCNF [lit4, lit5]
cnf3 = smartCNF [lit6]
cnf4 = smartCNF [lit7]
cnf5 = smartCNF [lit8] -- negated goal

--------------------------------
kb = [cnf1, cnf2, cnf3, cnf4]
goal = cnf5
initialClauses = kbUnionGoal kb goal
----------------------------------------
----------------------------------------
{-truth1 = Fals [Very,Very,Very,More]
truth2 = Fals [More,Possibly]
res1 = compare truth1 truth2        
res2 = compare truth2 truth1-}

