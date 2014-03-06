import ProsLogic
import Ahedge
import AlphaResolution

----------------------------------------------------
----------------------------------------------------
                

---------testing data-----------
lit1 = Lit "America is behind the Ukraine's crisis" (Fals [More])
lit2 = Lit "Russia's intervention does not violate the International Law" (Fals [])
lit3 = Lit "Russia's intervention is justified" (Tru [Very, More])
lit4 = Lit "Russia's intervention does not violate the International Law" (Tru [Less])
lit5 = Lit "Russia's intervention is justified" (Tru [Possibly])
lit6 = Lit "America is behind the Ukraine's crisis" (Tru [Possibly])
lit7 = Lit "Russia's intervention does not violate the International Law" (Tru [Very])

lit8 = Lit "Russia's intervention is justified" (Fals [Very]) --goal

cnf1 = smartCNF [lit1, lit2, lit3]
cnf2 = smartCNF [lit4, lit5]
cnf3 = smartCNF [lit6]
cnf4 = smartCNF [lit7]
cnf5 = smartCNF [lit8]

kb = zip (map lsCNF [cnf1, cnf2, cnf3, cnf4, cnf5]) (repeat MaxT)
--------------------------------

----------------------------------------
----------------------------------------
{-truth1 = Fals [Very,Very,Very,More]
truth2 = Fals [More,Possibly]
res1 = compare truth1 truth2        
res2 = compare truth2 truth1-}

