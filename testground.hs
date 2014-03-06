import ProsLogic
import Ahedge
import AlphaResolution

truth1 = Fals [Very,Very,Very,More]
truth2 = Fals [More,Possibly]
res1 = compare truth1 truth2        
res2 = compare truth2 truth1
----------------------------------------------------
----------------------------------------------------
                

---------testing data-----------
lit1 = Lit "a" (Fals [More])
lit2 = Lit "b" (Fals [])
lit3 = Lit "c" (Tru [Very, More])
lit4 = Lit "b" (Tru [Less])
lit5 = Lit "c" (Tru [Possibly])
lit6 = Lit "a" (Tru [Possibly])
lit7 = Lit "b" (Tru [Very])
lit8 = Lit "c" (Fals [Very])

cnf1 = smartCNF [lit1, lit2, lit3]
cnf2 = smartCNF [lit4, lit5]
cnf3 = smartCNF [lit6]
cnf4 = smartCNF [lit7]
cnf5 = smartCNF [lit8]

kb = zip (map lsCNF [cnf1, cnf2, cnf3, cnf4, cnf5]) (repeat MaxT)
--------------------------------

----------------------------------------
----------------------------------------

