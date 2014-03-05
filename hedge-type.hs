----------------------------------------------------
----------------------------------------------------
-----------------------
--Hedge algebra class:
--defining lists of
--all hedges, pos hedge,
--neg hedges, relation
--b/w hedges (either <+>
--or <-> to each other)
--and a generic comparing
--function for Ord implementation
--of Ha types

class (Eq a, Ord a, Enum a, Show a) => Ha a where

      --positivity, negativity, order of Hedges 
        hedgeLs :: [a]
        hedgeLs = enumFrom (toEnum 0)

        posLs :: [a]
        posLs = [h | h <- hedgeLs, not $ negative h]

        negLs :: [a]
        negLs = [h | h <- hedgeLs, not $ positive h]

        positive :: (Ha a) => a -> Bool
        negative :: (Ha a) => a -> Bool

        positive x = elem x posLs        
        negative x = elem x negLs

     --relation between hedges
        allRel :: [(a,a)]
        allRel = [(x,y) | x <- hedgeLs, y <- hedgeLs, x /= y]

        posRel :: [(a,a)]
        posRel = [(x,y) | (x,y) <- allRel, not (x <-> y)]

        negRel :: [(a,a)]
        negRel = [(x,y) | (x,y) <- allRel, not (x <+> y)]

        (<+>) :: a -> a -> Bool
        a <+> b = elem (a,b) posRel

        (<->) :: a -> a -> Bool
        a <-> b = elem (a,b) negRel

      --generic comparing function 
        compare':: a -> a -> Ordering
        compare' h1 h2 
                | h1 == h2 = EQ
                | (positive h1) && (negative h2) = GT
                | (positive h1) && (positive h2) && (position posLs h1) < (position posLs h2) = GT
                | (negative h1) && (negative h2) && (position negLs h1) < (position negLs h2) = GT
                | otherwise = LT
                where position :: Eq a => [a] -> a -> Int
                      position xs x = head [i | (i,y) <- zip [1..] xs, x == y]

----------------------------------------------------
----------------------------------------------------




----------------------------------------------------
----------------------------------------------------
--A hedge algebra:
--implementations for ord
--and show went here
-- Hedge is Ord, Eq and Enum
-- Hedge is Enum in the sense
-- that one can list it from
-- Possibly to Less
data Hedge = Possibly | Very | More | Less deriving (Eq, Show, Enum)

instance (Ha Hedge) where
        posLs = [Very, More]
        posRel = [(Very,More), (Very,Less), (More,Very), (Very,Very), (More, More)]

--Eq Hedge = default

--Ord Hedge
instance Ord Hedge where
        compare = compare' 

--instace Show Hedge = default

----------------------------------------------------
----------------------------------------------------



----------------------------------------------------
----------------------------------------------------
--Truth value:
--Truth :: (Ha hedge) => hedge -> Truth hedge -> Truth hedge
--Truth hedge <= Eq, Ord, Show
data Truth hedge = Tru [hedge] | Fals [hedge] deriving (Eq)

-- Show Truth h        
instance (Ha h) => Show (Truth h) where
        show (Tru []) = "True"
        show (Fals []) = "False"
        show (Tru (x:xs)) = show x ++ " " ++ show (Tru xs)
        show (Fals (x:xs)) = show x ++ " " ++ show (Fals xs)

isHTrue (Tru h) = True
isHTrue (Fals h) = False
isHFalse = not . isHTrue

andH :: (Ha h) => Truth h -> Truth h -> Truth h
andH = min

orH :: (Ha h) => Truth h -> Truth h -> Truth h
orH = max

notH :: (Ha h) => Truth h -> Truth h
notH (Tru h) = Fals h
notH (Fals h) = Tru h

(><) :: (Ha h) => Truth h -> Truth h -> Bool
(Tru h) >< (Fals k) = True
(Fals h) >< (Tru k) = True
_ >< _ = False



-- Ord Truth h
-- Here comes the dragon...
instance Ha h => Ord (Truth h) where
       --basic cases: 
       --cover 0-0 case
       compare (Tru _) (Fals _) = GT
       compare (Fals _) (Tru _) = LT
       compare h1 h2 | h1 == h2 = EQ
        
       --exhaustive:
       ---- both True

       compare (Tru ls1) (Tru ls2) =
        let hs1 = reverse ls1
            hs2 = reverse ls2
            aux h1 h2 =
              case (h1,h2) of
              -- at least 1 - 0
                ((h:hs),[])
                         | positive h -> GT
                         | otherwise -> LT                
                ([],(h:ks))
                         | positive h -> LT
                         | otherwise -> GT
                         
              -- 1 - 1 ( at least 1 - at least 1 )                    
                ((h:hs),(k:ks))
                         | positive h, negative k -> GT
                         | negative h, positive k -> LT

                         | positive h, positive k, h > k -> GT
                         | positive h, positive k, h < k -> LT

                         | negative h, negative k, h > k -> LT
                         | negative h, negative k, h < k -> GT

              -- 1 - at least 2, guaranteed equal head           
                ((h1:h2:hs), [k])
                         | positive h1, h2 <+> h1 -> GT
                         | positive h1, h2 <-> h1 -> LT
                         | negative h1, h2 <+> h1 -> LT
                         | negative h1, h2 <-> h1 -> GT
                ([k], (h1:h2:hs))
                         | positive h1, h2 <+> h1 -> LT
                         | positive h1, h2 <-> h1 -> GT
                         | negative h1, h2 <+> h1 -> GT
                         | negative h1, h2 <-> h1 -> LT

              -- at least 2 - at least 2, guaranteed equal head
                ((h1:h2:hs),(k1:k2:ks))
                         |h1 == k1, h2 == k2 -> compare (h2:hs) (k2:ks)

                         |h1 == k1, positive h1, h2 <+> h1, k2 <+> k1, h2 > k2 -> GT
                         |h1 == k1, positive h1, h2 <+> h1, k2 <+> k1, h2 < k2 -> LT
                         |h1 == k1, positive h1, h2 <-> h1, k2 <-> k1, h2 > k2 -> LT
                         |h1 == k1, positive h1, h2 <-> h1, k2 <-> k1, h2 < k2 -> GT
                         |h1 == k1, positive h1, h2 <+> h1, k2 <-> k1 -> GT
                         |h1 == k1, positive h1, h2 <-> h1, k2 <+> k1 -> LT

                         |h1 == k1, negative h1, h2 <+> h1, k2 <+> k1, h2 > k2 -> LT
                         |h1 == k1, negative h1, h2 <+> h1, k2 <+> k1, h2 < k2 -> GT
                         |h1 == k1, negative h1, h2 <-> h1, k2 <-> k1, h2 > k2 -> GT
                         |h1 == k1, negative h1, h2 <-> h1, k2 <-> k1, h2 < k2 -> LT
                         |h1 == k1, negative h1, h2 <+> h1, k2 <-> k1 -> LT
                         |h1 == k1, negative h1, h2 <-> h1, k2 <+> k1 -> GT
        in aux hs1 hs2

       ---- both false:
       compare (Fals ls1) (Fals ls2) = 
                case (compare (Tru ls1) (Tru ls2)) of
                        GT -> LT
                        LT -> GT
                        EQ -> EQ

-- test cases
truth1 = Fals [Very,Very,Very,More]
truth2 = Fals [More,Possibly]
res1 = compare truth1 truth2        
res2 = compare truth2 truth1
----------------------------------------------------
----------------------------------------------------
                
----------------------------------------------------
----------------------------------------------------
-- Literal and CNF clause:
-- Literal = atomic sentence with degree of truth
-- CNF clause of literals = list of literals

---Literal:
data Lit = Lit String (Truth Hedge) 
instance Show Lit where
        show (Lit x truth) = "<" ++ x ++ ": " ++ show truth ++ ">"

truthLit (Lit string truth) = truth 
stringLit (Lit string truth) = string

data CNF a = CNF [a]
instance Show a => Show (CNF a) where
        show (CNF [x]) = show x ++ "\n"
        show (CNF (x:xs)) = show x ++  " OR " ++ (show (CNF xs)) 

lsCNF (CNF a) = a
        

