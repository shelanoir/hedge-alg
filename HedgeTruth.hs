module HedgeTruth (
        Truth(..),
        module HedgeClass,
        module Data.List,
        isHTrue,
        isHFalse,
        andH,
        orH,
        notH,
        (><)
)
where
import HedgeClass
import Data.List
----------------------------------------------------
----------------------------------------------------
--Truth value:
--Truth :: (Ha hedge) => hedge -> Truth hedge -> Truth hedge
--Truth hedge <= Eq, Ord, Show, bounded by MaxT and MinT
data Truth hedge = Tru [hedge] | Fals [hedge] | Maxt | Mint deriving (Eq, Read)
-- Show Truth h        
instance (Show h) => Show (Truth h) where        
        show (Tru []) = "True"
        show (Fals []) = "False"
        show (Tru (x:xs)) = show x ++ " " ++ show (Tru xs)
        show (Fals (x:xs)) = show x ++ " " ++ show (Fals xs)
        show Maxt = "Maxt"
        show Mint = "Mint"

isHTrue (Tru h) = True
isHTrue Maxt = True
isHTrue Mint = False
isHTrue (Fals h) = False
isHFalse = not . isHTrue

andH :: (Ha h) => Truth h -> Truth h -> Truth h
andH = min

orH :: (Ha h) => Truth h -> Truth h -> Truth h
orH = max

notH :: (Ha h) => Truth h -> Truth h
notH (Tru h) = Fals h
notH Maxt = Mint
notH Mint = Maxt
notH (Fals h) = Tru h

(><) :: (Ha h) => Truth h -> Truth h -> Bool
(Tru h) >< (Fals k) = True
(Fals h) >< (Tru k) = True
Maxt >< Mint = True
Mint >< Maxt = True
Maxt >< (Fals k) = True
(Fals k) >< Maxt = True
Mint >< (Tru k) = True
(Tru k) >< Mint = True
_ >< _ = False


-- Ord Truth h
-- Here comes the dragon...
instance Ha h => Ord (Truth h) where
       --basic cases: 
       compare Maxt t | t /= Maxt = GT
       compare Mint t | t /= Mint = LT
       compare t Mint | t /= Mint = GT
       compare t Maxt | t /= Maxt = LT
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

