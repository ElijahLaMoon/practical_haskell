{-# LANGUAGE LambdaCase #-}

module SimpleFunctions where

import Data.Char ( toUpper )
import Data.List (find)
import Data.Bifunctor (first, second)

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
(+++) [] ys = ys
(+++) (x : xs) ys = x : (xs +++ ys)

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x : xs) = reverse2 xs ++ [x]

maxmin :: Ord a => [a] -> (a, a)
maxmin [x] = (x, x)
maxmin (x : xs) = ( if x > xs_max then x else xs_max
                  , if x < xs_min then x else xs_min
                  ) where (xs_max, xs_min) = maxmin xs

-- data Client = GovOrg     String
--             | Company    String Integer Person String
--             | Individual Person
--             deriving Show

-- data Person = Person String String Gender
--             deriving Show

-- data Gender = Male | Female | Unknown
--             deriving Show

sumTuple3 :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
sumTuple3 (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

-- countGenders :: [Client] -> (Int, Int, Int)
-- countGenders [] = (0,0,0)
-- countGenders (x@(Individual (Person _ _ gender)) : xs) =
--               case gender of
--                 Male    -> (1,0,0) `sumTuple3` countGenders xs
--                 Female  -> (0,1,0) `sumTuple3` countGenders xs
--                 Unknown -> (0,0,1) `sumTuple3` countGenders xs
-- countGenders (_ : xs) = (0,0,0) `sumTuple3` countGenders xs

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x : _) = x

myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (_ : xs) = xs

fibonacci :: Integer -> Maybe Integer
fibonacci 0         = Just 0
fibonacci 1         = Just 1
fibonacci n
  | n < 0     = Nothing
  | otherwise = let Just f1 = fibonacci (n - 1)
                    Just f2 = fibonacci (n - 2)
                in Just (f1 + f2)

binom :: (Eq a, Num a) => a -> a -> a
binom _ 0          = 1
binom x y | x == y = 1
binom n k          = binom (n - 1) (k - 1) + binom (n - 1) k

ackermann :: Integral a => a -> a -> Maybe a
ackermann m n
  | m == 0          = Just $ n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | m > 0 && n > 0  = ackermann (m - 1) n'
  | otherwise       = Nothing
    where Just n' = ackermann m (n - 1)

myUnzip :: [(a, a)] -> ([a], [a])
myUnzip []       = ([], [])
myUnzip (x : xs) = (fst x : fsts, snd x : snds)
  where fsts = fst $ myUnzip xs
        snds = snd $ myUnzip xs

data Client = GovOrg      { clientId    :: Integer
                          , clientName  :: String }
            | Company     { clientId    :: Integer
                          , clientName  :: String
                          , person      :: Person
                          , duty        :: String }
            | Individual  { clientId    :: Integer
                          , person      :: Person }
            deriving Show

data Person = Person { firstName :: String
                     , lastName  :: String
                     } deriving Show

nameInCapitals :: Person -> Person
nameInCapitals p@Person { firstName = initial : rest } =
  let newName = toUpper initial : rest
  in p { firstName = newName }
nameInCapitals p@Person { firstName = "" } = p

swapTriple :: (x, y, z) -> (y, z, x)
swapTriple (x,y,z) = (y,z,x)

duplicate :: a -> (a, a)
duplicate a = (a, a)

nothing :: a -> Maybe a
nothing _ = Nothing

index :: Integral b => [a] -> [(b, a)]
index [] = []
index [x] = [(0, x)]
index (x : xs) = let indexed@((n, _) : _) = index xs
                 in (n + 1, x) : indexed

maybeA :: [a] -> Char
maybeA [] = 'a'

myFiler :: (a -> Bool) -> [a] -> [a]
myFiler _ [] = []
myFiler p (x : xs) = if p x
                     then x : myFiler p xs
                     else myFiler p xs

filterOnes :: (Eq a, Num a) => [a] -> [a]
filterOnes = filter ( == 1)

filterANumber :: (Eq a, Num a) => a -> [a] -> [a]
filterANumber n = filter ( == n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter $ not . p

filterGovOrg :: [Client] -> [Client]
filterGovOrg = filter isGovOrg
  where isGovOrg (GovOrg _ _) = True
        isGovOrg _          = False

filterGovOrg1 :: [Client] -> [Client]
filterGovOrg1 = filter (\case (GovOrg _ _) -> True
                              _          -> False)

product' :: Integral a => [a] -> a
product' = foldl (*) 1

minimumClient :: [Client] -> Maybe Client
minimumClient [] = Nothing
minimumClient cls = Just . fst . foldl1 minimumClient' $ map calculateNameLength cls
  where
    calculateNameLength c@(Individual _ p) = (c, length $ firstName p ++ " " ++ lastName p)
    calculateNameLength c                 = (c, length $ clientName c)

    minimumClient' x@(_, i1) y@(_, i2) = if min i1 i2 == i1 then x else y

all' :: [Bool] -> Bool
all' [] = True
all' bs = foldl1 (&&) bs

minimumBy :: Integral a => (a -> a) -> [a] -> Maybe a
minimumBy f (x : y : xs) = if f x < f y
                           then minimumBy f (x : xs)
                           else minimumBy f (y : xs)
minimumBy _ [x] = Just x
minimumBy _ _ = Nothing

bothFilter :: (a -> Bool) -> [a] -> ([a], [a])
bothFilter _ [] = ([], [])
bothFilter p (x : xs) = if p x
                        then first (x : ) (bothFilter p xs)
                        else second (x : ) (bothFilter p xs)

elem' :: Eq a => a -> [a] -> Bool
elem' x = auxF . find (== x)
  where auxF (Just _) = True
        auxF Nothing  = False

compareClient :: Client -> Client -> Ordering
compareClient Individual { person = p1 } Individual {person = p2 }
                              = compare (firstName p1) (firstName p2)
compareClient Individual {} _ = GT
compareClient _ Individual {} = LT
compareClient c1 c2           = compare (clientName c1) (clientName c2)

listOfClients = [ Individual 2 (Person "H. G." "Wells")
                , GovOrg 3 "NTTF" -- National Time Travel Foundation
                , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
                , Individual 5 (Person "Doctor" "")
                , Individual 6 (Person "Sarah" "Jane")
                ]

{-
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x ys = let headPrefix = takeWhile (/= x) ys
                   rest       = drop 1 $ dropWhile (/= x) ys
               in headPrefix ++ rest
-}

delete' :: Eq a => a -> [a] -> [a]
delete' x = aux x []
  where
    aux e pref [] = pref
    aux e pref (r : rs) = if e == r
                          then pref ++ rs
                          else aux e (pref ++ [r]) rs

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f seed = auxUnfoldr $ f seed
  where auxUnfoldr Nothing       = []
        auxUnfoldr (Just (a, b)) = a : unfoldr' f b
