module Lecture3 where

import qualified Data.List as L
import Debug.Trace

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr op init [] = init
myfoldr op init (x:xs) = x `op` (myfoldr op init xs)

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl op init [] = init
myfoldl op init (x:xs) = myfoldl op (op init x) xs

myfoldl' :: (b -> a -> b) -> b -> [a] -> b
myfoldl' op init [] = init
myfoldl' op init (x:xs) = let init' = (op init x) 
                          in seq init' (myfoldl' op init' xs)

{- 
1 * (2 * (3 * 4))
((1 * 2) * 3) * 4)

(((1 + 2) + 3) + 4)

myfoldl (+) (3) [3..100000]
myfoldl (+) (6) [3..100000]
.
.
(((1 + 2) + 3) + 4)
(3 + 3) + 4

-}

cylinderArea' :: Float -> Float -> Float
cylinderArea' r h
    | trace ("calling cylinder with r" ++ show r) False = undefined
cylinderArea' r h = sideArea + 2 * topArea
    where
        sideArea = 2 * pi * r * h
        topArea = pi * r * r

type IncreasingList = [Int]

type IntFunction = Int -> Int

merge :: IncreasingList -> IncreasingList -> IncreasingList
merge = undefined

type Trace a = Int -> a

exampleTrace :: Trace Int
exampleTrace = id

-- indices : 1 2 3 4 5 ...
-- values : 1 2 3 

newtype Feet = Feet Double

sixFeet :: Feet
sixFeet = Feet 6.0

doubleFeet :: Feet -> Feet
doubleFeet (Feet x) = Feet (x *2)

data Ellipse = Ellipse { major :: Float, minor :: Float }
    deriving Eq

-- ad-hoc polymorphic
instance (Show Ellipse) where
    show (Ellipse major minor)
        | major == minor = "We have a circle"
        | otherwise = "Just an ellipse"

mkEllipse :: Float -> Float -> Ellipse
mkEllipse major minor
    | major < minor = error "major < minor"
mkEllipse major minor = Ellipse major minor

ellipseArea :: Ellipse -> Float
ellipseArea e = pi * (major e) * (minor e)

data Day = Saturday | Sunday | Monday | Wednesday
    deriving Show

data Shape = MyRectangle Float Float | MySquare Float
    deriving Show

area :: Shape -> Float
area (MyRectangle a b) = a * b
area (MySquare a) = a * a

data List a = Nil | Cons a (List a)
    deriving Show

instance Eq a => Eq (List a) where
    Nil == Nil = True
    Cons x l1 == Cons y l2 = x == y && l1 == l2
    _ == _ = False

data NonEmpty a = Singleton a | More a (NonEmpty a)
data Stream a = Stream a (Stream a)
    deriving Show

ones :: Stream Int
ones = Stream 1 ones

--Int
-- List Int
-- 7 :: Int :: *
-- Int -> Float

mylookup :: Int -> [(Int, b)] -> b
mylookup i tab = snd . head $ filter (\(key, val) -> key == i) tab

-- parametrically polymorphic
safeLookup :: Eq a => a -> [(a, b)] -> Maybe b
safeLookup _ [] = Nothing
safeLookup i ((k, v) : xs)
    | i == k = Just v
    | otherwise = lookup i xs

--takeMaybe :: Maybe a -> Int
takeMaybe _ = 7

nothingInt :: Maybe Int
nothingInt = Nothing

nothingBool :: Maybe Bool
nothingBool = Nothing

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = lesser ++ [x] ++ greater
    where
        lesser = quicksort [y | y <- xs , y <= x]
        greater = quicksort [y | y <- xs , y > x]


scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct l r = go 0 l r
    where
        go accum [] _ = accum
        go accum _ [] = accum
        go accum (x:xs) (y:ys) = go (accum + x * y) xs ys 