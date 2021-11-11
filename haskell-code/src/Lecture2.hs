{-# OPTIONS_GHC -Wall #-}
module Lecture2 where

test :: p -> Bool
test _ = True

listExample :: [Int]
listExample = [1,2,3]

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

-- map, filter
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : (myMap f xs)

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter p (x:xs)
    | p x = x : myfilter p xs
    | otherwise = myfilter p xs

triangles :: [(Int, Int, Int)]
triangles = [(a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10]]

rightTriangles :: [(Int, Int, Int)]
rightTriangles = [(a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10], a*a + b *b == c *c]

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l r = go 0 l r
    where
        go accum [] _ = accum
        go accum _ [] = accum
        go accum (x:xs) (y:ys) = go (accum + x * y) xs ys 
        
scalarProduct' :: [Float] -> [Float] -> Float
scalarProduct' l r = sum $ zipWith (*) l r

{-
[1, 2][3, 4]
[(1, 3), (2, 4)]
[3, 8]
11
-}

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = lesser ++ [x] ++ greater
    where
        lesser = quicksort [y | y <- xs , y <= x]
        greater = quicksort [y | y <- xs , y > x]

myMult :: Int -> Int -> Int
myMult 0 _ = 0
myMult b a = (a * b)

primes :: [Int]
primes = filterPrime [2..]
    where
        filterPrime [] = error "should never happen!"
        filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

{-
(old, new) -> (new, new + old)
-}

fib :: Int -> Integer
fib 0 = 0
fib n = snd $ iterate (\(old, new) -> (new, new + old)) (0, 1) !! n

myIterate :: (a -> a) -> a -> [a]
myIterate next seed = seed : (myIterate next (next seed))

{-
 ( 8 + (9 + (10 + 0)))
-}

foldExample =
    foldl (\x s -> "(" ++ x  ++ " * " ++ show s ++")") "init" [1..5]

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr op init [] = init
myfoldr op init (x:xs) = x `op` (myfoldr op init xs)

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl = undefined

or' :: Bool -> Bool -> Bool
or' True _ = True
or' False x = x