module Lecture1 where

x :: Int
x = 4 * 5

doubleMe :: Int -> Int
doubleMe x = 2 * x

doubleMe'' :: Int -> Int
doubleMe'' = (2 * )

doubleMe' = \x -> 2 * x

foo :: Int -> (Int -> Int)
foo x y = 2 * x + y

-- (foo 6) 2

foo' :: (Int, Int) -> Int
foo' (x, y) = 2 * x + y

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor True True = False
xor False False = False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

and :: Bool -> Bool -> Bool
and True x = x
and False _ = False

and' :: Bool -> Bool -> Bool
and' x y = case x of
    True -> y
    False -> False

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

myMax :: Int -> Int -> Int
myMax m n
    | m > n = m
    | m < n = n
    | otherwise = m

cylinderArea :: Float -> Float -> Float
cylinderArea r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r * r
    in sideArea + 2 * topArea

cylinderArea' :: Float -> Float -> Float
cylinderArea' r h = sideArea + 2 * topArea
    where
        sideArea = 2 * pi * r * h
        topArea = pi * r * r

identity :: typeee -> typeee
identity x = x

apply :: 
    (a -> b) -- type of f
    -> a -- type of x
    -> b -- return type
apply f x = f x

compose :: 
    (b -> c)         -- type of f
    -> (a -> b)          -- type of g
    -> a -> c                  -- ?
compose f g = \x -> f (g x)

{-

g >>> f

-}