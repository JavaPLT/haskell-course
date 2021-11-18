{-# LANGUAGE DeriveFunctor #-}

module Lecture4 where

import Data.Char
import Control.Monad

data Two = Zero | One
    deriving (Eq, Ord)

-- map :: (a -> b) -> [a] -> [b]
-- fmap :: (a -> b) -> f a -> f b
-- Functor 

data BinTree a = Leaf | Fork a (BinTree a) (BinTree a)
    deriving (Functor, Show)

tree1 = Fork 1 Leaf (Fork 2 Leaf Leaf)

binTreeMap :: (a -> b) -> BinTree a -> BinTree b
binTreeMap f Leaf = Leaf
binTreeMap f (Fork a t1 t2) = Fork (f a) (binTreeMap f t1) (binTreeMap f t2)


type Trace a = Int -> a

-- .... -3 -2  -1 0 1 2 3 ...
--       a  b   c d e f g 

traceMap :: (a -> b) -> Trace a -> Trace b
traceMap f t = f . t 

traceA :: Trace Char
traceA _ = 'a' 

{-

(1) fmap id v = v
(2) fmap f $ fmap g $ v = fmap (f . g) v


fmap = <$> 

class Functor f => Applicative f where
    pure :: a -> f a
    <*> :: f (a -> b) -> f a -> f b

(+) <$> (Just 10)
Just (10 +)

-}

{-
x :: Int
x = 5

x :: IO Int
x = read x from some file

unwrap x :: Int

unwrapIO :: IO a -> a
-}

{-
>>= :: IO a -> (a -> IO b) -> IO b
       (IO String) (String -> IO ())
-}

ignored :: IO ()
ignored = getLine >>= \_ -> putStrLn "You are ignored"

addHello :: String -> String
addHello name = "Hello, " ++ name

greet :: IO ()
greet = getLine >>= (\name -> putStrLn (addHello name))

main :: IO ()
main = greet

dummy :: IO String
dummy = pure "always the same"

greet2 :: IO ()
greet2 = putStrLn "Hello, what is your name?" 
        >> getLine 
        >>= \name -> let bigName = map toUpper name in putStrLn (addHello bigName)

greet3 :: IO ()
greet3 = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    putStrLn (addHello name)

greet4 :: IO ()
greet4 = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    let bigName = map toUpper name
    putStrLn (addHello bigName)

yes :: IO ()
yes = putStrLn "Yes" >> yes

yes1 = forever $ putStrLn "Yes"

-- getLine :: IO String
-- read :: String -> Int
-- fmap read getLine :: IO Int

isPositive :: IO ()
isPositive = do
    n <- read <$> getLine
    if (n > 0)
    then putStrLn "positive"
    else if (n == 0)
    then putStrLn "zero"
    else putStrLn "negative"