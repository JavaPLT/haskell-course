module Lecture5 where

import Control.Monad.State

{-

-- functor
fmap :: (a -> b) -> IO a -> IO b
fmap :: (a -> b) -> [a] -> [b]

-- applicative
pure :: a -> IO a
<*> :: IO (a -> b) -> IO a -> IO b

--monad
(>>=) :: IO a -> (a -> IO b) -> IO b
(=<<) :: (a -> IO b) -> IO a -> IO b
-- chainMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- concatMap :: (a -> [b]) -> [a] -> [b]

Functor f => Applicative m => Monad m

-}

phonebook :: [(String, String)]
phonebook = [ ("Bob",   "01788 665242"),
              ("Fred",  "01624 556442"),
              ("Alice", "01889 985333"),
              ("Jane",  "01732 187565") ]

registrationID :: [(String, Int)]
registrationID = [ ("01788 665242", 1)
                 , ("01624 556442", 2)
                 , ("01889 985333", 3)
                 ]

moneyOwed :: [(Int, Double)]
moneyOwed = [   (1, 200.0)
            ,   (2, 60.0)
            ,   (4, 100.0)
            ]

nameToMoney :: String -> Maybe Double
nameToMoney name =
    case (lookup name phonebook :: Maybe String) of
        Nothing -> (Nothing :: Maybe Double)
        Just num -> case lookup num registrationID of
            Nothing -> Nothing
            Just reg -> lookup reg moneyOwed

-- >>> nameToMoney "Alice"
-- Nothing

chainMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
chainMaybe Nothing _ = Nothing
chainMaybe (Just v) f = f v

nameToMoney' :: String -> Maybe Double
nameToMoney' name = do
    num <- lookup name phonebook 
    reg <- lookup num registrationID
    lookup reg moneyOwed

add50ToMoney :: String -> Maybe Double
add50ToMoney name = (+ 50.0) <$> (nameToMoney' name)

-- >>> nameToMoney' "Alice"
-- Nothing

--- >>> add50ToMoney "Bob"
-- Just 250.0
--- >>> nameToMoney "Bob"
-- Just 200.0


knightMove :: (Int, Int) -> [(Int, Int)]
knightMove (x, y) = [(x + i, y + j)| i <- [-2, 2], j <- [-1, 1]]
                    ++ [(x + i, y + j)| i <- [-1, 1], j <- [-2, 2]]

knight2 :: (Int, Int) -> [(Int, Int)]
knight2 (x, y) = concatMap knightMove $ knightMove (x, y)

knight3 :: (Int, Int) -> [(Int, Int)]
knight3 (x, y) = knightMove =<< knightMove =<< knightMove (x, y)
knight3' (x, y) = knightMove (x, y) >>= knightMove >>= knightMove
knight3'' (x, y) = pure (x, y) >>= knightMove  >>= knightMove >>= knightMove

-- f :: a -> m b
-- f x == pure x >>= x
-- knightMove (x, y) == concatMap knightMove [(x, y)]

triangles = [ (a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10]]

-- >>> triangles

triangles' = do
    a <- [1..10]
    b <- [1..10]
    c <- [1..10]
    pure (a, b, c)

-- >>> concatMap (\x -> concatMap (\y -> [(x, y)]) [1..5]) [1..5]

-- f :: a -> b
-- mulif :: a -> [b]

insert :: Int -> a -> [a] -> [a]
insert n x xs = take n xs ++ [x] ++ drop n xs

-- >>> insert 3 'a' ['w', 'x', 'y', 'z']
-- "wxyaz"

permutations :: [a] -> [[a]]
permutations [] = pure []
permutations (x:xs) = do
    n <- [0..length xs]
    xs' <- permutations xs
    pure $ insert n x xs'

-- >>> permutations [1, 2, 3]
-- [[1,2,3],[1,3,2],[2,1,3],[3,1,2],[2,3,1],[3,2,1]]

-- NL = co-NL

data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving Show


abstract :: Eq a => Tree a -> Tree Int
abstract t = evalState (abstractAux t) []

type Table a = [a]

findIndex :: Eq a => a -> Table a -> Maybe Int
findIndex x t = lookup x $ zip t [1..]

addToTable :: a -> Table a -> Table a
addToTable x = (++ [x])

-- get :: State s r -> State s s
-- put :: s -> State s ()
-- evalState :: State s a -> s -> a

abstractAux :: Eq a => Tree a -> State (Table a) (Tree Int)
abstractAux Nil = pure Nil
abstractAux (Node x t1 t2) = do
    curTable <- get
    n' <- case findIndex x curTable of
            Just n -> pure n
            Nothing -> do
                let newTable = addToTable x curTable
                put newTable
                pure $ length newTable
    tree1' <- abstractAux t1
    tree2' <- abstractAux t2
    pure $ Node n' tree1' tree2'

treeExample :: Tree Char
treeExample = Node 'b' (Node 'b' (Node 'a' Nil Nil) (Node 'b' Nil Nil)) (Node 'a' Nil Nil)

-- >>> abstract treeExample
-- Node 1 (Node 1 (Node 2 Nil Nil) (Node 1 Nil Nil)) (Node 2 Nil Nil)
