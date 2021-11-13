## IO and its interfaces

* Functor

```haskell
fmap, (<$>) :: (a -> b) -> IO a -> IO b
```

* Applicative 

```haskell
pure, return :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b 
```

* Monad

```haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```

* Remember that

```haskell
class Functor f => Applicative f
class Applicative m => Monad m
```

## Chaining Lookups

* Consider some tables that look like this.

```haskell
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
```

* Lookup
    - May occassionally fail

```haskell
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```

* Given name, find money owed.

```haskell
nameToMoney :: String -> Maybe Double
nameToMoney name =
    case lookup name phonebook of
        Nothing -> Nothing
        Just num -> case lookup num registrationID of
            Nothing -> Nothing 
            reg -> lookup reg moneyOwed
```

* Very inconvenient! Consider instead `chainMaybe`.

```haskell
chainMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
chainMaybe Nothing _ = Nothing
chainMaybe (Just a) f = f a
```

* Now, we can rephrase

```haskell
nameToMoney :: String -> Maybe Double
nameToMoney name = 
    lookup name phonebook
    `chainMaybe` \num -> lookup num registrationID
    `chainMaybe` \reg -> lookup reg moneyOwed
```

* Notice the familiarity?

```haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
(chainMaybe) :: Maybe a -> (a -> Maybe b) -> Maybe b
```

* `chainMaybe` is already implemented in the Prelude as `(>>=)`

```haskell
nameToMoney :: String -> Maybe Double
nameToMoney name = 
    lookup name phonebook
    >>= \num -> lookup num registrationID
    >>= \reg -> lookup reg moneyOwed
```

* Lets give the Functor and Applicative instances to `Maybe`

```haskell
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Applicative Maybe where
    pure a = Just a
    (Just f) (<*>) (Just x) = Just (f x)
```

* And the Monad

```haskell
instance Monad Maybe where
    (>>=) = chainMaybe
```

* `do` Notation

```haskell
nameToMoney :: String -> Maybe Double
nameToMoney name = do
   num <- lookup name phonebook 
   reg <- lookup num registrationID
   lookup reg moneyOwed
```

## Lists

* Consider `concatMap`

```
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = concat $ map f xs
```

* Knights Move
    - Two steps in one direction, one step in an orthogonal direction

```haskell
knightMove :: (Int, Int) -> [(Int, Int)]
knightMove (x, y) = [ (x + i,  y + j) | i <- [-2, 2], j <- [-1, 1]]
                 ++ [ (x + i,  y + j) | i <- [-1, 1], j <- [-2, 2]]
```

* Where does the knight go after two moves?

```haskell
knight2  (x, y) = concatMap knightMove $ knightMove (x, y)
```

After three moves

```haskell
knight3 (x, y) = concatMap knightMove $ concatMap knightMove $ knightMove (x, y)
```

* Also written as

```haskell
knight3 (x, y) = knightMove `concatMap` knightMove `concatMap` knightMove (x, y)
```

* List is also already a monad

```haskell
knight3 (x, y) = knightMove =<< knightMove =<< knightMove (x, y)
```

or 

```haskell
knight3 (x, y) = knightMove (x, y) >>= knightMove >>= knightMove
```

* `(>>=)` is just `flip concatMap`

* `pure` creates a singleton list, i.e, `pure x = [x]`
    - So, we can rephrase the above as

```haskell
knight3 (x, y) = pure (x, y) >>= knightMove >>= knightMove >>= knightMove
```

* `fmap` is just `map` (of course!)
    - `fs (<*>) xs` applies every `f` in `fs` to every `x` in `xs`
    - Try `[(*2), (+1)] <*> [10, 11]`

* List comprehensions are really just `do` notation in disguise

```haskell
triangles = [(a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10]]
```

can be written as

```haskell
triangles = do
    c <- [1..10]
    b <- [1..10]
    a <- [1..10]
    pure (a, b, c)
```

* Think of a computation inside the list monad as a non-deterministic computation. You could read the above code as, non-deterministically pick c, non-deterministically pick b, and non-deterministically pick a, and then return the triple (a, b, c). The result is a non-deterministic way to pick a triple.

* Let's use the idea of non-determinism to compute the list of all permuations of a list.
  - How can we pick an arbitrary permuation?
  - First, arbitrarily arrange the rest of the elements of the list.
  - Then, put the first element at an arbitrary location.

```haskell
insert :: Int -> a -> [a] -> [a]
insert n x xs = take n xs ++ [x] ++ drop n xs

permutations :: [a] -> [[a]]
permutations [] = pure []
permutations (x:xs) = do
    n <- [0..length xs]
    xs' <- permutations xs
    pure $ insert n x xs'
```

You can also phrase this with `concatMap`.


## State

```haskell
Tree a = Nil | Node a (Tree a) (Tree a)
```

* Problem: Define `abstract :: Eq a => Tree a -> Tree Int` such that two nodes are replaced with equal integers iff their current labels are equal.

* First, we will define a `Table` on which we can perform lookups to keep track of which key maps to what integer.

```haskell
type Table a = [a]

findIndex :: Eq a => a -> Table a -> Maybe Int
findIndex x t = lookup x $ zip t [1..]

addToTable :: a -> Table a -> Table a
addToTable x = (++ [x])
```

* Now, we will define `abstract` as a stateful function.
    - Think of `State s r` as a value of type `r` that can be computed by modifying a state of type `s`.
    - We want to define `abstractAux :: Eq a => Tree a -> State (Table a) (Tree Int)`
    - Given some table `t`, `abstractAux t` uses a table to compute the abstracted tree.

* We need to be able use the state.
    - We can use `get` to get the current table.
    - We can use `put` to update the table.

```
abstractAux1 :: Eq a => Tree a -> State (Table a) (Tree Int)
abstractAux1 Nil = pure Nil
abstractAux1 (Node x t1 t2) = do
    curTable <- get
    n <- case findIndex x curTable of
        Just n' -> pure n'
        Nothing -> do
            let newTable = addToTable x curTable
            put newTable
            pure $ length newTable
    Node n <$> (abstractAux1 t1) <*> (abstractAux1 t2)
```

* We can run the stateful computation by using `evalState`.

```haskell
abstract :: Eq a => Tree a -> Tree Int
abstract t = evalState (abstractAux1 t) []
```

* Here are the types of `get`, `put` and `evalState`.
    - Suppose we are using `s` as the type of `State`. Then, the value that `get` is returning and using as state is both `s`.
    - `put` takes a value of type `s` and puts that as the state, it does not give us any result, so the result type is `()`.
    - `evalState` takes a stateful computation and an initial state and runs the computation with the given initial state.

```haskell
get :: State s s
put :: s -> State s ()
evalState :: State s a -> s -> a
```

* All of this can be implemented as purely functional code.

```haskell
newtype MyState s r = MyState {myRun :: s -> (s, r)}

myGet :: MyState s s
myGet = MyState $ \s -> (s, s)

myPut :: MyState s ()
myPut = MyState $ \s -> (s, ())

myEval :: s -> MyState s r -> r
myEval s ms = snd $ myRun ms s

instance Functor (MyState s) where
    fmap f ms = MyState $ \x -> let (s, r) = myRun ms x in (s, f r)

instance Applicative (MyState s) where
    pure a = MyState $ \s -> (s, a)
    sf <*> sa = MyState $ \s -> let (s', f) = myRun sf s
                                    (s'', a) = myRun sa s'
                                 in (s'', f a)

instance Monad (MyState s) where
    sa >>= f = MyState $ \s -> let (s', a) = myRun sa s in myRun (f a) s'
```