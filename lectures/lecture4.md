## Functors

- Consider `map :: (a -> b) -> [a] -> [b]`
  - Claim: We can generalize the list part
  - Some Examples follow

```
type Trace a = Int -> a
traceMap :: (a -> b) -> (Trace a) -> (Trace b)
```

```
type BinTree a = Leaf | Fork a (BinTree a) (BinTree a)
binTreeMap :: (a -> b) -> (BinTree a) -> (BinTree b)
```

- Can consider a general notion of map
  - `fmap :: (a -> b) -> f a -> f b`
  - We also want `fmap` to satisfy some properties
    - Preserve Identity: `fmap id = id`
    - Preserve Composition: `fmap f . fmap g = fmap (f . g)`

- Notice that the generalization we made was over a constructor, not a type
  - Higher kinded typeclass!

- `fmap` is also written as `(<$>)`
  - We can derive Functor.

- Type which is not a covariant functor
  - `newtype T c = T (c -> Int)`
  - `contramap :: Contravariant f => (a -> b) -> f b -> f a`

## Applicative

```
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
```

- `pure :: a -> f a` puts the `a` inside a box
- `<*> :: f (a -> b) -> f a -> f b` lets you use operations inside a box

```
m10, m20 :: Maybe Int
m10 = pure 10
m20 = pure 20

m10 :: Maybe Int
(+) :: Int -> (Int -> Int)
(+) <$> m10 :: Maybe (Int -> Int)
(+) <$> m10 <*> m20 :: Maybe Int
```

- Note: You could define Applicative and Functor yourself. They don't have to come with Haskell, they just do because that's handy.

## IO

- Haskell is pure
  - every function `A -> B` takes some value in A and gives you something in B.
  - A function only talks about an abstract rule, or an association.
  - It never does any side effect.
  - It is always deterministic.

- Haskell code **defines** things.
  - But we want actual code to **do** things.

- Haskell's effect system
  - View 1 : Carefully flag every side effect
  - We will focus on View 2

- Consider `getLine :: IO String`
  - `getLine` is not a `String`. Rather it is a recipe that produces a `String`.
  - In human language, the recipe looks something like "get a line from console and produce it"

- `putStrLn :: String -> IO ()`
  - `putStrLn "hello"` is a recipe that produces nothing useful.
  - In human language, `putStrLn "hello"` is the recipe "write 'hello' to the console, and do not produce anything"

- `(>>=) :: IO a -> (a -> IO b) -> IO b`

```haskell
getLine :: IO String
putStrLn :: String -> IO ()

echo :: IO ()
echo = getLine >>= putStrLn
```

- Consider `addHello :: String -> String`
  - Can also write `greet = getLine >>= (putStrLn . addHello)`

```haskell
addHello :: String -> String
addHello name = "Hello, " + name

greet :: IO ()
greet = getLine >>= \name -> putStrLn (addHello name)

```

- `main :: IO ()` describes what the program does
  - `main = greet`

- `IO Cake` is like a recipe to produce a `Cake`. It is not an actual `Cake`.
  - The whole program is a recipe for an action.
  - Inside a haskell program, we are manipulating recipes by composing them in different ways.
  - We never actually create a cake, we just create very complex **recipes** for creating a cake.
  - The whole program is a `IO Cake`.
  - The compiler is a chef who turns `IO Cake -> Cake`.

- The interface of IO
  - IO is a Functor. `(<$>)`
  - IO is an Applicative. `(<*>), pure`
  - IO is a Monad. `(>>=)`
  - Notice the absence of anything like `extract :: IO a -> a`
    - You **never** extract anything from an `IO a`, instead you use `>>=` to channel it to another function.

## Do Notation

- Consider previous example

```haskell
getLine >>=
    (\name ->  putStrLn ("Hey " ++ name ++ ", you rock!")
```

- Suppose we want to do one more action before this

```haskell
putStrLn "Hello, what's your name?"  >>=
  (\_ -> getLine >>=
    (\name ->  putStrLn ("Hey " ++ name ++ ", you rock!")
    )
  )
```

- The brackets are getting tedious
  - Notice how `name <- getLine` is translated to `getLine >>= \name -> ...`

```haskell
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
```

- let notation in do blocks

```haskell
import Data.Char

putStrLn "Hello, what's your name?"  >>=
  (\_ -> getLine >>=
    (\name ->  let bigName = map toUpper name in putStrLn ("Hey " ++ bigName ++ ", you rock!")
    )
  )
```

```haskell
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine
    let bigName = map toUpper name
    putStrLn ("Hey " ++ name ++ ", you rock!")  
```

- A do block cannot end in a `let` or a `<-`
- The last statement in the do block is the type of the entire expression.

## Some useful patterns for IO

- `>>`: `a >> b`. First do `a`, then do `b`. Example `putStrLn "Hello" >> putStrLn "Bye"`.
  - The expression `getLine >> getLine` takes two lines from STDIN, ignores the first. See the example below
  - How is `>>` implemented?

```haskell
foo = getLine >> getLine

main = do
  secondLine <- foo
  putStrLn $ "The second line was: " ++ secondLine
```

- loops

```haskell
yes = putStrLn "Yes" >> yes
yes1 = forever $ putStrLn "Yes"
```

- conditionals

```haskell
isPositive = do
  n <- read <$> getLine
  if (n > 0)
    then putStrLn "positive"
    else if (n == 0) then putStrLn "zero"
      else putStrLn "negative"
```

## A large example for IO

See https://github.com/Agnishom/PRGH17/blob/master/tut4/notes.md
