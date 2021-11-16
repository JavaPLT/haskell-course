## `foldl`, `foldr`, `foldl'`

- Definition of `foldr`
```haskell
foldr op init [] = init
foldr op init (x:xs) = x `op` (foldr op init xs)
```
- Definition of `foldl`
```haskell
foldl op init [] = init
foldl op init (x:xs) = foldl op (init `op` x) xs
```
- `foldr` lends itself to laziness. `foldr (&&) False (repeat False)`
- `foldl` does not lend it to laziness, but it can be more efficient in some cases, with some cleverness.

- Consider `foldr (+) 0 [1..1000000]`.

```
foldr (+) 0 [1..1000000]
= 1 + (foldr (+) 0 [2..1000000])
= 1 + (2 + (foldr (+) 0 [3..1000000]))
.
.
.
= 1 + (2 + ... + (1000000 + 0)))
= 1 + (2 + ... (+ 1000000))
=
.
.
.
.
= ...
```

- `foldl (+) 0 [1..1000000]`

```
foldl (+) 0 [1..1000000]
= foldl (+) (0+1) [2..1000000]
= foldl (+) ((0+1)+2) [3..1000000]
.
.
.
= ((0 + 1) + 2) ... 1000000)
.
.
.
= ....
```

- `foldl'`

```haskell
foldl' op init [] = init
foldl' op init (x:xs) = let z = (init `op` x) in seq z (foldl' op z xs)
```

## Debug.Trace

- `import Debug.Trace`

```
myfun a b | trace ("myfun " ++ show a ++ " " ++ show b) False = undefined
myfun a b = ...
```

```
cylinder r h
  | trace ("calling cylinder with r = " ++ show r ++ " and h = " ++ show h) False =
    undefined
cylinder r h =
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
```

## Modules

- `import`
  - `import ... hiding ...`
  - `import qualified ...`
  - `import qualified ... as ...`
- `module ... (...) where`


## Algebraic Datatypes

- Type Synnonyms

`type IncreasingList = [Int]`

- Type Synnonyms can have parameters

`type Trace a = Int -> a`

- Newtypes
  - type Synnonyms are interchangable; Newtypes are not
  - Newtypes are strict

```
newtype Feet = Feet Double
newtype Cm   = Cm   Double
```

- Products

```
data Ellipse = MkEllipse Float Float
```

- Pattern Matching on Products

```
ellipseArea :: Ellipse -> Float
ellipseArea (MkEllipse major minor) = pi * major * minor
```

- Using the same name for the constructor and the type is common

```
data Ellipse = Ellipse Float Float
```

- Record Syntax

```
data Ellipse = Ellipse {major :: Float, minor :: Float}
```

- Interlude: `deriving Show`

- Updating Record Syntax

```
ellipseA = Ellipse 1.0 9.0
ellipseB = ellipseA {major = 10.0}
```

- Sums

```
data Day = Saturday | Sunday | Monday | Tuesday | Wednesday | Thursday | Friday
```

- Sums of Products

```
data Shape = Rectangle Float Float | Square Float
```

```
area :: Shape -> Float
area (Rectangle a b) = a * b
area (Square a) = a * a
```

- Inductive/Co-Inductive Types
  - Carefully check which are the constructors here and which are the types
  - `:t Singleton` makes sense but `:t NonEmpty` does not

```
data List a = Nil | Cons a (List a)
data NonEmpty a = Singleton a | More a (NonEmpty a)
data Stream a = Stream { head :: a, tail :: (Stream a) }
data Mealy a b = Mealy { mOut :: a -> b, mNext :: a -> Mealy a b }
```

- CoInductive Examples

```
ones :: Stream Int
ones = Stream 1 ones
```

```
mealySimple :: Mealy Int String
mealySimple = Mealy { mOut = show, mNext = \x -> mealySimple }
```

- Tuples are anonymous products
  - What are anonymous sums? `Either`
  - `(->)` are anonymous exponentials

- Option Type: `Maybe`
  - The first program is partial
  - The second program isn't

```
lookup :: Int -> [(Int, b)] -> b
lookup i tab = snd . head $ filter (\(x,y) -> x ==  i) tab
```

```
lookup _ [] = Nothing
lookup i (k, v):xs
  | i == k    = Just v
  | otherwise = lookup i xs
```

## Kinds

- Kinds are types of Types
- `:k Int`
- `:k (Maybe Int)`
- `:k Maybe`
- `:k Either`
- Consider `data Fix f = Fix { unFix :: f (Fix f) }`.
  - `Fix` has kind `(* -> *) -> *`

## Typeclasses

- What does `deriving Show` do?
  - `Show` is a typeclass. Similar to interfaces in Java
  - `:i Show`

```
data Ellipse = Ellipse {major :: Float, minor :: Float}

instance Show Ellipse where
    show (Ellipse major minor)
        | major == minor = "Just a circle"
        | otherwise = "Another ellipse"
```

- How can we generalize `lookup`?
  - Nothing in the code prevents us from generalizing `Int` to `a`

```
myLookup :: Int -> [(Int, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup i (k, v):xs
  | i == k    = Just v
  | otherwise = lookup i xs
```

- Paramteric vs Ad-Hoc Polymorphism

- generalizing quicksort
  - The `Ord` typeclass

```
quicksort (x : xs) = lesser ++ [x] ++ greater
  where
    lesser = [y | y <- xs, y <= x]
    greater = [y | y <- xs, y >= x]
```

- Playing around with dependencies
  - `Eq a => Ord a`. For `a` to be `Ord`, it must first be `Eq`
  - New typeclass resolution from old `Ord a => Ord [a]`

```
data Two = One | Two deriving (Ord, Show)

testCompare = [One, Two] < [One, One]
```

- Constraint `:k Ord`