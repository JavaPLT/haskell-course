## Haskell Features

- Statically Typed with Type Inference
- Lazy
- Purely Functional
- Algebraic Datatypes
- Typeclasses

## Comments

- No more prefix notation `4 * 5`.
  - backtick notation ```3 `elem` [3,4,5]```
- Function application is written as `f x`

## Definitions

- Defining constants

```haskell
theAnswer :: Int
theAnswer = 42
```
- Simple definitions

```haskell
doubleMe :: Int -> Int
doubleMe x = 2 * x

```
- Lambda Notation

```haskell
doubleMe = \x -> 2 * x
```
- Type annotations are optional but encouraged
  - GHCi `:t`

- Currying
  - Try `:t` on `foo`

```haskell
foo x y = 2 * x + y
```

- Definitions by pattern matching

```haskell
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor False False = False
xor True True = False
```

- Wildcards

```haskell
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
```

- Patterns with variables

```haskell
and :: Bool -> Bool -> Bool
and True x = x
and False _ = False
```

- Fibonacci Function (Naive)
  - `Integer` is unbounded

```haskell
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

- Guards

```haskell
  myMax :: Int -> Int -> Int
  myMax m n
    | m > n = m
    | m < n = n
    | otherwise = m
```

- `where` clauses
  - Example from Learn You a Haskell
  - `String` is a list of `Char`

```haskell
bmiTell :: Float -> Float -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
```

- `let` clauses

```haskell
cylinder r h =
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
```

- `case` expressions

```haskell
and :: Bool -> Bool -> Bool
and y x =
  case y of
    True -> x
    False -> False
```

## Polymorphism

- Consider ```id x = x```. What type should we assign to `id`? `id :: Int -> Int` or `Bool -> Bool` is not general enough
  - `id :: a -> a`
  - `($) :: (a -> b) -> a -> b`
  - `(.) :: (b -> c) -> (a -> b) -> a -> c`
- We will revisit this concept again.