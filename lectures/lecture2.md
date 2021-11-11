## Compiler Warnings

It's a good thing to have compiler warnings enabled.

Add `{-# OPTIONS_GHC -Wall #-}` to the top of your file. Or add `-Wall` in the `package.yaml` file of your stack config.

## Lists

- Pattern matching on lists
  - `map`, `filter`
  - Types of `map` and `filter`
  - Other functions `take`, `drop`, `length`, `(!!)`, `zip`, `zipWith`

- Using currying
  - `map (+1) [1,2,3] = [2,3,4]`

- List comprehensions
  - `triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]`
  - `rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]`
  - We will revisit this concept again, as well.

- dotProduct

```haskell
dotProduct xs ys = go 0 xs ys
  where
    go n [] ys = n
    go n xs [] = n
    go n (x : xs) (y : ys) = go (n + (x * y)) xs ys
```

```haskell
dotProduct xs ys = sum $ zipWith (*) xs ys
```

- quicksort

```haskell
quicksort (x : xs) = lesser ++ [x] ++ greater
  where
    lesser = [y | y <- xs, y <= x]
    greater = [y | y <- xs, y >= x]
```

## Laziness

- Consider

```
myMult 0 a = 0
myMult b a = (a * b)
```

- `(1/0)` is undefined
  - But `myMult 0 (1/0)` is defined.

- `[1..]` does not terminate but `take 3 [1..]` works perfectly

- Sieve of Eratosthenes

```haskell

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

```

- `repeat`, `iterate`
  - `cycle`

## `foldl`, `foldr`, `foldl'`


- `foldr (\x y -> "( " ++ show x ++ " * " ++ y ++ ")") "init" [1..5]`
- `foldl (\x y -> "( " ++ x ++ " * " ++ show y ++ ")") "init" [1..5]`
- `foldr1`, `foldl1`
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