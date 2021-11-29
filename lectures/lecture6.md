# Writing a Tic-Tac-Toe Program with Haskell

Tic-Tac-Toe, Xs and Os, Knaughts and Crosses is a popular game. See [Wikipedia](https://en.wikipedia.org/wiki/Tic-tac-toe) if you are not familiar with it

## Setting up a new Stack project

To set up a new stack project, run `stack new tictactoe`. This will create a new directory called `tictactoe` in the current directory, with a project template.

The `package.yaml` file contains, among other things, a list of packages that is used by the current project. We will eventually modify the `dependencies` section as follows:

```
dependencies:
- base >= 4.7 && < 5
- containers
- mtl
- gloss
```

`base` is the Haskell standard library, that is part of almost every Haskell based project. We will describe what the other packages are for as we need them.

As usual, you run `stack build` to build the project, `stack test` to run tests, and `stack exec tictactoe-exe` to run the executable.

When you add new dependencies, `stack build` may require a little bit of time to download and compile these dependencies.

The `src/` folder contains most of the Haskell code. The `test/` folder contains the tests. The `app/Main.hs` file is what will be compiled to an executable.

## Describing the Game Board

### Player Type

First, we define a player type 

```haskell
data Player = X | O
    deriving (Show)
```

(TODO: Add Eq, Ord. Explain why.)

### Board Type

Next, we need a type for the board. There are a lot of options here:
- Three lists of size 3 lists
- A size 9 list
- A size 9 array
- A function from a finite type

What are the tradeoffs of each of these?

We will choose a key-value Map. This is implemented using Size Balanced Binary Trees. See [Data.Map](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html) for more information.

(This is not necessarily the best software engineering choice, but a pedagogical excuse to introduce this library.)

To use maps, we need to do a few things:

- First, we need the `containers` dependency added in our `package.yaml`.
- Second, we need to import the `Data.Map` module.

```haskell
import Data.Map (Map, (!))
import qualified Data.Map as Map
```

Now, we will define the board type as follows:

```haskell
newtype Board = Board (Map (Int, Int) (Maybe Player))
```

(TODO: Add Eq, Ord. Explain why.)

Thus, a board is a map from a pair `(Int, Int)` to a `Maybe Player`. The `Nothing` represents an empty square.

Note that this is not a very _safe_ way to represent a board. Adding some thing to an arbitrary coordinate is allowed, but it does not make sense for our application. So, we will provide additional functions and wrap this in a module. Then, we will not have to modify the board directly, but using these functions. This is why we have the `Board` module. 

### Accessor Functions

First, we define some getters and setters for the board.

```haskell
getMark :: Board -> (Int, Int) -> Maybe Player
getMark (Board board) (x, y)
    | x < 0 || x > 2 || y < 0 || y > 2 = error "Invalid coordinates"
    | otherwise = board ! (x, y)

putMark :: Board -> Player -> (Int, Int) -> Maybe Board
putMark (Board board) player (x, y)
    | x < 0 || x > 2 || y < 0 || y > 2 = error $ "Invalid coordinates" ++ show (x, y)
    | Data.Maybe.isJust (board ! (x, y)) = Nothing
    | otherwise = Just $ Board $ Map.insert (x, y) (Just player) board
```

We will need a few other definitions as well.

```haskell
initBoard :: Board
initBoard = Board $ Map.fromList [((x, y), Nothing) | x <- [0..2], y <- [0..2]]

emptySquares :: Board -> [(Int, Int)]
emptySquares (Board board) = [(x, y) | x <- [0..2], y <- [0..2], isNothing (board ! (x, y))]
```

We also want to be able to pretty print the board.

```haskell
instance Show Board where
    show (Board board) =
        intercalate "\n- - - \n"
            [ ( intercalate "|" [prettyShow $ board ! (x, y) | y <- [0..2]] )
                | x <- [0..2]]
            where
                prettyShow Nothing = " "
                prettyShow (Just X) = "X"
                prettyShow (Just O) = "O"
```


## Evaluating Positions

### Win/Lose/Draw Positions

For convenience, define a `Line` to be a list of coordinates.

```haskell
type Line = [(Int, Int)]
```

Here is a list of the winning lines:

```haskell
winningLines :: [Line]
winningLines = [ [(x, y) | x <- [0..2]] | y <- [0..2]] ++ -- vertical lines
               [ [(x, y) | y <- [0..2]] | x <- [0..2]] ++ -- horizontal lines
               [[(0, 0), (1, 1), (2, 2)], -- main diagonal
                [(0, 2), (1, 1), (2, 0)]] -- off diagonal 
```

Then, define a `lineWinner :: Board -> Line -> Maybe Player` which given a board and a line, returns the winner on that line if there is one.

```haskell
lineWinner :: Board -> Line -> Maybe Player
lineWinner b l
    | all (== Just X) marks = Just X
    | all (== Just O) marks = Just O
    | otherwise = Nothing
    where
       marks = map (getMark b) l
```

First, define the function `<|>`, that takes the first non-Nothing value as follows:

```haskell
(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> x = x
x <|> Nothing = x
Just x <|> Just y = Just x
```

Suppose the winning lines are `l1, l2, l3 ...`. Then, the winner of the board is
`lineWinner b l1 <|> lineWinner b l2 <|> lineWinner b l3 ... <|> Nothing`. We can write this as follows:

```haskell
boardWinner :: Board -> Maybe Player
boardWinner b = foldr ((<|>) . lineWinner b) Nothing winningLines
```

(The assumption is that we will stop our game when there is a winner.)

Also, we define `isDraw` as follows:

```haskell
isDraw :: Board -> Bool
isDraw b = null (emptySquares b) && isNothing (boardWinner b)
```


### Using Minimax to evaluate other positions

So far, we have identified terminal positions in the game tree. In other words, we can tell when the game has ended and whether there is a winner or the game is a draw.

We want to be able to say something about other positions in the game tree. Eventually, we want to be able to tell what is the best next move.

We will start by defining a `Position` type that packages the current board and the current player. Call a position winning if there is a winning strategy for the current player. Conversely, let a losing position be a position where the current player has no winning strategy. A winning position must either be a terminal position or a position at which all the successors are losing positions.

```haskell
data Position = Position { curBoard :: Board, curPlayer :: Player }

isWinning :: Position -> Bool
isWinning pos@(Position b p) =
    case boardWinner b of
        Just p' -> p == p'
        Nothing -> (not . null $ emptySquares b)
                && (any isLosing $ succPositions pos)

isLosing :: Position -> Bool
isLosing pos@(Position b p) =
    case boardWinner b of
        Just p' -> p /= p'
        Nothing -> (not . null $ emptySquares b)
                && (all isWinning $ succPositions pos)
```

This requires a few more a defintion for `succPositions`. Given a position, we can extract all the empty squares from the board, then for each of them, we can mark that square to create the next position.

```haskell
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

succPositions :: Position -> [Position]
succPositions (Position b p) = newPosition . fromJust . markSquare <$> emptySquares b
    where
        newPosition b' = Position { curBoard = b', curPlayer = nextPlayer p }
        markSquare = putMark b p
```

We add two boards to `Board.hs` for testing these functions.

```haskell
allX :: Board
allX = Board $ Map.fromList [((x, y), Just X) | x <- [0..2], y <- [0..2]]

allO :: Board
allO = Board $ Map.fromList [((x, y), Just O) | x <- [0..2], y <- [0..2]]
```

Another interesting position is

```haskell
-- x has not yet won, but he has a winning strategy from here
xBoard2 :: Board
xBoard2 = fromJust $ do
    b1 <- putMark initBoard X (1, 1)
    b2 <- putMark b1 O (2, 1)
    b3 <- putMark b2 X (0, 0)
    b4 <- putMark b3 O (2, 2)
    putMark b4 X (2, 0)
```

Now, given a winning position, the winning strategy is clear. We simply have to chose the successor node in the game tree that is losing.

We can improve this design in two ways.
- The current definition requires evaluating many positions again and again. We can do better by caching these results.
- Given a choice of two winning strategies, we want to choose the one that will win in fewer moves.

Let's start by defining a notion of `Score`. 

```haskell
data Label = Win | Lose | Draw
    deriving (Show, Eq)
data Score = Score { label :: Label, height :: Int }
    deriving (Show, Eq)
```

A score which is a win in fewer moves is considered superior. Hence:

```haskell
instance Ord Score where
    (Score Win i) <= (Score Win j) = i >= j
    (Score Win _) <= _ = False
    (Score Lose i) <= (Score Lose j) = i <= j
    (Score Lose _) <= _  = True
    (Score Draw i) <= (Score Draw j) = i >= j
    (Score Draw _) <= (Score Win _) = True
    (Score Draw _) <= (Score Lose _) = False
```

We will use a `Map` to store scores, and we will define `scorePosition` and `bestResponse` as a stateful function. We will require `mtl` in our `package.yaml` in order to use the `State` monad.

```haskell
type KnowledgeBase = Map Position Score
scorePosition :: Position -> State KnowledgeBase Score
bestResponse :: Position -> State KnowledgeBase Position
```

To use this, we will need Position to be `Ord`. That will require Board to be `Ord`, which will require Player to be `Ord`. Also, recall that `Ord` requires `Eq`.

```haskell
scorePosition pos@(Position b p)
    | winner == Just p = pure $ Score { label = Win, height = 0 }
    | Just _ <- winner = pure $ Score { label = Lose, height = 0 }
    | null (emptySquares b) = pure $ Score { label = Draw, height = 0 }
    where winner = boardWinner b
scorePosition pos@(Position b p) =
    do
        knowledge <- gets (Map.lookup pos)
        case knowledge of
            Just s -> pure s
            Nothing -> do
                let nextPositions = succPositions pos
                nextScores <- mapM scorePosition nextPositions
                let bestSuccScore = minimum nextScores
                let score = curScore bestSuccScore
                modify (Map.insert pos score)
                pure score

-- given the minimum score among the successors,
-- compute the current score
curScore :: Score -> Score
curScore (Score Win i) = Score Lose (i + 1)
curScore (Score Lose i) = Score Win (i + 1)
curScore (Score Draw i) = Score Draw (i + 1)

bestResponse pos@(Position b p) =
    do
        let nextPositions = succPositions pos
        nextScores <- mapM scorePosition nextPositions
        let bestSucc = snd $ minimumBy (\(s1, p1) (s2, p2) -> compare s1 s2) $ zip nextScores nextPositions
        pure bestSucc
```

Thorough testing is very important. Some testing ideas:

- Test `scorePosition` 
    + on some terminal positions.
    + on some positions that are draw in a few moves.
    + on some positions that are winning/losing in a few moves.
- Test `bestResponse`
    + to see if partial lines are blocked by the opponent.
    + to see if partial lines are completed by the player.
