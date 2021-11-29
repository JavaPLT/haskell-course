# Writing a Tic-Tac-Toe Program with Haskell (contd.)

## Designing an Interface 

To design an interface, we need to think about the game loop. The game cycles through these states

```
-> player's turn -> has player won? -> computer's turn -> has computer won? -> player's turn -> ...
```

Additionally, from one state to another, we may need to update the knowledge base and the position.

### Command Line Interface

We will define `ControlState` to mark our position in the loop.

```haskell
data ControlState = 
      PlayersTurn 
    | HasPlayerWon
    | ComputersTurn
    | HasComputerWon
    | GameOver Player
```

And, we will package that together with the knowledge base and the position in a `GameState`:

```haskell
data GameState = GameState {
        pos :: Position
    ,   kb :: KnowledgeBase
    ,   loopState :: ControlState
    }
```

We will write a function `gameLoop :: GameState -> IO ()` that runs the game loop given an initial state.

```haskell
gameLoop :: GameState -> IO ()
gameLoop gs = case loopState gs of
    GameOver p -> undefined
    PlayersTurn -> undefined
    HasPlayerWon -> undefined
    ComputersTurn -> undefined
    HasComputerWon -> undefined
```

If we are at the `GameOver p` state, we will name the print the winner.

```haskell
gameLoop :: GameState -> IO ()
gameLoop gs = case loopState gs of
    GameOver p -> handleGameOver p
    ...

handleGameOver :: Player -> IO ()
handleGameOver p = putStrLn $ show p ++ " has won!"
```

If we are at `PlayersTurn`, we will prompt the user for a move.

```haskell
gameLoop gs = case loopState gs of
    ...
    PlayersTurn -> do
        pos' <- handlePlayersTurn (pos gs) 
        print $ (curBoard $ pos')
        gameLoop $ gs { pos = pos', loopState = HasPlayerWon }
    ...

getCoordinates :: IO (Int, Int)
getCoordinates = do
  putStrLn "Your move (row, column):"
  read <$> getLine

handlePlayersTurn :: Position -> IO Position
handlePlayersTurn pos@(Position board player) = do
  move <- getCoordinates
  case putMark board player move of
      Nothing -> do
          putStrLn "Please Try Again"
          handlePlayersTurn pos
      Just newBoard -> pure (Position newBoard (nextPlayer player))
```

Note how we are transitioning into `gameLoop {... HasPlayerWon ...}` from this state.

Next, we handle the `HasPlayerWon` state.

```haskell
gameLoop gs = case loopState gs of
    ...
    HasPlayerWon -> do
            case boardWinner (curBoard $ pos gs) of
                Just p -> gameLoop $ gs { loopState = GameOver p }
                Nothing -> gameLoop $ gs { loopState = ComputersTurn }
    ...
```

Next, we handle the `ComputersTurn` state.

```haskell
gameLoop gs = case loopState gs of
    ...
    ComputersTurn -> do
            let (pos', kb') = runState (bestResponse $ pos gs) (kb gs)
            putStrLn "Computer's Move:"
            print $ (curBoard $ pos')
            gameLoop $ gs { pos = pos', kb = kb' , loopState = HasComputerWon }
    ...
```
Note the use of `runState` to get the new position and knowledge base.

Finally, we handle the `HasComputerWon` state.

```haskell
gameLoop gs = case loopState gs of
    ...
    HasComputerWon -> do
            case boardWinner (curBoard $ pos gs) of
                Just p -> gameLoop $ gs { loopState = GameOver p }
                Nothing -> gameLoop $ gs { loopState = PlayersTurn }
    ...
```

The main action will call `gameLoop` with the initial state.

```haskell
main :: IO ()
main = do
    print initBoard
    gameLoop $ GameState (Position initBoard X) Map.empty PlayersTurn
```

### Graphical Interface with Gloss