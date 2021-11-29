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

We will use Gloss here to render the interface. Gloss is a library for rendering 2d graphics.

Gloss has a `play` function that takes in four parameters (among other things):
- An Initial value of some type `state`
- A function of type `state -> Picture`
- A function of type `Event -> state -> state` that handles events
- A function of type `Float -> state -> state` that updates the state given a time delta.

We will use `GameState` as our `state` type, as before. We will divide the transitions into two parts: Those that require event handling (`PlayersTurn`) and those that should automatically take place (`HasPlayerWon`, `ComputersTurn`, `HasComputerWon`).

```haskell
gameTime :: GameState -> GameState
gameTime gs = case loopState gs of
  PlayersTurn -> gs
  HasPlayerWon -> undefined
  ComputersTurn -> undefined
  HasComputerWon -> undefined
  GameOver _ -> gs

gameEvent :: Size -> Event -> GameState -> GameState
gameEvent k (EventKey (MouseButton LeftButton) Down _ (x', y')) gs =
  case loopState gs of 
    PlayersTurn -> undefined
    _ -> gs
gameEvent _ _ gs = gs
```

If we are at `PlayersTurn`, we need to wait for an event. If we are at some other state, no event handling needs to be done. We have divided the transitions accordingly. Also, if we are at `GameOver`, we will keep the state as is.

The full functions are as follows.

```haskell
gameEvent :: Size -> Event -> GameState -> GameState
gameEvent k (EventKey (MouseButton LeftButton) Down _ (x', y')) gs =
  case loopState gs of 
    PlayersTurn ->
      let newBoard = do
              (i, j) <- getCoordinates k (x', y')
              putMark (curBoard $ pos gs) (curPlayer $ pos gs) (i, j)
      in case newBoard of
        Nothing -> gs
        Just b -> gs { pos = Position {
                              curBoard = b
                            , curPlayer = nextPlayer (curPlayer $ pos gs)
                            }
                    , loopState = HasPlayerWon
                      }
    _ -> gs
gameEvent _ _ gs = gs

gameTime :: GameState -> GameState
gameTime gs = case loopState gs of
  PlayersTurn -> gs
  HasPlayerWon -> case boardWinner . curBoard $ pos gs of
    Just p -> gs { loopState = GameOver p }
    Nothing -> gs { loopState = ComputersTurn }
  ComputersTurn -> 
    let (pos', kb') = runState (bestResponse $ pos gs) (kb gs)
    in gs { pos = pos'
          , kb = kb'
          , loopState = HasComputerWon
          }
  HasComputerWon -> case boardWinner . curBoard $ pos gs of
    Just p -> gs { loopState = GameOver p }
    Nothing -> gs { loopState = PlayersTurn }
  GameOver _ -> gs
```

We define the `drawGame` function in a manner that replaces all elements of the board with the winning symbol if someone has won.

```haskell
drawGame :: Size -> GameState -> Picture
drawGame k gs = case loopState gs of
    GameOver p -> drawBoard k (case p of X -> allX; O -> allO)
    _ -> drawBoard k (curBoard $ pos gs)
```

There is some additional code necessary in order to render the board, as well as converting the mouse coordinates to board coordinates. See the `src/GlossUI` file for these details.