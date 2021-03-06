module GameCLI where

import Control.Monad.State
import Control.Concurrent
import qualified Data.Map as Map

import Board
import Position


data GameState = GameState {
        pos :: Position
    ,   kb :: KnowledgeBase
    ,   controlState :: ControlState
    }

data ControlState = 
      PlayersTurn 
    | HasPlayerWon
    | ComputersTurn
    | HasComputerWon
    | GameOver Player

gameLoop :: GameState -> IO ()
gameLoop gs = case controlState gs of
    GameOver p -> handleGameOver p
    PlayersTurn -> do
        pos' <- handlePlayersTurn (pos gs) 
        print $ (curBoard $ pos')
        gameLoop $ gs { pos = pos', controlState = HasPlayerWon }
    HasPlayerWon -> do
        case boardWinner (curBoard $ pos gs) of
            Just p -> gameLoop $ gs { controlState = GameOver p }
            Nothing -> gameLoop $ gs { controlState = ComputersTurn }
    ComputersTurn -> do
        let (pos', kb') = runState (bestResponse $ pos gs) (kb gs)
        putStrLn "Computer's Move:"
        print $ (curBoard $ pos')
        gameLoop $ gs { pos = pos', kb = kb' , controlState = HasComputerWon }
    HasComputerWon -> do
        case boardWinner (curBoard $ pos gs) of
            Just p -> gameLoop $ gs { controlState = GameOver p }
            Nothing -> gameLoop $ gs { controlState = PlayersTurn }


handleGameOver :: Player -> IO ()
handleGameOver p = putStrLn $ show p ++ " has won!"

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

main :: IO ()
main = do
    print initBoard
    gameLoop $ GameState (Position initBoard X) Map.empty PlayersTurn