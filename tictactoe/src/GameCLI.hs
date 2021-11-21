module GameCLI where

import Control.Monad.State
import Control.Concurrent
import qualified Data.Map as Map

import Board
import Position

getCoordinates :: IO (Int, Int)
getCoordinates = do
  putStrLn "Enter coordinates (row, column):"
  row <- getLine
  column <- getLine
  pure (read row, read column)

getPlayerMove :: Position -> IO Position
getPlayerMove pos@(Position board player) = do
  move <- getCoordinates
  case putMark board player move of
      Nothing -> do
          putStrLn "Please Try Again"
          getPlayerMove pos
      Just newBoard -> pure (Position newBoard (nextPlayer player))

gameLoop :: Position -> KnowledgeBase -> IO ()
gameLoop pos@(Position board player) kb = do
    case boardWinner board of
        Just X -> putStrLn "X Wins!"
        Just O -> putStrLn "O Wins!"
        Nothing -> do
            nextPos <- getPlayerMove pos
            threadDelay 1000000
            let (nextPos', newKB) = runState (bestResponse nextPos) kb
            putStrLn "Computer's move:"
            print $ curBoard nextPos'
            gameLoop nextPos' newKB



main :: IO ()
main = do
    putStrLn "Welcome to the game of Tic Tac Toe!"
    putStrLn "Enter your name: "
    name <- getLine
    putStrLn $ "Hello " ++ name ++ ", let's play!"
    print initBoard
    gameLoop (Position initBoard X) Map.empty