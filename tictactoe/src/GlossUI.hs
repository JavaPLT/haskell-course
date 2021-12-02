module GlossUI where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

import Board
import Position

--------------------------------------------------------------------------------
--- Game State
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
--- Rendering, Event Handling, Time Handling, Initialization and main
--------------------------------------------------------------------------------

drawGame :: Size -> GameState -> Picture
drawGame k gs = case controlState gs of
    GameOver p -> drawBoard k (case p of X -> allX; O -> allO)
    _ -> drawBoard k (curBoard $ pos gs)

gameEvent :: Size -> Event -> GameState -> GameState
gameEvent k (EventKey (MouseButton LeftButton) Down _ (x', y')) gs =
  case controlState gs of 
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
                    , controlState = HasPlayerWon
                      }
    _ -> gs
gameEvent _ _ gs = gs

gameTime :: GameState -> GameState
gameTime gs = case controlState gs of
  PlayersTurn -> gs
  HasPlayerWon -> case boardWinner . curBoard $ pos gs of
    Just p -> gs { controlState = GameOver p }
    Nothing -> gs { controlState = ComputersTurn }
  ComputersTurn -> 
    let (pos', kb') = runState (bestResponse $ pos gs) (kb gs)
    in gs { pos = pos'
          , kb = kb'
          , controlState = HasComputerWon
          }
  HasComputerWon -> case boardWinner . curBoard $ pos gs of
    Just p -> gs { controlState = GameOver p }
    Nothing -> gs { controlState = PlayersTurn }
  GameOver _ -> gs

initGameState :: GameState
initGameState =
  GameState {
      pos = Position { curBoard = initBoard, curPlayer = X
      }
    , kb = Map.empty
    , controlState = PlayersTurn
    }

main :: IO ()
main =
  let window = InWindow "Tic Tac Toe" (300, 300) (10, 10)
      size   = 100.0
  in play
        window
        white
        4
        initGameState
        (drawGame size)
        (gameEvent size)
        (\t -> gameTime)

--------------------------------------------------------------------------------
--- Rendering Details
--- copying some code from https://gist.github.com/gallais/0d61677fe97aa01a12d5
--------------------------------------------------------------------------------

type Size = Float

resize :: Size -> Path -> Path
resize k = fmap (\ (x, y) -> (x * k, y * k))

drawO :: Size -> (Int, Int) -> Picture
drawO k (i, j) =
  let x' = k * (fromIntegral j - 1)
      y' = k * (1 - fromIntegral i)
  in color (greyN 0.8) $ translate x' y' $ thickCircle (0.1 * k) (0.3 * k)

drawX :: Size -> (Int, Int) -> Picture
drawX k (i, j) =
  let x' = k * (fromIntegral j - 1)
      y' = k * (1 - fromIntegral i)
  in color black $ translate x' y' $ Pictures
     $ fmap (polygon . resize k)
     [ [ (-0.35, -0.25), (-0.25, -0.35), (0.35,0.25), (0.25, 0.35) ]
     , [ (0.35, -0.25), (0.25, -0.35), (-0.35,0.25), (-0.25, 0.35) ]
     ]

drawBoard :: Size -> Board -> Picture
drawBoard k b = Pictures $ grid : markPics where

  markPics = [drawAt (i, j) (getMark b (i, j)) | i <- [0..2], j <- [0..2]]

  drawAt :: (Int, Int) -> Maybe Player -> Picture
  drawAt (_, _) Nothing = Blank
  drawAt (i, j) (Just X) = drawX k (i, j)
  drawAt (i, j) (Just O) = drawO k (i, j)

  grid :: Picture
  grid = color black $ Pictures $ fmap (line . resize k)
       [ [(-1.5, -0.5), (1.5 , -0.5)]
       , [(-1.5, 0.5) , (1.5 , 0.5)]
       , [(-0.5, -1.5), (-0.5, 1.5)]
       , [(0.5 , -1.5), (0.5 , 1.5)]
       ]

--------------------------------------------------------------------------------
--- Converting from mouse coordinates to board coordinates
--------------------------------------------------------------------------------

checkCoordinateY :: Size -> Float -> Maybe Int
checkCoordinateY k f' =
  let f = f' / k
  in  2    <$ guard (-1.5 < f && f < -0.5)
  <|> 1    <$ guard (-0.5 < f && f < 0.5)
  <|> 0    <$ guard (0.5  < f && f < 1.5)

checkCoordinateX :: Size -> Float -> Maybe Int
checkCoordinateX k f' =
  let f = f' / k
  in  0    <$ guard (-1.5 < f && f < -0.5)
  <|> 1    <$ guard (-0.5 < f && f < 0.5)
  <|> 2    <$ guard (0.5  < f && f < 1.5)

getCoordinates :: Size -> (Float, Float) -> Maybe (Int, Int)
getCoordinates k (x, y) =
  (,) <$> checkCoordinateY k y <*> checkCoordinateX k x