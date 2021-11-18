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

-- copying some code from https://gist.github.com/gallais/0d61677fe97aa01a12d5

data GameState = GameState {
      pos :: Position
    , kb :: KnowledgeBase
    , playersTurn :: Bool
    , needToEval :: Bool
    }
    deriving Show

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

  drawAt :: (Int, Int) -> (Maybe Player) -> Picture
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

gameUpdate' :: Size -> Event -> GameState -> GameState
gameUpdate' _ e gs
  | playersTurn gs == False || needToEval gs = gs
gameUpdate' k (EventKey (MouseButton LeftButton) Down _ (x', y')) gs =
    let newBoard = do 
            (i, j) <- getCoordinates k (x', y')
            putMark (curBoard $ pos gs) (curPlayer $ pos gs) (i, j)
    in case newBoard of
        Nothing -> gs
        Just b' -> gs { pos = Position { 
                              curBoard = b'
                            , curPlayer = nextPlayer (curPlayer $ pos gs) 
                            }
                      , playersTurn = False
                      , needToEval = True
                      }
gameUpdate' _ _ gs = gs

gameTime :: Float -> GameState -> GameState
-- let the player move
gameTime _ gs
  | playersTurn gs && not (needToEval gs) = gs
-- check if player has won
gameTime t gs
  | (needToEval gs) =
      case (boardWinner $ curBoard $ pos gs) of
        Just X -> gs { pos = (pos gs) { curBoard = allX } }
        Just O -> gs { pos = (pos gs) { curBoard = allO } }
        Nothing -> gs { needToEval = False }
-- make computers move
gameTime _ gs =
    let (pos', kb') = runState (bestResponse $ pos gs) (kb gs)
    in GameState {pos = pos', kb = kb', playersTurn = True, needToEval = True}

initGameState :: GameState
initGameState = 
  GameState {
      pos = Position {
        curBoard = initBoard
      , curPlayer = X
      }
    , kb = Map.empty
    , playersTurn = True
    , needToEval = False
    }

main :: IO ()
main =
  let window = InWindow "Tic Tac Toe" (300, 300) (10, 10)
      size   = 100.0
  in play 
        window 
        white 
        1 
        initGameState
        (\ gs -> drawBoard size $ curBoard $ pos gs) 
        (gameUpdate' size) 
        gameTime