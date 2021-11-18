module Position where

import Control.Applicative
import Control.Monad.State
import Data.Maybe 
import Data.Map (Map)
import Data.List (minimumBy)
import qualified Data.Map as Map

import Board 

data Position = Position { curBoard :: Board, curPlayer :: Player }
    deriving (Eq, Ord, Show)

type Line = [(Int, Int)]

winningLines :: [Line]
winningLines = [ [(x, y) | x <- [0..2]] | y <- [0..2]] ++ -- vertical lines
               [ [(x, y) | y <- [0..2]] | x <- [0..2]] ++ -- horizontal lines
               [[(0, 0), (1, 1), (2, 2)], -- main diagonal
                [(0, 2), (1, 1), (2, 0)]] -- off diagonal 

lineWinner :: Board -> Line -> Maybe Player
lineWinner b l
    | all (== Just X) marks = Just X
    | all (== Just O) marks = Just O
    | otherwise = Nothing 
    where 
       marks = map (getMark b) l 

boardWinner :: Board -> Maybe Player
boardWinner b = foldr (<|>) Nothing $ map (lineWinner b) winningLines

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

succPositions :: Position -> [Position]
succPositions (Position b p) = newPosition . fromJust . markSquare <$> (emptySquares b)
    where
        newPosition b' = Position { curBoard = b', curPlayer = nextPlayer p }
        markSquare = putMark b p

isDraw :: Board -> Bool
isDraw b = null (emptySquares b) && isNothing (boardWinner b)

data Label = Win | Lose | Draw
    deriving (Show, Eq)
data Score = Score { label :: Label, height :: Int }
    deriving (Show, Eq)

instance Ord Score where
    (Score Win i) <= (Score Win j) = i >= j
    (Score Win _) <= _ = False
    (Score Lose i) <= (Score Lose j) = i <= j
    (Score Lose _) <= _  = True
    (Score Draw i) <= (Score Draw j) = i >= j
    (Score Draw _) <= (Score Win _) = True 
    (Score Draw _) <= (Score Lose _) = False

type KnowledgeBase = Map Position Score

scorePosition :: Position -> State KnowledgeBase Score
scorePosition pos@(Position b p)
    | isDraw b = pure $ Score { label = Draw, height = 0 }
    | (boardWinner b) == Just p = pure $ Score { label = Win, height = 0 }
    | Just _ <- (boardWinner b) = pure $ Score { label = Lose, height = 0 }
scorePosition pos@(Position b p) = 
    do
        knowledge <- gets (Map.lookup pos)
        case knowledge of
            Just s -> return s
            Nothing -> do
                let nextPositions = succPositions pos
                nextScores <- mapM scorePosition nextPositions
                let bestSuccScore = minimum nextScores
                let score = curScore bestSuccScore
                modify (Map.insert pos score)
                return score

bestResponse :: Position -> State KnowledgeBase Position
bestResponse pos@(Position b p) = 
    do
        let nextPositions = succPositions pos
        nextScores <- mapM scorePosition nextPositions
        let bestSucc = snd $ minimumBy (\(s1, p1) (s2, p2) -> compare s1 s2) $ zip nextScores nextPositions
        return bestSucc

-- given the minimum score among the successors,
-- compute the current score
curScore :: Score -> Score
curScore (Score Win i) = Score Lose (i + 1)
curScore (Score Lose i) = Score Win (i + 1)
curScore (Score Draw i) = Score Draw (i + 1)

isWinning :: Position -> Bool
isWinning pos@(Position b p) =
    case (boardWinner b) of
        Just p' -> (p == p')
        Nothing -> any isWinning $ succPositions pos
