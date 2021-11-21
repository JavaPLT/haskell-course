module Board where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Maybe

data Player = X | O
    deriving (Eq, Ord, Show)

-- we wrap the extra constructor because 
-- we want to discourage constructing boards without 
-- the functions from this module
newtype Board = Board (Map (Int, Int) (Maybe Player))
    deriving (Eq, Ord)

initBoard :: Board
initBoard = Board $ Map.fromList [((x, y), Nothing) | x <- [0..2], y <- [0..2]]

getMark :: Board -> (Int, Int) -> Maybe Player
getMark (Board board) (x, y)
    | x < 0 || x > 2 || y < 0 || y > 2 = error "Invalid coordinates"
    | otherwise = board ! (x, y)

putMark :: Board -> Player -> (Int, Int) -> Maybe Board
putMark (Board board) player (x, y)
    | x < 0 || x > 2 || y < 0 || y > 2 = error $ "Invalid coordinates" ++ show (x, y)
    | Data.Maybe.isJust (board ! (x, y)) = Nothing
    | otherwise = Just $ Board $ Map.insert (x, y) (Just player) board

emptySquares :: Board -> [(Int, Int)]
emptySquares (Board board) = [(x, y) | x <- [0..2], y <- [0..2], isNothing (board ! (x, y))]

instance Show Board where
    show (Board board) =
        intercalate "\n- - - \n"
            [ ( intercalate "|" [prettyShow $ board ! (x, y) | y <- [0..2]] )
                | x <- [0..2]]
            where
                prettyShow Nothing = " "
                prettyShow (Just X) = "X"
                prettyShow (Just O) = "O"

allX :: Board
allX = Board $ Map.fromList [((x, y), Just X) | x <- [0..2], y <- [0..2]]

allO :: Board
allO = Board $ Map.fromList [((x, y), Just O) | x <- [0..2], y <- [0..2]]