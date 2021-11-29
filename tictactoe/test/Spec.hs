import Control.Exception (evaluate)
import Data.Maybe
import Test.Hspec
import Control.Monad.State
import qualified Data.Map as Map


import Board
import Position

boardSafety :: Spec
boardSafety = do
    describe "testing safety of getMark" $ do
        it "works for legitimate positions" $ do
            getMark initBoard (1, 2) `shouldBe` Nothing
            getMark initBoard (0, 1) `shouldBe` Nothing
        it "throws an error for illegitimate positions" $ do
            evaluate (getMark initBoard (0, 3)) `shouldThrow` anyErrorCall
            evaluate (getMark initBoard (3, 0)) `shouldThrow` anyErrorCall
            evaluate (getMark initBoard (-1, 0)) `shouldThrow` anyErrorCall
    describe "testing safety of putMark" $ do
        it "works for empty squares" $ do
            putMark initBoard X (0, 0) `shouldSatisfy` isJust
            putMark initBoard O (1, 0) `shouldSatisfy` isJust
        it "returns Nothing for filled squares" $ do
            putMark allX X (0, 0) `shouldBe` Nothing
            putMark allO X (1, 1) `shouldBe` Nothing
            let b1 = fromJust $ putMark initBoard X (0, 0)
            putMark b1 X (0, 0) `shouldBe` Nothing
        it "throws an error for illegitimate coordinates" $ do
            evaluate (putMark initBoard X (0, 3)) `shouldThrow` anyErrorCall
            evaluate (putMark initBoard O (-1, 0)) `shouldThrow` anyErrorCall

oBoard1 :: Board
oBoard1 = fromJust $ do
    b1 <- putMark initBoard O (0, 0)
    b2 <- putMark b1 X (0, 2)
    b3 <- putMark b2 O (1, 1)
    b4 <- putMark b3 X (1, 0)
    b5 <- putMark b4 O (2, 2)
    putMark b5 X (2, 0)

xBoard1 :: Board
xBoard1 = fromJust $ do
    b1 <- putMark initBoard X (0, 0)
    b2 <- putMark b1 O (0, 2)
    b3 <- putMark b2 X (1, 1)
    b4 <- putMark b3 O (1, 0)
    b5 <- putMark b4 X (2, 2)
    putMark b5 O (2, 0)

-- x has not yet won, but he has a winning strategy from here
xBoard2 :: Board
xBoard2 = fromJust $ do
    b1 <- putMark initBoard X (1, 1)
    b2 <- putMark b1 O (2, 1)
    b3 <- putMark b2 X (0, 0)
    b4 <- putMark b3 O (2, 2)
    putMark b4 X (2, 0)

drawBoard :: Board
drawBoard = fromJust $ do
    b1 <- putMark initBoard O (0, 1)
    b2 <- putMark b1 X (0, 2)
    b3 <- putMark b2 O (1, 2)
    b4 <- putMark b3 X (1, 1)
    putMark b4 O (2, 0)

twoVerticalX :: Board
twoVerticalX = fromJust $ do
    b1 <- putMark initBoard X (0, 0)
    putMark b1 X (1, 0)

twoDiagonalO :: Board
twoDiagonalO = fromJust $ do
    b1 <- putMark initBoard O (0, 0)
    b2 <- putMark b1 O (1, 1)
    putMark b2 X (0, 2)




simpleAnalysisTests :: Spec
simpleAnalysisTests = do
    describe "testing lineWinner" $
        it "works for some simple cases" $ do
            lineWinner initBoard (winningLines !! 0) `shouldBe` Nothing
            lineWinner initBoard (winningLines !! 3) `shouldBe` Nothing
            lineWinner allX (winningLines !! 2) `shouldBe` Just X
            lineWinner allO (winningLines !! 5) `shouldBe` Just O 
    describe "testing boardWinner" $ do
        it "works for some simple cases" $ do
            boardWinner initBoard `shouldBe` Nothing
            boardWinner allX `shouldBe` Just X
            boardWinner allO `shouldBe` Just O
        it "works on some slightly more complex cases" $ do
            boardWinner xBoard1 `shouldBe` Just X
            boardWinner oBoard1 `shouldBe` Just O
            boardWinner xBoard2 `shouldBe` Nothing
    describe "testing succPositions" $ do
        it "the empty board has 9 successors" $ do
            length (succPositions $ Position initBoard X) `shouldBe` 9
            length (succPositions $ Position initBoard O) `shouldBe` 9
        it "the full board has no successors" $ do
           (succPositions $ Position allX X) `shouldBe` []
           (succPositions $ Position allO O) `shouldBe` []
    describe "testing isWinning/isLosing" $ do
        it "on some simple cases" $ do
            isWinning (Position initBoard X) `shouldBe` False
            isWinning (Position initBoard O) `shouldBe` False
            isLosing (Position initBoard X) `shouldBe` False
            isLosing (Position initBoard O) `shouldBe` False
            isWinning (Position allX X) `shouldBe` True
            isWinning (Position allO O) `shouldBe` True
            isLosing (Position allX X) `shouldBe` False
            isLosing (Position allO O) `shouldBe` False
        it "on some slightly more complex cases" $ do
            isWinning (Position xBoard1 X) `shouldBe` True
            isWinning (Position oBoard1 O) `shouldBe` True
            isWinning (Position xBoard2 X) `shouldBe` True
            isLosing (Position xBoard2 O) `shouldBe` True
            isWinning (Position drawBoard X) `shouldBe` False
            isWinning (Position drawBoard O) `shouldBe` False

statefulAnalysisTests :: Spec
statefulAnalysisTests = do
    describe "testing scorePosition" $ do
        it "on some terminal Positions" $ do
            evalState (scorePosition (Position allX X)) (Map.empty) `shouldBe` (Score Win 0)
            evalState (scorePosition (Position allX O)) (Map.empty) `shouldBe` (Score Lose 0)
            evalState (scorePosition (Position allO O)) (Map.empty) `shouldBe` (Score Win 0)
        it "on a draw board" $ do
            evalState (scorePosition (Position drawBoard X)) (Map.empty) `shouldBe` (Score Draw 4)
        it "on the fork board" $ do
            evalState (scorePosition (Position xBoard2 X)) (Map.empty) `shouldBe` (Score Win 1)
            evalState (scorePosition (Position xBoard2 O)) (Map.empty) `shouldBe` (Score Lose 2)
    describe "testing bestResponse" $ do
        it "blocks partial lines successfully" $ do
            let twoVerticalXBlock = fromJust $ putMark twoVerticalX O (2, 0)
            evalState (bestResponse (Position twoVerticalX O)) Map.empty `shouldBe` (Position twoVerticalXBlock X)
            let twoDiagonalOBlock = fromJust $ putMark twoDiagonalO X (2, 2)
            evalState (bestResponse (Position twoDiagonalO X)) Map.empty `shouldBe` (Position twoDiagonalOBlock O)
        it "completes partial lines successfully" $ do
            let twoVerticalXComplete = fromJust $ putMark twoVerticalX X (2, 0)
            evalState (bestResponse (Position twoVerticalX X)) Map.empty `shouldBe` (Position twoVerticalXComplete O)
            let twoDiagonalOComplete = fromJust $ putMark twoDiagonalO O (2, 2)
            evalState (bestResponse (Position twoDiagonalO O)) Map.empty `shouldBe` (Position twoDiagonalOComplete X)

main :: IO ()
main = hspec $ do
    boardSafety
    simpleAnalysisTests
    statefulAnalysisTests
