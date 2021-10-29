import Lecture0

import Test.Hspec

main :: IO ()
main = hspec $ do
    gcdTests

gcdTests = 
    describe "Testing gcdN" $ do
        it "passes some simple tests" $ do
            gcdN [10] `shouldBe` 10
            gcdN [1, 10] `shouldBe` 1
            gcdN [10, 20, 30] `shouldBe` 10
            gcdN [49, 63, 70] `shouldBe` 7
