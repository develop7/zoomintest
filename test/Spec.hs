{-# LANGUAGE NumericUnderscores #-}

import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "calcNISTax" $ do
    it "calculates first-tier-only amounts" $ do
      calcNISTax 1000 `shouldBe` 100
      calcNISTax 63_359 `shouldBe` 6_336

    it "calculates multiple-tier amounts" $
      calcNISTax (63_360 + 10_000) `shouldBe` (6_336 + 1_400)

    it "handles incorrect amounts correctly" $ do
      calcNISTax 0 `shouldBe` 0
      calcNISTax (-232) `shouldBe` 0
