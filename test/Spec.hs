{-# LANGUAGE NumericUnderscores #-}

import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "calcNISTax" $ do
    it "calculates first-tier-only amounts" $ do
      calcNISTax 1000 `shouldBe` 100
      calcNISTax 63_359 `shouldBe` 6_336

    it "calculates two-tier amounts" $
      calcNISTax (63_360 + 10_000) `shouldBe` (6_336 + 1_400)

    it "calculates multiple-tiers amounts" $
      calcNISTax (108_120 + 10_000) `shouldBe` (6_336 + 6_266 + 2_100)

    it "handles incorrect amounts" $ do
      calcNISTax 0 `shouldBe` 0
      calcNISTax (-232) `shouldBe` 0
