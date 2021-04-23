{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (foldl')
import Lib

main :: IO ()
main = someFunc

calcNISTax :: Integer -> Integer
calcNISTax amount = foldl' (taxCalc amount) 0 rates
  where
    taxCalc amount' acc TaxRate {..} =
      if amount' > lower
        then case higher of
          Nothing -> addRate acc rate amount'
          Just higher' ->
            if amount' <= higher'
              then addRate acc rate amount'
              else addRate acc rate higher'
        else acc
    addRate acc rate amt = acc + round (fromIntegral amt * rate)

data TaxRate = TaxRate {lower :: Integer, higher :: Maybe Integer, rate :: Double}

rates :: [TaxRate]
rates = [TaxRate 811_560 Nothing 0.5, TaxRate 0 (Just 63_360) 0.1]
