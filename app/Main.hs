{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (foldl')
import System.Environment (getArgs)
import Text.Read (readEither)

main :: IO ()
main = do
  (amt : _) <- getArgs
  let amt'' = readEither amt
  putStrLn $ case amt'' of
    Right amt' -> show $ calcNISTax amt'
    Left err -> "failed to parse \"" ++ amt ++ "\": " ++ err

calcNISTax :: Integer -> Integer
calcNISTax amount = foldl' (taxCalc amount) 0 rates
  where
    taxCalc amount' acc TaxRate {..} =
      if amount' > lower
        then addRate $
          case higher of
            Nothing -> amount'
            Just higher' ->
              if amount' <= higher'
                then amount' - lower
                else higher'
        else acc
      where
        addRate amt = acc + round (fromIntegral amt * rate)

data TaxRate = TaxRate {lower :: Integer, higher :: Maybe Integer, rate :: Double}

rates :: [TaxRate]
rates =
  [ TaxRate 811_560 Nothing 0.5,
    TaxRate 501_960 (Just 811_560) 0.48,
    TaxRate 240_000 (Just 501_960) 0.34,
    TaxRate 168_000 (Just 240_000) 0.31,
    TaxRate 108_120 (Just 168_000) 0.21,
    TaxRate 63_360 (Just 108_120) 0.14,
    TaxRate 0 (Just 63_360) 0.1
  ]
