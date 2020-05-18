module Main where

import DeckOfSet
import ArraySample

handIndicator :: Hand -> Int
handIndicator h =
    case handHasASet h of
      Nothing -> 0
      Just _ -> 1

main :: IO ()
main = do
  let n = 1000
  hands <- sampleOfHands 1000
  let num = sum $ fmap handIndicator hands
      ratio = (fromIntegral num) / n
  putStrLn $ "Probability of a set in a 12 card hand = " <> show (ratio :: Double)
