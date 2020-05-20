module ArraySample where

import Control.Monad(forM)
import System.Random
import Data.List as L
import DeckOfSet(Hand, deck, handHasASet)

-- Sample one item from a list
sampleList :: [a] -> IO a
sampleList xs = do
  idx <- randomRIO (0, length xs - 1)
  return $ xs !! idx


-- Sample without replacement from a list
sampleNList :: (Eq a) => [a] -> Int -> IO [a]
sampleNList _ 0 = return []
sampleNList xs n = do
  val <- sampleList xs
  let n' = min n $ length xs - 1
      xs' = L.delete val xs
  (:) <$> (pure val) <*> sampleNList xs' (n' - 1)


-- My first use of forM
sampleOfHands :: Int -> IO [Hand]
sampleOfHands n = do
  let xs = take n $ repeat 12
      sf = sampleNList deck
  forM xs sf


handIndicator :: Hand -> Int
handIndicator h =
    case handHasASet h of
      Nothing -> 0
      Just _ -> 1

computeRatio :: [Hand] -> Double
computeRatio hands =
    let num = sum $ fmap handIndicator hands
        n = length hands
    in (fromIntegral num) / (fromIntegral n)

monteCarloSample :: Int -> IO [Double]
monteCarloSample tries = do
  let ns = take tries $ repeat 1000
  mcSamples <- forM ns sampleOfHands
  return $ computeRatio <$> mcSamples
