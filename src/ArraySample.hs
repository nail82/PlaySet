module ArraySample where

import Control.Monad
import System.Random
import Data.List as L
import DeckOfSet

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
