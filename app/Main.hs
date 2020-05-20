module Main where

import Data.List as L
import System.IO
import DeckOfSet
import ArraySample

main :: IO ()
main = do
  let tries = 500
      fnm = "set_montecarlo.csv"
  vals <- monteCarloSample tries
  fh <- openFile fnm WriteMode
  let csv = L.intercalate "\n" $ fmap show vals
  hPutStr fh csv
  hClose fh
  putStrLn $ "Output in => " <> fnm
