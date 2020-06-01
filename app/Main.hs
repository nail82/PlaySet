module Main where

import Data.List as L
import System.IO
import ArraySample
import DeckOfSet

main :: IO ()
main = do
{--
  let tries = 500
      fnm = "set_montecarlo.csv"
  vals <- monteCarloSample tries
  fh <- openFile fnm WriteMode
  let csv = L.intercalate "\n" $ fmap show vals
  hPutStr fh csv
  hClose fh
  putStrLn $ "Output in => " <> fnm
--}
  let t2 = handHasASet trouble2
  case t2 of
    Nothing -> putStrLn "No set"
    Just h -> putStrLn $ "Trouble 2 " <> show h
