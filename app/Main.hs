module Main where

import DeckOfSet

main :: IO ()
main = do
  case handHasASet hand of
    Nothing -> putStrLn "No set"
    Just s -> putStrLn $ show s
  case handHasASet troublesomeHand of
    Nothing -> putStrLn "Trouble has no set"
    _ -> putStrLn "Trouble shouldn't have a set!"
