module Main where

import DeckOfSet

main :: IO ()
main = do
  case handHasASet hand of
    Nothing -> putStrLn "No set"
    Just s -> putStrLn $ show s
