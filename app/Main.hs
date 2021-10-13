module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List as L
import System.IO
import System.Exit (exitSuccess)
import Text.Trifecta
import ArraySample
import CardParse
import DeckOfSet

checkForQuit :: String -> IO ()
checkForQuit handStr = if length handStr == 0 then exitSuccess
                       else return ()

goodHand :: Hand -> IO ()
goodHand hand = do
  let s = handHasASet hand
  case s of
    Just match -> putStrLn $ "Here's one => " <> show match
    Nothing -> putStrLn "No match"

ph :: String -> Result Hand
ph = parseString parseHand mempty

checkForSet :: String -> IO ()
checkForSet handStr = do
  let parsedHand = ph handStr
  case parsedHand of
    (Success hand') -> goodHand hand'
    _ -> putStrLn "Oops, something wrong with the hand you entered."

main :: IO ()
main = forever $ do
         putStrLn "Give me a hand or an empty line to quit:"
         handStr <- getLine
         checkForQuit $ toLower <$> handStr
         checkForSet handStr


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
