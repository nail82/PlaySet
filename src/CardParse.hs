module CardParse where

import Control.Applicative
import Text.Trifecta
import DeckOfSet

parseValue :: Parser Value
parseValue = do
  char 't' >> return Tres
  <|> (char 'd' >> return Dos)
  <|> (char 'u' >> return Uno)

parseColor :: Parser Color
parseColor = do
  char 'g' >> return Green
  <|> (char 'p' >> return Purple)
  <|> (char 'r' >> return Red)

parseShade :: Parser Shade
parseShade = do
  char 'b' >> return Blank
  <|> (char 'h' >> return Hash)
  <|> (char 's' >> return Solid)

parseShape :: Parser Shape
parseShape = do
  char 'a' >> return Angles
  <|> (char 'p' >> return Pill)
  <|> (char 'w' >> return Worm)

parseCard :: Parser Card
parseCard = do
  v <- parseValue
  c <- parseColor
  d <- parseShade
  p <- parseShape
  return (v,c,d,p)

parseHand :: Parser Hand
parseHand = undefined
