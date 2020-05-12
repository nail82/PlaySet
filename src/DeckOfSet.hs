{-# LANGUAGE InstanceSigs #-}
module DeckOfSet where

import qualified Data.List as L

data Value = Uno
           | Dos
           | Tres
             deriving (Eq, Ord, Enum, Show)

data Color = Green
           | Purple
           | Red
             deriving (Eq, Ord, Enum, Show)

data Shade = Blank
           | Hash
           | Solid
             deriving (Eq, Ord, Enum, Show)

data Shape = Angles
           | Pill
           | Worm
             deriving (Eq, Ord, Enum, Show)

type Card = (Value, Color, Shade, Shape)

mateFeature :: (Enum a, Eq a) => a -> a -> Maybe a
mateFeature v1 v2 =
    if v1 == v2 then (Just v1) else
        case vs of
          1 -> Just $ toEnum 2
          2 -> Just $ toEnum 1
          3 -> Just $ toEnum 0
          _ -> Nothing
        where vs = (fromEnum v1) + (fromEnum v2)

mateCard :: Card -> Card -> Maybe Card
mateCard (v1, c1, d1, p1) (v2, c2, d2, p2) = do
  v3 <- mateFeature v1 v2
  c3 <- mateFeature c1 c2
  d3 <- mateFeature d1 d2
  p3 <- mateFeature p1 p2
  return (v3, c3, d3, p3)



hand :: [Card]
hand = [deck !! 0, deck !! 1, deck !! 2
       , deck !! 3, deck !! 4, deck !! 5
       , deck !! 6, deck !! 7, deck !! 8
       , deck !! 9, deck !! 10, deck !! 11]

-- Get all pairs in a hand.  A hand of 12 cards has 66 pairs.
allPairs :: [Card] -> [(Card, Card)]
allPairs cards =
    let cs = L.nub $ L.sort [swapCards (c1, c2) | c1 <- cards, c2 <- cards]
    in filter (\(c1, c2) -> c1 /= c2) cs

-- Order a tuple of cards
swapCards :: (Card, Card) -> (Card, Card)
swapCards (c1, c2)
          | c2 < c1 = (c2, c1)
          | otherwise = (c1, c2)

mateInHand' :: [Card] -> (Card, Card) -> Maybe (Card, Card, Card)
mateInHand' h (c1, c2) =
    case (mateCard c1 c2) of
      Nothing -> Nothing
      Just c3 -> if c3 `elem` h then (Just (c1, c2, c3)) else Nothing

handHasASet :: [Card] -> Maybe (Card, Card, Card)
handHasASet h =
    let pairs = allPairs h
        mih = mateInHand' h
        cs = fmap mih pairs
    in case cs of
         [] -> Nothing
         (c:_) -> c


-- Lists of cards
troublesomeHand :: [Card]
troublesomeHand = [(Dos, Red, Blank, Pill), (Tres, Purple, Blank, Pill)
                  , (Tres, Purple, Hash, Pill), (Dos, Green, Blank, Pill)
                  , (Dos, Red, Blank, Worm), (Uno, Purple, Solid, Pill)
                  , (Tres, Green, Solid, Worm), (Dos, Purple, Solid, Pill)
                  , (Dos, Red, Solid, Pill), (Dos, Purple, Hash, Angles)
                  , (Tres, Red, Blank, Worm), (Tres, Green, Hash, Worm)]

deck :: [Card]
deck = [ (v, c, d, p) |
         v <- [Uno, Dos, Tres],
         c <- [Green, Purple, Red],
         d <- [Blank, Hash, Solid],
         p <- [Angles, Pill, Worm]]
