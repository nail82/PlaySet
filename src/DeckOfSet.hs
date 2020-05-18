module DeckOfSet
    ( Value (..)
    , Color (..)
    , Shade (..)
    , Shape (..)
    , Deck
    , Hand
    , deck
    , mateInHand'
    , handHasASet
    , mateCard
    , hand
    , newHand
    , troublesomeHand
    ) where

import qualified Data.List as L

-- Define the features of a Set card
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
type Hand = [Card]
type Deck = [Card]

mateFeature :: (Enum a, Eq a) => a -> a -> Maybe a
mateFeature v1 v2 =
    if v1 == v2 then (Just v1) else
        case vs of
          1 -> Just $ toEnum 2
          2 -> Just $ toEnum 1
          3 -> Just $ toEnum 0
          _ -> Nothing
        where vs = (fromEnum v1) + (fromEnum v2)

-- Find the mate of two cards
mateCard :: Card -> Card -> Maybe Card
mateCard m1@(v1, c1, d1, p1) m2@(v2, c2, d2, p2) =
  if m1 == m2 then Nothing else do
      v3 <- mateFeature v1 v2
      c3 <- mateFeature c1 c2
      d3 <- mateFeature d1 d2
      p3 <- mateFeature p1 p2
      return (v3, c3, d3, p3)

-- Get all pairs in a hand.  A hand of 12 cards has 66 pairs.
allPairs :: Hand -> [(Card, Card)]
allPairs cards =
    let cs = L.nub $ L.sort [swapCards (c1, c2) | c1 <- cards, c2 <- cards]
    in filter (\(c1, c2) -> c1 /= c2) cs

-- Order a tuple of cards
swapCards :: (Card, Card) -> (Card, Card)
swapCards (c1, c2)
          | c2 < c1 = (c2, c1)
          | otherwise = (c1, c2)

-- Find a mate of two cards in a hand, if it exists.
mateInHand' :: Hand -> (Card, Card) -> Maybe (Card, Card, Card)
mateInHand' h (c1, c2) =
    case (mateCard c1 c2) of
      Nothing -> Nothing
      Just c3 -> if c3 `elem` h then (Just (c1, c2, c3)) else Nothing

-- If a hand has a Set, return the first one.
handHasASet :: Hand -> Maybe (Card, Card, Card)
handHasASet h =
    let pairs = allPairs h
        mih = mateInHand' h
        cs = dropWhile (== Nothing) $ fmap mih pairs
    in case cs of
         [] -> Nothing
         (c:_) -> c


-- A few lists of cards
troublesomeHand :: Hand
troublesomeHand = [(Dos, Red, Blank, Pill), (Tres, Purple, Blank, Pill)
                  , (Tres, Purple, Hash, Pill), (Dos, Green, Blank, Pill)
                  , (Dos, Red, Blank, Worm), (Uno, Purple, Solid, Pill)
                  , (Tres, Green, Solid, Worm), (Dos, Purple, Solid, Pill)
                  , (Dos, Red, Solid, Pill), (Dos, Purple, Hash, Angles)
                  , (Tres, Red, Blank, Worm), (Tres, Green, Hash, Worm)]

deck :: Deck
deck = [ (v, c, d, p) |
         v <- [Uno, Dos, Tres],
         c <- [Green, Purple, Red],
         d <- [Blank, Hash, Solid],
         p <- [Angles, Pill, Worm]]

hand :: Hand
hand = [deck !! 0, deck !! 1, deck !! 2
       , deck !! 3, deck !! 4, deck !! 5
       , deck !! 6, deck !! 7, deck !! 8
       , deck !! 9, deck !! 10, deck !! 11]

newHand :: Hand
newHand = (Tres,Purple,Solid,Worm) : troublesomeHand
