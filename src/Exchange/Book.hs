module Exchange.Book
  ( Book(..)
  ) where

import Exchange.Order (Order)

-- | Standard representation of a list of bids and asks
data Book = Book {
  bids :: [Order]
, asks :: [Order]
} deriving (Show)
