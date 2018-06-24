module Exchange.Book
  ( OrderData(..)
  , Order(..)
  , Book(..)
  ) where

-- | Order information
data OrderData = OrderData {
  oid    :: String
, tid    :: String
, ticker :: String
}

-- | Should appear as "[orderid/traderid]ticker"
instance Show OrderData where
  show (OrderData o t tick) = concat ["[", o, "/", t, "]", tick]

data Order = Order {
  quantity :: Int
, price    :: Float
, metadata :: OrderData
}

instance Show Order where
  show (Order q p odata) = concat [show odata, " :: ", show q, " @ ", show p]

-- | Standard representation of a list of bids and asks
data Book = Book {
  bids :: [Order]
, asks :: [Order]
} deriving (Show)
