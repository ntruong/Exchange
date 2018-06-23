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

-- | Standard representation of a list of asks and bids
data Book = Book {
  asks :: [Order]
, bids :: [Order]
}
