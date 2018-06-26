module Exchange.Order
  ( OrderData(..)
  , Order(..)
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

