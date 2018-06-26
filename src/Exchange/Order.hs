module Exchange.Order
  ( Order(..)
  , OrderData(..)
  , OrderMetadata(..)
  , OrderType(..)
  , OrderStyle(..)
  , OrderDirection(..)
  ) where

data OrderType = LIMIT | MARKET deriving (Show)
data OrderStyle = PARTIAL
                | AON
                | FOK
                deriving (Show)
data OrderDirection = BID   | ASK    deriving (Show)

-- | Order information
data OrderData = OrderData {
  otype  :: OrderType
, ostyle :: OrderStyle
, odir   :: OrderDirection
}

-- | Should appear as "[type/style/direction]"
instance Show OrderData where
  show (OrderData t s d) = concat ["[", show t, "/", show s, "/", show d, "]"]

-- | Order meta-information
data OrderMetadata = OrderMetadata {
  oid    :: String
, tid    :: String
, ticker :: String
}

-- | Should appear as "[orderid/traderid]ticker"
instance Show OrderMetadata where
  show (OrderMetadata o t tick) = concat ["[", o, "/", t, "]", tick]

data Order = Order {
  quantity :: Int
, price    :: Float
, odata    :: OrderData
, ometa    :: OrderMetadata
}

instance Show Order where
  show (Order q p d md) = concat [show md, " :: ", show q, " @ ", show p]
