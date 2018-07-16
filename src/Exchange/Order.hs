--------------------------------------------------------------------------------
module Exchange.Order
  ( Order(..)
  , OrderStyle(..)
  , OrderDirection(..)
  , OrderContract(..)
  , OrderInfo(..)
  ) where
--------------------------------------------------------------------------------

-- | Specifies how the order should be fulfilled.
data OrderStyle = PARTIAL
                deriving (Show)

-- | The direction of an order.
data OrderDirection = BID | ASK deriving (Show)

-- | Order contract types; e.g. limit or market.
data OrderContract = MARKET
                   | LIMIT { price :: Float }
                   deriving (Eq, Ord, Show)

-- | Order metadata, condensed into a single ADT. This represents the:
-- oid    (order ID)
-- tid    (trader ID)
-- ticker (security ID)
data OrderInfo = OrderInfo {
  oid    :: String
, tid    :: String
, ticker :: String
} deriving (Eq)

-- | Should appear as "[orderid/traderid]ticker".
instance Show OrderInfo where
  show (OrderInfo o t tick) = concat ["[", o, "/", t, "]", tick]

-- | Every order ships with:
-- quantity   (amount of security to transact)
-- info       (metadata about who/what placed the order)
-- style      (how the order should be filled)
-- contract   (the type of order and required info)
data Order = Order {
  quantity :: Int
, style    :: OrderStyle
, dir      :: OrderDirection
, contract :: OrderContract
, info     :: OrderInfo
} deriving (Show)
