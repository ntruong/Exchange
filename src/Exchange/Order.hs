--------------------------------------------------------------------------------
module Exchange.Order
  ( Order(..)
  , Direction(..)
  , Metadata(..)
  ) where
--------------------------------------------------------------------------------

-- | The direction of an order.
data Direction = Bid
               | Ask
               deriving (Show)

-- | Order metadata, condensed into a single ADT. This represents the:
-- oid    (order ID)
-- tid    (trader ID)
-- ticker (security ID)
data Metadata = Metadata
  { oid    :: String
  , tid    :: String
  , ticker :: String
  } deriving (Eq)

-- | Should appear as "[orderid/traderid]ticker".
instance Show Metadata where
  show (Metadata o t tick) = concat ["[", o, "/", t, "]", tick]

-- | Every order ships with:
-- quantity   (amount of security to transact)
-- metadata   (metadata about who/what placed the order)
-- style      (how the order should be filled)
data Order = Order
  { quantity :: Int
  , price    :: Float
  , metadata :: Metadata
  } deriving (Show)

instance Eq Order where
  x == y = price x == price y

instance Ord Order where
  x <= y = price x <= price y
