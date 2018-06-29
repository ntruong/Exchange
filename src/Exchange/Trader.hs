--------------------------------------------------------------------------------
module Exchange.Trader
  ( Trader(..)
  ) where
--------------------------------------------------------------------------------

-- | Trader information.
data Trader = Trader {
  funds :: Float
} deriving (Show)
