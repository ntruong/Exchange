--------------------------------------------------------------------------------
module Exchange.Messages
  ( Message(..)
  ) where

import Exchange.Order (Direction, Metadata, Order)
--------------------------------------------------------------------------------

-- | Messages the exchange is expected to receive and handle.
data Message = Limit { order :: Order
                     , dir   :: Direction
                     }

             | Market { quantity :: Int
                      , dir      :: Direction
                      , metadata :: Metadata
                      }

             | Cancel { metadata :: Metadata }
