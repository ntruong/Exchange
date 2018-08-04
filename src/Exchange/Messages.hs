--------------------------------------------------------------------------------
module Exchange.Messages
  ( Request(..)
  , Response(..)
  ) where

import Exchange.Order (Direction, Metadata, Order)
--------------------------------------------------------------------------------

-- | Messages the exchange is expected to receive and handle.
data Request = Limit { order :: Order
                     , dir   :: Direction
                     }

             | Market { quantity :: Int
                      , dir      :: Direction
                      , metadata :: Metadata
                      }

             | Status

             | Cancel { metadata :: Metadata }

-- | Messages the exchange is expected to respond with.
data Response = Ok
              | Error { errmsg :: String }
