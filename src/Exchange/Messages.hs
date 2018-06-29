--------------------------------------------------------------------------------
module Exchange.Messages
  ( Message(..)
  ) where

import Exchange.Order (Order, OrderInfo)
--------------------------------------------------------------------------------

-- | Messages the exchange is expected to receive and handle.
data Message = ORDER  { order :: Order }
             | CANCEL { msginfo :: OrderInfo }
