module Exchange.Messages
 ( Message(..)
 , OrderDirection(..)
 , OrderType(..)
 ) where

import Exchange.Book (Order, OrderData)

data OrderDirection = BID | ASK
data OrderType = LIMIT | MARKET

data Message = ORDER  { order :: Order, otype :: OrderType, direction :: OrderDirection }
             | CANCEL { odata :: OrderData }
