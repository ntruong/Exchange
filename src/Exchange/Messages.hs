module Exchange.Messages
 ( Message(..)
 ) where

import Exchange.Order (Order, OrderMetadata)

data Message = ORDER  { order :: Order }
             | CANCEL { ometa :: OrderMetadata }
