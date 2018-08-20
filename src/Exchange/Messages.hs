--------------------------------------------------------------------------------
module Exchange.Messages
  ( Request(..)
  , Response(..)
  , ErrorCode(..)
  ) where

import Exchange.Order  (Direction, Metadata, Order)
import Exchange.Trader
--------------------------------------------------------------------------------


-- | Messages the exchange is expected to receive and handle. The messages are
-- defined as follows:
--   Limit: limit orders for a given security
--   Market: market orders for a given security
--   RegisterS: register a security (i.e. create an orderbook if applicable)
--   RegisterT: register a trader
--   Status: get the current status of the exchange
--   Cancel: cancel an order that matches the given metadata
data Request = Limit { order :: Order
                     , dir   :: Direction
                     }

             | Market { quantity :: Int
                      , dir      :: Direction
                      , metadata :: Metadata
                      }

             | RegisterS { ticker :: String }

             | RegisterT { trader :: String }

             | Status

             | Cancel { metadata :: Metadata }
             deriving (Show)


-- | Error codes for responses.
data ErrorCode = Ok
               | Error
               deriving (Eq, Ord, Show)


-- | Messages the exchange is expected to respond with.
data Response a = Response ErrorCode a

instance (Show a) => Show (Response a) where
  show (Response ec a) = concat [show ec, ": ", show a]

instance Eq (Response a) where
  (Response ec _) == (Response ec' _) = ec == ec'

instance Ord (Response a) where
  (Response ec _) <= (Response ec' _) = ec <= ec'

instance Functor Response where
  fmap f (Response ec a) = Response ec (f a)

instance Applicative Response where
  pure = Response Ok
  (Response ec f) <*> (Response ec' a) = Response (max ec ec') (f a)

instance Monad Response where
  (Response ec a) >>= f =
    let Response ec' b = f a
    in  Response (max ec ec') b

