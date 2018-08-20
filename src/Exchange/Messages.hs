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
data Request = Limit { trader   :: String
                     , ticker   :: String
                     , quantity :: Int
                     , price    :: Float
                     , dir      :: Direction
                     }

             | Market { trader   :: String
                      , ticker   :: String
                      , quantity :: Int
                      , dir      :: Direction
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
data Response = Response ErrorCode String

instance Show Response where
  show (Response ec msg) = concat [ "["
                                  , show ec
                                  , ": "
                                  , msg
                                  , "] "
                                  ]

instance Semigroup Response where
  (Response ec msg) <> (Response ec' msg') =
    Response (max ec ec') (msg' ++ " -> " ++ msg)

instance Monoid Response where
  mempty = Response Ok "()"
