module Exchange.Core
  ( Exchange
  , update
  ) where

import Exchange.Book
import Exchange.Messages

import Data.Map.Strict (Map, adjust)

{- TODO(ntruong):
 -   market orders
 -   limit orders
 -   implement notifications/effectual
 -   better error checking for funds and whatnot
 #   order cancellation
 -}

type Exchange = Map String Book

-- | Big update; handles orders and cancellations and such
update :: Message -> Exchange -> Exchange
update msg = adjust (f msg) tick
  where
    f    :: Message -> Book -> Book
    tick :: String
    (f, tick) = case msg of
      CANCEL odata -> (cancel, ticker odata)
      _        -> (const id, "")

-- | Update a book according to a cancellation
cancel :: Message -> Book -> Book
cancel (CANCEL odata) = \(Book bs as) -> Book (keep bs) (keep as)
  where
    keep :: [Order] -> [Order]
    keep = filter (\(Order _ _ odata') -> (oid odata') /= (oid odata))
cancel _ = id
