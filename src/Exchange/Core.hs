module Exchange.Core
  ( Exchange
  , Books
  , Traders
  , update
  ) where

import Exchange.Book
import Exchange.Messages
import Exchange.Order
import Exchange.Trader

import Data.Map.Strict (Map, adjust)

{- TODO(ntruong):
 -   market orders
 -   limit orders
 -   implement notifications/effectual
 -   better error checking for funds and whatnot
 #   order cancellation
 -}

type Books = Map String Book
type Traders = Map String Trader
type Exchange = (Books, Traders)

-- | Big update; handles orders and cancellations and such
update :: Message -> Exchange -> Exchange
update msg (books, traders) = (adjust (f msg) tick books, traders)
  where
    f    :: Message -> Book -> Book
    tick :: String
    (f, tick) = case msg of
      CANCEL ometa -> (cancel, ticker ometa)
      _        -> (const id, "")

-- | Update a book according to a cancellation
cancel :: Message -> Book -> Book
cancel (CANCEL ometa) = \(Book bs as) -> Book (keep bs) (keep as)
  where
    keep :: [Order] -> [Order]
    keep = filter (\(Order _ _ _ ometa') ->
      ((oid ometa') /= (oid ometa)) || ((tid ometa') /= (tid ometa)))
cancel _ = id
