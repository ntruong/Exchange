--------------------------------------------------------------------------------
module Exchange.Core
  ( Exchange
  , Books
  , Traders
  , Trader(..)
  , Message(..)
  , update
  ) where

import Exchange.Book
import Exchange.Order
import Exchange.Messages
import Exchange.Trader
import Data.Map.Strict   (Map, adjust)
--------------------------------------------------------------------------------


{- TODO(ntruong):
 -   market orders
 -   limit orders
 -   non-partial orders
 -   implement notifications/effectual
 -   better error checking for funds and whatnot
 #   order cancellation
 -}


-- | Type aliases to make life easier.
type Exchange = (Books, Traders)
type Books    = Map String Book
type Traders  = Map String Trader


-- | Update; handle orders and cancellations and such.
update :: Message -> Exchange -> Exchange
update msg (books, traders) = (books', traders')
  where
    handleBooks   :: (String, Book -> Book)
    handleTraders :: [(String, Trader -> Trader)]
    (handleBooks, handleTraders) = case msg of
      CANCEL msginfo -> (cancel msginfo, [])
      _            -> (("", id), [])
    adj :: (Ord k) => (k, a -> a) -> Map k a -> Map k a
    adj = uncurry $ flip adjust
    books' = adj handleBooks books
    traders' = foldr adj traders handleTraders

cancel :: OrderInfo -> (String, Book -> Book)
cancel msginfo = (ticker msginfo, handleBook)
  where
    handleBook :: Book -> Book
    handleBook (Book bids asks) = Book (keep bids) (keep asks)
    keep :: [Order] -> [Order]
    keep = filter (\order -> (info order) /= msginfo)
