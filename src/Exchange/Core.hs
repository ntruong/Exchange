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
import Data.List           (inits, partition, sortOn)
import Data.Map.Strict     (Map, adjust, insert, (!))
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

update (ORDER msgorder) (books, traders) = (books', traders')
  where
    msgticker = (ticker . info) msgorder
    Book bids asks lastP = books ! msgticker

    -- Get the modified orders
    remaining :: Maybe Order
    filled :: [Order]
    unfilled :: [Order]
    (remaining, filled, unfilled) = case dir msgorder of
      BID -> handleOrder msgorder asks
      ASK -> handleOrder msgorder bids

    -- Get the modified book
    newBook :: Book
    newBook = case dir msgorder of
      BID -> case remaining of
        Just x -> Book (x:bids) unfilled lastP'
        Nothing -> Book bids unfilled lastP'
      ASK -> case remaining of
        Just x -> Book unfilled (x:asks) lastP'
        Nothing -> Book unfilled asks lastP'
      where
        lastP' = case (contract . last) filled of
          MARKET  -> lastP
          LIMIT p -> Just p

    -- Generate the trades
    handleTraders :: [(String, Trader -> Trader)]
    handleTraders = concat $ generateTrades <$> filled
      where
        generateTrades :: Order -> [(String, Trader -> Trader)]
        generateTrades (Order amt _ _ con inf) = [trade, trade']
          where
            p = case con of
              MARKET -> case lastP of
                Just p' -> p'
                Nothing -> 0 -- If there are no prior trades, set it at 0
            cash = case dir msgorder of
              BID -> -p * (fromIntegral amt)
              ASK -> p * (fromIntegral amt)
            trade  = ((tid . info) msgorder, \t -> t { funds = funds t + cash })
            trade' = (tid inf, \t -> t { funds = funds t - cash })

    -- Update the maps
    books' = adjust (const newBook) msgticker books
    traders' = foldr (\(k, v) m -> adjust v k m) traders handleTraders

-- | Filter out orders that match the given metadata
update (CANCEL msginfo) (books, traders) = (books', traders)
  where
    keep :: [Order] -> [Order]
    keep = filter ((msginfo /=) . info)
    handleBook :: Book -> Book
    handleBook (Book bids asks lastP) = Book (keep bids) (keep asks) lastP
    books' = adjust handleBook (ticker msginfo) books

-- | Handle logic of an order reducing a given book
handleOrder :: Order -> [Order] -> (Maybe Order, [Order], [Order])
handleOrder order orders = (order', filled, unfilled)
  where
    -- Get an accumulating list of filled orders/quantities/prices
    sorted = (theOrder . sortOn contract) orders
      where
        theOrder = case dir order of
          BID -> id
          ASK -> reverse
    cumulative :: [(Order, Int)]
    cumulative = zip sorted (tail . scanl (+) 0 $ quantity <$> sorted)

    -- Split the list based on full fills and potential fills
    goodQuant :: (Order, Int) -> Bool
    goodQuant = (<= quantity order) . snd
    goodPrice :: (Order, Int) -> Bool
    goodPrice (oppOrd, _) = case contract order of
      -- Automatically keep all opposing order prices if this is a market
      MARKET  -> True
      LIMIT p -> case contract oppOrd of
        -- Automatically keep opposing market orders
        MARKET  -> True
        -- Only keep opposing limit orders cheaper than this one
        LIMIT p' -> p >= p'
    conditions =
      [ goodQuant
      , goodPrice
      ]

    -- Parse out orders from the cumulative list
    (lFillable, lUnfillable) =
      partition (\x -> (any id) $ ($ x) <$> conditions) cumulative
    fillable   = fst <$> lFillable
    unfillable = fst <$> lUnfillable

    -- Do math
    (order', filled, unfilled) =
      -- See if we have any remainder quantity to be filled
      case quantity order - (snd . last) lFillable of
        -- Lucky! It was a perfect fill
        0 -> (Nothing, fillable, unfillable)
        -- We have to handle partial filling of opposing orders
        remaining -> case unfillable of
          -- We have no opposing orders; leave the order on the book
          [] ->
            ( (Just $ order { quantity = remaining })
            , fillable
            , unfillable
            )
          -- We have opposing orders; modify the first available
          (y:ys) ->
            ( Nothing
            , fillable ++ [y { quantity = remaining }]
            , y { quantity = quantity y - remaining } : ys
            )
