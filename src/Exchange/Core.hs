--------------------------------------------------------------------------------
module Exchange.Core
  ( Exchange
  , Books
  , Traders
  , Trader(..)
  , update
  ) where

import           Exchange.Book
import           Exchange.Order
import qualified Exchange.Messages                              as Msg
import           Exchange.Trader
import           Data.List           (inits, partition, sort)
import           Data.Map.Strict     (Map, adjust, insert, (!?))
import           Data.Maybe          (fromMaybe)
import           Data.Ord            (Down)
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
update :: Msg.Message -> Exchange -> Exchange

update (Msg.Limit msgorder msgdir) (books, traders) = result
  where
    msgticker = (ticker . metadata) msgorder
    msgtrader = (tid . metadata) msgorder
    result = case books !? msgticker of
      -- If the ticker isn't in the map, do nothing.
      -- TODO: should return an error at some point, when responses are implemented.
      Nothing -> (books, traders)
      Just (Book bids asks lastP) -> (books', traders')
        where
          -- Get the result of applying the order to the book.
          (potential, filled, unfilled) = handleOrder msgdir msgorder opps
            where
              opps = case msgdir of
                Bid -> asks
                Ask -> bids

          -- Update the last traded price.
          lastP' = case reverse filled of
            []  -> lastP
            x:_ -> Just $ price x

          -- Generate the new book after the trade is completed.
          newBook = case msgdir of
            Bid -> case potential of
              Just x  -> Book (x:bids) unfilled lastP'
              Nothing -> Book bids unfilled lastP'
            Ask -> case potential of
              Just x  -> Book unfilled (x:asks) lastP'
              Nothing -> Book unfilled asks lastP'

          -- Generate the necessary updates for the filled orders' traders.
          traderUpdates = concat $ createUpdate <$> filled
            where
              createUpdate (Order tq tp tmd) = ts
                where
                  cash = case msgdir of
                    Bid -> -1 * tp * fromIntegral tq
                    Ask ->  1 * tp * fromIntegral tq
                  ts =
                    [ (msgtrader, \t -> t { funds = funds t + cash })
                    , (tid tmd, \t -> t { funds = funds t - cash })
                    ]

          -- Update the values in the map.
          books' = adjust (const newBook) msgticker books
          traders' = foldr (\(k, v) ts' -> adjust v k ts') traders traderUpdates

-- | Remove the requested order from the orderbook.
update (Msg.Cancel msgmd) (books, traders) = (books', traders)
  where
    keep :: [Order] -> [Order]
    keep = filter ((msgmd /=) . metadata)
    removeOrder :: Book -> Book
    removeOrder (Book bids asks lastP) = Book (keep bids) (keep asks) lastP
    books' = adjust removeOrder (ticker msgmd) books

-- | Reduce a book with a given order, returning a potentially unfilled order to
-- be left on the book, filled orders, and unfilled orders.
handleOrder :: Direction -> Order -> [Order] -> (Maybe Order, [Order], [Order])
handleOrder direction order orders = (potential, filled, unfilled)
  where
    -- Sort the opposing orders based on fill precedence.
    sorted = case direction of
      Bid -> sort orders
      -- Ask -> sortOn Down orders

    -- Zip the orders together with the cumulative amount filled.
    cumulative :: [(Order, Int)]
    cumulative = zip sorted (tail . scanl (+) 0 $ quantity <$> sorted)

    -- Split the list based on full fills and potential fills
    goodQuant :: (Order, Int) -> Bool
    goodQuant = (<= quantity order) . snd
    goodPrice :: (Order, Int) -> Bool
    goodPrice = f . price . fst
      where
        f = case direction of
          Bid -> (<= price order)
          Ask -> (>= price order)

    -- All the conditions an order must fulfill to be fillable.
    conditions =
      [ goodQuant
      , goodPrice
      ]

    -- Parse out fillable/unfillable orders from the cumulative list.
    (fillable, unfillable) =
      let (x, y) = partition (\x -> and (($ x) <$> conditions)) cumulative
      in  (fst <$> x, fst <$> y)

    -- Calculate results.
    filledQuantity = case (take $ length fillable) cumulative of
      [] -> 0
      xs -> (snd . last) xs
    potential = case quantity order - filledQuantity of
      0 -> Nothing
      x -> Just order { quantity = x }
    (filled, unfilled) = case quantity order - filledQuantity of
      -- Lucky! It was a perfect fill.
      0 -> (fillable, unfillable)
      x -> case unfillable of
        []     -> (fillable, unfillable)
        (y:ys) ->
          ( y { quantity = x } : fillable
          , y { quantity = quantity y - x } : ys
          )
