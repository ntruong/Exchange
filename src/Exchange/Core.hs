--------------------------------------------------------------------------------
module Exchange.Core
  ( Exchange
  , Books
  , Traders
  , Trader(..)
  , empty
  , update
  ) where

import           Exchange.Book
import           Exchange.Order
import qualified Exchange.Messages as Msg
import           Exchange.Trader
import           Data.List
import qualified Data.Map.Strict   as M
import           Data.Maybe               (fromMaybe)
import           Numeric.Limits
--------------------------------------------------------------------------------


-- | Type aliases to make life easier.
type Exchange = (Books, Traders)
type Books    = M.Map String Book
type Traders  = M.Map String Trader


-- | Initial (empty) state of the exchange
empty :: Exchange
empty = (M.empty, M.empty)


-- | Update; handle orders and cancellations and such.
update :: Msg.Request -> Exchange -> Exchange

update (Msg.Limit msgorder msgdir) (books, traders) = result
  where
    msgticker = (ticker . metadata) msgorder
    msgtrader = (tid . metadata) msgorder
    result = case books M.!? msgticker of
      -- If the ticker isn't in the map, do nothing.
      -- TODO(ntruong): should return an error at some point, when responses are
      --   implemented.
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
          books' = M.adjust (const newBook) msgticker books
          traders' = foldr (\(k, v) ts' -> M.adjust v k ts') traders traderUpdates

update (Msg.Market msgquant msgdir msgmd) (books, traders) = result
  where
    msgticker = ticker msgmd
    msgtrader = tid msgmd
    result = case books M.!? msgticker of
      -- If the ticker isn't in the map, do nothing.
      Nothing -> (books, traders)
      Just (Book bids asks lastP) -> (books', traders')
        where
          -- Create a "limit" order unrestrained by price
          fakeLimit = case msgdir of
            Bid -> Order msgquant maxValue msgmd
            Ask -> Order msgquant minValue msgmd
          (potential, filled, unfilled) = handleOrder msgdir fakeLimit opps
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
            Bid -> Book bids unfilled lastP'
            Ask -> Book unfilled asks lastP'

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
          books' = M.adjust (const newBook) msgticker books
          traders' = foldr (\(k, v) ts' -> M.adjust v k ts') traders traderUpdates

-- | Register a security (i.e. create an orderbook if applicable)
update (Msg.RegisterS ticker) (books, traders) = (books', traders)
  where
    books' = case books M.!? ticker of
      Nothing -> M.insert ticker (Book [] [] Nothing) books
      Just _  -> books

-- | Register a trader
update (Msg.RegisterT trader) (books, traders) = (books, traders')
  where
    traders' = case traders M.!? trader of
      Nothing -> M.insert trader (Trader 0) traders
      Just _  -> traders

-- | Get the current status of the exchange; a no-op.
update Msg.Status exchange = exchange

-- | Remove the requested order from the orderbook.
update (Msg.Cancel msgmd) (books, traders) = (books', traders)
  where
    keep :: [Order] -> [Order]
    keep = filter ((msgmd /=) . metadata)
    removeOrder :: Book -> Book
    removeOrder (Book bids asks lastP) = Book (keep bids) (keep asks) lastP
    books' = M.adjust removeOrder (ticker msgmd) books


-- | Reduce a book with a given order, returning a potentially unfilled order to
-- be left on the book, filled orders, and unfilled orders.
handleOrder :: Direction -> Order -> [Order] -> (Maybe Order, [Order], [Order])
handleOrder direction order orders = (potential, filled, unfilled)
  where
    -- Sort the opposing orders based on fill precedence.
    sorted = case direction of
      Bid -> sort orders
      Ask -> (reverse . sort) orders

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
        (y:ys) -> if goodPrice (y, 0)
          then
            ( y { quantity = x } : fillable
            , y { quantity = quantity y - x } : ys
            )
          else (fillable, unfillable)
