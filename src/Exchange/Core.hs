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
import qualified Exchange.Messages as EM
import           Exchange.Trader
import           Data.List
import qualified Data.Map.Strict   as M
import           Data.Maybe               (fromMaybe)
import           Numeric.Limits
--------------------------------------------------------------------------------


-- | Type aliases to make life easier.
type Books    = M.Map String Book
type Traders  = M.Map String Trader

data Exchange = Exchange
  { books   :: Books
  , traders :: Traders
  , lastOid :: Int
  } deriving (Show)


-- | Initial (empty) state of the exchange
empty :: Exchange
empty = Exchange M.empty M.empty 0


-- | Update; handle orders and cancellations and such.
update :: EM.Request -> Exchange -> (Exchange, EM.Response)

update msgLimit @ (EM.Limit _ _ _ _ _) exchange = result
  where
    -- Get the exchange's parameters.
    msgBooks    = books exchange
    msgTraders  = traders exchange
    msgOid      = lastOid exchange
    -- Get the message's parameters.
    msgTrader   = EM.trader   msgLimit
    msgTicker   = EM.ticker   msgLimit
    msgQuantity = EM.quantity msgLimit
    msgPrice    = EM.price    msgLimit
    msgDir      = EM.dir      msgLimit
    msgMd       = Metadata msgOid msgTrader msgTicker
    msgOrder    = Order msgQuantity msgPrice msgMd
    -- Handle the order.
    result = case msgBooks M.!? msgTicker of
      -- If the ticker isn't in the map, do nothing.
      Nothing ->
        ( exchange
        , EM.Response EM.Error ("Ticker \"" ++ msgTicker ++ "\"not found")
        )
      Just (Book bids asks lastP) -> (exchange', EM.Response EM.Ok $ show msgMd)
        where
          -- Get the result of applying the order to the book.
          (potential, filled, unfilled) = handleOrder msgDir msgOrder opps
            where
              opps = case msgDir of
                Bid -> asks
                Ask -> bids

          -- Update the last traded price.
          lastP' = case reverse filled of
            []  -> lastP
            x:_ -> Just $ price x

          -- Generate the new book after the trade is completed.
          newBook = case msgDir of
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
                  cash = case msgDir of
                    Bid -> -1 * tp * fromIntegral tq
                    Ask ->  1 * tp * fromIntegral tq
                  ts =
                    [ (msgTrader, \t -> t { funds = funds t + cash })
                    , (tid tmd, \t -> t { funds = funds t - cash })
                    ]

          -- Update the values in the map.
          books'    = M.adjust (const newBook) msgTicker msgBooks
          traders'  = foldr (\(k, v) ts' -> M.adjust v k ts') msgTraders traderUpdates
          exchange' = Exchange books' traders' (msgOid + 1)

update msgMarket @ (EM.Market _ _ _ _) exchange = result
  where
    -- Get the exchange's parameters.
    msgBooks    = books exchange
    msgTraders  = traders exchange
    msgOid      = lastOid exchange
    -- Get the message's parameters.
    msgTrader   = EM.trader   msgMarket
    msgTicker   = EM.ticker   msgMarket
    msgQuantity = EM.quantity msgMarket
    msgDir      = EM.dir      msgMarket
    msgMd       = Metadata msgOid msgTrader msgTicker
    -- Handle the order.
    result = case msgBooks M.!? msgTicker of
      -- If the ticker isn't in the map, do nothing.
      Nothing ->
        ( exchange
        , EM.Response EM.Error ("Ticker \"" ++ msgTicker ++ "\" not found")
        )
      Just (Book bids asks lastP) -> (exchange', EM.Response EM.Ok $ show msgMd)
        where
          -- Create a "limit" order unrestrained by price
          fakeLimit = case msgDir of
            Bid -> Order msgQuantity maxValue msgMd
            Ask -> Order msgQuantity minValue msgMd
          (potential, filled, unfilled) = handleOrder msgDir fakeLimit opps
            where
              opps = case msgDir of
                Bid -> asks
                Ask -> bids

          -- Update the last traded price.
          lastP' = case reverse filled of
            []  -> lastP
            x:_ -> Just $ price x

          -- Generate the new book after the trade is completed.
          newBook = case msgDir of
            Bid -> Book bids unfilled lastP'
            Ask -> Book unfilled asks lastP'

          -- Generate the necessary updates for the filled orders' traders.
          traderUpdates = concat $ createUpdate <$> filled
            where
              createUpdate (Order tq tp tmd) = ts
                where
                  cash = case msgDir of
                    Bid -> -1 * tp * fromIntegral tq
                    Ask ->  1 * tp * fromIntegral tq
                  ts =
                    [ (msgTrader, \t -> t { funds = funds t + cash })
                    , (tid tmd, \t -> t { funds = funds t - cash })
                    ]

          -- Update the values in the map.
          books'    = M.adjust (const newBook) msgTicker msgBooks
          traders'  = foldr (\(k, v) ts' -> M.adjust v k ts') msgTraders traderUpdates
          exchange' = Exchange books' traders' (msgOid + 1)

-- | Register a security (i.e. create an orderbook if applicable)
update (EM.RegisterS ticker) exchange = (exchange', resp)
  where
    msgBooks = books exchange
    (books', resp) = case msgBooks M.!? ticker of
      Nothing ->
        ( M.insert ticker (Book [] [] Nothing) msgBooks
        , EM.Response EM.Ok ("Inserting \"" ++ ticker ++ "\"")
        )
      Just _  ->
        ( msgBooks
        , EM.Response EM.Error ("Ticker \"" ++ ticker ++ "\" already exists")
        )
    exchange' = exchange { books = books'}

-- | Register a trader
update (EM.RegisterT trader) exchange = (exchange', resp)
  where
    msgTraders = traders exchange
    (traders', resp) = case msgTraders M.!? trader of
      Nothing ->
        ( M.insert trader (Trader 0) msgTraders
        , EM.Response EM.Ok ("Inserting \"" ++ trader ++ "\"")
        )
      Just _  ->
        ( msgTraders
        , EM.Response EM.Error ("Trader \"" ++ trader ++ "\" already exists")
        )
    exchange' = exchange { traders = traders' }

-- | Get the current status of the exchange; a no-op.
update EM.Status exchange = (exchange, EM.Response EM.Ok $ show exchange)

-- | Remove the requested order from the orderbook.
update (EM.Cancel msgMd) exchange =
  ( exchange'
  , EM.Response EM.Ok ("Removing order matching " ++ (show msgMd))
  )
  where
    keep :: [Order] -> [Order]
    keep = filter ((msgMd /=) . metadata)
    removeOrder :: Book -> Book
    removeOrder (Book bids asks lastP) = Book (keep bids) (keep asks) lastP
    books' = M.adjust removeOrder (ticker msgMd) (books exchange)
    exchange' = exchange { books = books' }


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
