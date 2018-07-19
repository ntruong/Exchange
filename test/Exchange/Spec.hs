--------------------------------------------------------------------------------
import SpecRunner

import Exchange.Book
import Exchange.Core
import Exchange.Order
import Exchange.Messages
import Exchange.Trader

import Data.List       (find)
import Data.Map.Strict (Map, fromList, map, (!))
import Data.Maybe      (fromMaybe)
--------------------------------------------------------------------------------

-- | Test setup
bids' :: [Order]
bids' = [ Order 100 PARTIAL BID MARKET (OrderInfo "order1" "trader1" "TICKER")
        , Order 100 PARTIAL BID MARKET (OrderInfo "order2" "trader2" "TICKER")
        , Order 100 PARTIAL BID MARKET (OrderInfo "order3" "trader3" "TICKER")
        ]

asks' :: [Order]
asks' = [ Order 100 PARTIAL ASK MARKET (OrderInfo "order4" "trader4" "TICKER")
        , Order 100 PARTIAL ASK MARKET (OrderInfo "order5" "trader5" "TICKER")
        , Order 100 PARTIAL ASK MARKET (OrderInfo "order6" "trader6" "TICKER")
        ]

books = fromList [("TICKER", Book bids' asks' Nothing)]
traders = fromList [ ("trader1", Trader 100.0)
                   , ("trader2", Trader 100.0)
                   , ("trader3", Trader 100.0)
                   , ("trader4", Trader 100.0)
                   , ("trader5", Trader 100.0)
                   , ("trader6", Trader 100.0)
                   ]

exchange :: Exchange
exchange = (books, traders)

orderIsIn :: OrderInfo -> Book -> Bool
orderIsIn order (Book bids asks _) = result
  where
    result = case find ((order ==) . info) (bids ++ asks) of
      Just _  -> True
      Nothing -> False

-- | Order cancellation
cancelSpec :: Bool
cancelSpec = result
  where
    order :: OrderInfo
    order = OrderInfo "order1" "trader1" "TICKER"
    msg :: Message
    msg = CANCEL order
    testbooks :: Map String Book
    (testbooks, _) = update msg exchange
    orderbook = testbooks ! "TICKER"
    Book bids asks _ = orderbook
    conditions =
      [ length bids == 2
      , length asks == 3
      , not $ orderIsIn order orderbook
      ]
    result = and conditions

-- | Create a filled order
fillOrderSpec :: Bool
fillOrderSpec = result
  where
    info :: OrderInfo
    info = OrderInfo "order7" "trader1" "TICKER"
    order :: Order
    order = Order 100 PARTIAL BID MARKET info
    msg :: Message
    msg = ORDER order
    testbooks :: Map String Book
    (testbooks, _) = update msg exchange
    orderbook = testbooks ! "TICKER"
    Book bids asks _ = orderbook
    conditions =
      [ length bids == 3
      , length asks == 2
      , not $ orderIsIn info orderbook
      ]
    result = and conditions

-- | Create an unfilled (LIMIT) order
unfillLimitOrderSpec :: Bool
unfillLimitOrderSpec = result
  where
    makeLimit :: Order -> Order
    makeLimit order = order { contract = LIMIT 100 }
    transform :: Book -> Book
    transform book = book
      { bids = makeLimit <$> bids book
      , asks = makeLimit <$> asks book
      }
    books' :: Map String Book
    books' = Data.Map.Strict.map transform books
    exchange' :: Exchange
    exchange' = (books', traders)
    info :: OrderInfo
    info = OrderInfo "order7" "trader1" "TICKER"
    order :: Order
    order = Order 100 PARTIAL BID (LIMIT 90) info
    msg :: Message
    msg = ORDER order
    testbooks :: Map String Book
    (testbooks, _) = update msg exchange'
    orderbook = testbooks ! "TICKER"
    Book bids' asks' _ = orderbook
    conditions =
      [ length bids' == 4
      , length asks' == 3
      , orderIsIn info orderbook
      ]
    result = and conditions

-- | Create an filled (LIMIT) order
fillLimitOrderSpec :: Bool
fillLimitOrderSpec = result
  where
    makeLimit :: Order -> Order
    makeLimit order = order { contract = LIMIT 100 }
    transform :: Book -> Book
    transform book = book
      { bids = makeLimit <$> bids book
      , asks = makeLimit <$> asks book
      }
    books' :: Map String Book
    books' = Data.Map.Strict.map transform books
    exchange' :: Exchange
    exchange' = (books', traders)
    info :: OrderInfo
    info = OrderInfo "order7" "trader1" "TICKER"
    order :: Order
    order = Order 100 PARTIAL BID (LIMIT 100) info
    msg :: Message
    msg = ORDER order
    testbooks :: Map String Book
    (testbooks, testtraders) = update msg exchange'
    orderbook = testbooks ! "TICKER"
    trader = testtraders ! "trader1"
    Book bids' asks' _ = orderbook
    conditions =
      [ length bids' == 3
      , length asks' == 2
      , not $ orderIsIn info orderbook
      , funds trader == -9900
      ]
    result = and conditions

main :: IO ()
main = runner "Exchange" specs
  where
    specs =
      [ ("cancelSpec",           cancelSpec)
      , ("fillOrderSpec",        fillOrderSpec)
      , ("unfillLimitOrderSpec", unfillLimitOrderSpec)
      , ("fillLimitOrderSpec",   fillLimitOrderSpec)
      ]
