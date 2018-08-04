--------------------------------------------------------------------------------
import SpecRunner

import           Exchange.Book
import           Exchange.Core
import           Exchange.Order
import qualified Exchange.Messages as Msg
import           Exchange.Trader

import           Data.List         (find)
import qualified Data.Map.Strict as M
import           Data.Maybe        (fromMaybe, isNothing)
--------------------------------------------------------------------------------

-- | Test setup
testbids :: [Order]
testbids = [ Order 100 85 (Metadata "order1" "trader1" "TICKER")
        , Order 100 90 (Metadata "order2" "trader2" "TICKER")
        , Order 100 95 (Metadata "order3" "trader3" "TICKER")
        ]

testasks :: [Order]
testasks = [ Order 100 105 (Metadata "order4" "trader4" "TICKER")
        , Order 100 110 (Metadata "order5" "trader5" "TICKER")
        , Order 100 115 (Metadata "order6" "trader6" "TICKER")
        ]

books   = M.fromList [("TICKER", Book testbids testasks Nothing)]
traders = M.fromList [ ("trader1", Trader 100.0)
                   , ("trader2", Trader 100.0)
                   , ("trader3", Trader 100.0)
                   , ("trader4", Trader 100.0)
                   , ("trader5", Trader 100.0)
                   , ("trader6", Trader 100.0)
                   ]

testmd :: Metadata
testmd = Metadata "order7" "trader1" "TICKER"

exchange :: Exchange
exchange = (books, traders)

orderIsIn :: Metadata -> Book -> Bool
orderIsIn md (Book bids asks _) = result
  where
    result = case find ((md ==) . metadata) (bids ++ asks) of
      Just _  -> True
      Nothing -> False

-- | Order cancellation
cancelSpec :: Bool
cancelSpec = result
  where
    testmd' :: Metadata
    testmd' = Metadata "order1" "trader1" "TICKER"
    msg :: Msg.Request
    msg = Msg.Cancel testmd'
    testbooks :: M.Map String Book
    (testbooks, _) = update msg exchange
    orderbook = testbooks M.! "TICKER"
    Book bids asks _ = orderbook
    conditions =
      [ length bids == 2
      , length asks == 3
      , not $ orderIsIn testmd' orderbook
      ]
    result = and conditions

-- | Create a filled order
fillOrderSpec :: Bool
fillOrderSpec = result
  where
    msg :: Msg.Request
    msg = Msg.Market 100 Bid testmd
    testbooks :: M.Map String Book
    (testbooks, _) = update msg exchange
    orderbook = testbooks M.! "TICKER"
    Book bids asks lp = orderbook
    conditions =
      [ length bids == 3
      , length asks == 2
      , lp == Just 105
      , not $ orderIsIn testmd orderbook
      ]
    result = and conditions

-- | Create an unfilled order
unfillLimitOrderSpec :: Bool
unfillLimitOrderSpec = result
  where
    order :: Order
    order = Order 100 99 testmd
    msg :: Msg.Request
    msg = Msg.Limit order Bid
    testbooks :: M.Map String Book
    (testbooks, _) = update msg exchange
    orderbook = testbooks M.! "TICKER"
    Book testbids testasks lp = orderbook
    conditions =
      [ length testbids == 4
      , length testasks == 3
      , isNothing lp
      , orderIsIn testmd orderbook
      ]
    result = and conditions

-- | Create a filled  order
fillLimitOrderSpec :: Bool
fillLimitOrderSpec = result
  where
    order :: Order
    order = Order 100 120 testmd
    msg :: Msg.Request
    msg = Msg.Limit order Bid
    testbooks :: M.Map String Book
    (testbooks, testtraders) = update msg exchange
    orderbook = testbooks M.! "TICKER"
    trader = testtraders M.! "trader1"
    Book testbids testasks _ = orderbook
    conditions =
      [ length testbids == 3
      , length testasks == 2
      , not $ orderIsIn testmd orderbook
      , funds trader == -10400
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
