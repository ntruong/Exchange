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
testBids :: [Order]
testBids =
  [ Order 100 85 (Metadata 1 "trader1" "TICKER")
  , Order 100 90 (Metadata 2 "trader2" "TICKER")
  , Order 100 95 (Metadata 3 "trader3" "TICKER")
  ]

testAsks :: [Order]
testAsks =
  [ Order 100 105 (Metadata 4 "trader4" "TICKER")
  , Order 100 110 (Metadata 5 "trader5" "TICKER")
  , Order 100 115 (Metadata 6 "trader6" "TICKER")
  ]

testBooks   = M.fromList [("TICKER", Book testBids testAsks Nothing)]
testTraders = M.fromList [ ("trader1", Trader 100.0)
                         , ("trader2", Trader 100.0)
                         , ("trader3", Trader 100.0)
                         , ("trader4", Trader 100.0)
                         , ("trader5", Trader 100.0)
                         , ("trader6", Trader 100.0)
                         ]

testMd :: Metadata
testMd = Metadata 7 "trader1" "TICKER"

exchange :: Exchange
exchange = Exchange testBooks testTraders 7

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
    testMd' :: Metadata
    testMd' = Metadata 1 "trader1" "TICKER"
    msg :: Msg.Request
    msg = Msg.Cancel testMd'
    testBooks :: M.Map String Book
    (Exchange testBooks _ _, _) = update msg exchange
    orderbook = testBooks M.! "TICKER"
    Book bids asks _ = orderbook
    conditions =
      [ length bids == 2
      , length asks == 3
      , not $ orderIsIn testMd' orderbook
      ]
    result = and conditions

-- | Create a filled order
fillOrderSpec :: Bool
fillOrderSpec = result
  where
    msg :: Msg.Request
    msg = Msg.Market "trader1" "TICKER" 100 Bid
    testBooks :: M.Map String Book
    (Exchange testBooks _ _, _) = update msg exchange
    orderbook = testBooks M.! "TICKER"
    Book bids asks lp = orderbook
    conditions =
      [ length bids == 3
      , length asks == 2
      , lp == Just 105
      , not $ orderIsIn testMd orderbook
      ]
    result = and conditions

-- | Create an unfilled order
unfillLimitOrderSpec :: Bool
unfillLimitOrderSpec = result
  where
    msg :: Msg.Request
    msg = Msg.Limit "trader1" "TICKER" 100 99 Bid
    testBooks :: M.Map String Book
    (Exchange testBooks _ _, _) = update msg exchange
    orderbook = testBooks M.! "TICKER"
    Book testBids testAsks lp = orderbook
    conditions =
      [ length testBids == 4
      , length testAsks == 3
      , isNothing lp
      , orderIsIn testMd orderbook
      ]
    result = and conditions

-- | Create a filled  order
fillLimitOrderSpec :: Bool
fillLimitOrderSpec = result
  where
    msg :: Msg.Request
    msg = Msg.Limit "trader1" "TICKER" 100 120 Bid
    testBooks :: M.Map String Book
    testTraders :: M.Map String Trader
    (Exchange testBooks testTraders _, _) = update msg exchange
    orderbook = testBooks M.! "TICKER"
    trader = testTraders M.! "trader1"
    Book testBids testAsks _ = orderbook
    conditions =
      [ length testBids == 3
      , length testAsks == 2
      , not $ orderIsIn testMd orderbook
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
