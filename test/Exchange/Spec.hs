--------------------------------------------------------------------------------
import SpecRunner

import Exchange.Book
import Exchange.Core
import Exchange.Order
import Exchange.Messages
import Exchange.Trader

import Data.List       (find)
import Data.Map.Strict (Map, fromList, (!))
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

cancelSpec :: Bool
cancelSpec = result
  where
    order :: OrderInfo
    order = OrderInfo "order1" "trader1" "TICKER"
    msg :: Message
    msg = CANCEL order
    books :: Map String Book
    (books, _) = update msg exchange
    Book bids asks _ = books ! "TICKER"
    conditions =
      [ length bids == 2
      , length asks == 3
      , case find ((order ==) . info) (bids ++ asks) of
          Just _  -> False
          Nothing -> True
      ]
    result = and conditions

main :: IO ()
main = runner "Exchange" specs
  where
    specs =
      [ ("cancelSpec", cancelSpec)
      ]
