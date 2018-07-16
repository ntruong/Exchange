module Main where

import Exchange.Book
import Exchange.Core
import Exchange.Order
import Exchange.Messages
import Exchange.Trader

import Data.Map.Strict (fromList)

bids' :: [Order]
bids' = [ Order 100 PARTIAL BID MARKET (OrderInfo "order4" "trader4" "TICKER")
        , Order 100 PARTIAL BID MARKET (OrderInfo "order5" "trader5" "TICKER")
        , Order 100 PARTIAL BID MARKET (OrderInfo "order6" "trader6" "TICKER")
        ]

asks' :: [Order]
asks' = [ Order 100 PARTIAL ASK MARKET (OrderInfo "order1" "trader1" "TICKER")
        , Order 100 PARTIAL ASK MARKET (OrderInfo "order2" "trader2" "TICKER")
        , Order 100 PARTIAL ASK MARKET (OrderInfo "order3" "trader3" "TICKER")
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

main :: IO ()
main = putStrLn (show $ update (CANCEL (OrderInfo "order2" "trader2" "TICKER")) exchange)
