module Main where

import Exchange.Core
import Exchange.Book
import Exchange.Messages
import Exchange.Order
import Exchange.Trader

import Data.Map.Strict (fromList)

bids' :: [Order]
bids' = [ Order 100 97.0 (OrderData LIMIT PARTIAL BID) (OrderMetadata "order4" "trader4" "TICKER")
        , Order 100 98.0 (OrderData LIMIT PARTIAL BID) (OrderMetadata "order5" "trader5" "TICKER")
        , Order 100 99.0 (OrderData LIMIT PARTIAL BID) (OrderMetadata "order6" "trader6" "TICKER")
        ]

asks' :: [Order]
asks' = [ Order 100 101.0 (OrderData LIMIT PARTIAL ASK) (OrderMetadata "order1" "trader1" "TICKER")
        , Order 100 102.0 (OrderData LIMIT PARTIAL ASK) (OrderMetadata "order2" "trader2" "TICKER")
        , Order 100 103.0 (OrderData LIMIT PARTIAL ASK) (OrderMetadata "order3" "trader3" "TICKER")
        ]

books = fromList [("TICKER", Book bids' asks')]
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
main = putStrLn (show $ update (CANCEL (OrderMetadata "order2" "trader2" "TICKER")) exchange)
