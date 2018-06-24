module Main where

import Exchange.Core
import Exchange.Book
import Exchange.Messages

import Data.Map.Strict (fromList)

bids' :: [Order]
bids' = [ Order 100 97.0 (OrderData "order4" "trader4" "TICKER")
       , Order 100 98.0 (OrderData "order5" "trader5" "TICKER")
       , Order 100 99.0 (OrderData "order6" "trader6" "TICKER")
       ]

asks' :: [Order]
asks' = [ Order 100 101.0 (OrderData "order1" "trader1" "TICKER")
       , Order 100 102.0 (OrderData "order2" "trader2" "TICKER")
       , Order 100 103.0 (OrderData "order3" "trader3" "TICKER")
       ]

book :: Book
book = Book bids' asks'

exchange :: Exchange
exchange = fromList [("TICKER", book)]

main :: IO ()
main = putStrLn (show $ update (CANCEL (OrderData "order2" "trader2" "TICKER")) exchange)
