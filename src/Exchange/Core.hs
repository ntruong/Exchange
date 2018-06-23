module Exchange.Core
  ( Exchange
  ) where

import Exchange.Book

import qualified Data.Map.Strict as Map

{- TODO(ntruong):
 -   market orders
 -   limit orders
 -   order cancellation
 -   implement notifications/effectual
 -}

type Exchange = Map.Map String Book
