--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Server.Messages
  (
  ) where

import qualified Exchange.Messages as EM
import qualified Exchange.Order as EO
import           Data.Aeson
import           Data.Aeson.Types
--------------------------------------------------------------------------------


-- Parse order metadata.
instance FromJSON EO.Metadata where
  parseJSON = withObject "Exchange.Messages.Metadata" $ \md -> EO.Metadata
    <$> md .: "oid"
    <*> md .: "tid"
    <*> md .: "ticker"

-- Parse order direction.
instance FromJSON EO.Direction where
  parseJSON = withText "Exchange.Messages.Direction" $ \dir -> case dir of
    "BID" -> return EO.Bid
    "ASK" -> return EO.Ask
    _     -> typeMismatch "Exchange.Messages.Direction" (String dir)

-- Parse EM.Request.
instance FromJSON EM.Request where
  parseJSON = withObject "Exchange.Messages.Request" $ \req -> do
    reqType <- req .: "type"
    case reqType of

      -- { type     : "limit"
      -- , tid      : String
      -- , ticker   : String
      -- , quantity : Int
      -- , price    : Float
      -- , dir      : String
      -- }
      String "limit" -> EM.Limit
        <$> req .: "tid"
        <*> req .: "ticker"
        <*> req .: "quantity"
        <*> req .: "price"
        <*> req .: "dir"

      -- { type     : "market"
      -- , tid      : String
      -- , ticker   : String
      -- , quantity : Int
      -- , dir      : String
      -- }
      String "market" -> EM.Market
        <$> req .: "tid"
        <*> req .: "ticker"
        <*> req .: "quantity"
        <*> req .: "dir"

      -- { type   : "registers"
      -- , ticker : String
      -- }
      String "registers" -> EM.RegisterS <$> req .: "ticker"

      -- { type   : "registert"
      -- , trader : String
      -- }
      String "registert" -> EM.RegisterT <$> req .: "trader"

      -- { type   : "status" }
      String "status" -> return EM.Status

      -- { type     : "cancel"
      -- , oid      : String
      -- , tid      : String
      -- , ticker   : String
      -- }
      String "cancel" ->
        let msgMd = EO.Metadata
              <$> req .: "oid"
              <*> req .: "tid"
              <*> req .: "ticker"
        in  EM.Cancel <$> msgMd

      _ -> typeMismatch "type" (Object req)
