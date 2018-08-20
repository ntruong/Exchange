--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Server.Core
  ( main
  ) where

import qualified Exchange.Core as E
import qualified Exchange.Messages as EM
import qualified Exchange.Order as EO
import           Control.Monad.Trans.Resource   (ResourceT)
import qualified Data.Aeson as A
import           Data.Aeson.Types
import           Data.ByteString.Lazy.Char8     (pack)
import           Data.Maybe                     (catMaybes, mapMaybe)
import           Data.IORef
import qualified Data.HashMap.Strict as HM      (lookup)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
--------------------------------------------------------------------------------


-- | Handles updates to the exchange.
exchangeApp' :: EM.Request -> IORef E.Exchange -> ResourceT IO Response
exchangeApp' request exchange =
  return $ responseLBS
    status200
    [("Content-Type", "text/html")]
    ((pack . show) E.empty)
  where
    update :: IORef E.Exchange -> IO E.Exchange
    update exch = atomicModifyIORef exch (\x -> (x, E.update request x))

-- need to get Request -> Response
-- we have Request -> IO [EM.Request] so we should be able to update the
-- exchange


-- | Routes requests to the server correspondingly.
router :: IORef E.Exchange -> Application
router exchange request respond = do
  print request
  case requestMethod request of
    -- Handle http GETs.
    "GET" -> case pathInfo request of
      -- "/": load the index page.
      [] -> respond indexPage

      -- "/res/*": load a resource.
      ("res":res) ->
        let req = request { pathInfo = res }
        in  fileServer req respond

      -- load a 404 for anything else.
      _ -> respond notFound

    -- Handle http POSTs.
    "POST" -> do
      -- TODO(ntruong): we can either fold all the updates into one massive
      -- update (and lose granularity if/when it errors) or perform them one by
      -- one and conveniently ignore the errors for now but should be better in
      -- the future
      requests <- decode request
      responses <- mapM updateExchangeM requests
      let resp = foldr max (EM.Response EM.Ok E.empty) responses
      respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        ((pack . show) resp)
      where
        updateExchangeM :: EM.Request -> IO (EM.Response E.Exchange)
        updateExchangeM req = atomicModifyIORef exchange $ \e ->
          (E.update req e, EM.Response EM.Ok E.empty)


-- | Let requests to the exchange be parseable. We flatten requests to make
-- parsing JSONs easier.

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

-- TODO(ntruong): what happens if JSON parsing fails? does the exchange crash?
-- Parse requests.
instance FromJSON EM.Request where
  parseJSON = withObject "Exchange.Messages.Request" $ \req ->
    case HM.lookup "type" req of
      Nothing -> typeMismatch "type" (Object req)

      -- {
      --   type     : "LIMIT"
      -- , quantity : Int
      -- , price    : Float
      -- , dir      : String
      -- , oid      : String
      -- , tid      : String
      -- , ticker   : String
      -- }
      Just "LIMIT" ->
        let msgMd = EO.Metadata
              <$> req .: "oid"
              <*> req .: "tid"
              <*> req .: "ticker"
            msgOrd = EO.Order
              <$> req .: "quantity"
              <*> req .: "price"
              <*> msgMd
        in  EM.Limit
              <$> msgOrd
              <*> req .: "dir"
      -- {
      --   type     : "MARKET"
      -- , quantity : Int
      -- , dir      : String
      -- , oid      : String
      -- , tid      : String
      -- , ticker   : String
      -- }
      Just "MARKET" ->
        let msgMd = EO.Metadata
              <$> req .: "oid"
              <*> req .: "tid"
              <*> req .: "ticker"
        in  EM.Market
              <$> req .: "quantity"
              <*> req .: "dir"
              <*> msgMd

      -- {
      --   type   : "REGISTERS"
      -- , ticker : String
      -- }
      Just "REGISTERS" -> EM.RegisterS <$> req .: "ticker"

      -- {
      --   type   : "REGISTERT"
      -- , trader : String
      -- }
      Just "REGISTERT" -> EM.RegisterT <$> req .: "trader"

      -- {
      --   type   : "STATUS"
      -- }
      Just "STATUS" -> return EM.Status

      -- {
      --   type     : "CANCEL"
      -- , oid      : String
      -- , tid      : String
      -- , ticker   : String
      -- }
      Just "CANCEL" ->
        let msgMd = EO.Metadata
              <$> req .: "oid"
              <*> req .: "tid"
              <*> req .: "ticker"
        in  EM.Cancel <$> msgMd


-- | Decode a request.
decode :: Request -> IO [EM.Request]
decode request = do
  -- Parse the request. `parseRequestBody` returns ([Param], [File _]) and Param
  -- is just (Bytestring, Bytestring). Posts should have a field "request"
  -- pointing to a stringified JSON of the desired request.
  (params, _) <- parseRequestBody lbsBackEnd request
  print params
  let reqs = filter (\(x, _) -> x == "request") params
  print reqs
  return $ mapMaybe (A.decodeStrict . snd) reqs


-- | Serve static files (e.g. html/css/js) needed by the server.
fileServer :: Application
fileServer = staticApp (defaultFileServerSettings "res/")


-- | The index page.
indexPage :: Response
indexPage = responseFile
  status200
  [("Content-Type", "text/html")]
  "res/index.html"
  Nothing


-- | Give a 404 if requesting an invalid resource/path.
notFound :: Response
notFound = responseLBS
  status404
  [("Content-Type", "text/plain")]
  "404 - Not found"


-- | Run the application; should be called in Main.
main :: IO ()
main = do
  exchange <- newIORef E.empty
  run 8080 $ router exchange
