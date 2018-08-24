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
      requests  <- decode request
      responses <- mapM updateExchangeM requests
      exchange' <- readIORef exchange
      let resp = case responses of
            [] -> EM.Response EM.Error "Could not parse requests."
            _  -> foldr (<>) mempty responses
      respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        ((pack . show) resp)
      where
        updateExchangeM :: EM.Request -> IO EM.Response
        updateExchangeM = atomicModifyIORef exchange . E.update


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

-- Parse requests.
instance FromJSON EM.Request where
  parseJSON = withObject "Exchange.Messages.Request" $ \req ->
    case HM.lookup "type" req of
      Nothing -> typeMismatch "type" (Object req)

      -- { type     : "LIMIT"
      -- , tid      : String
      -- , ticker   : String
      -- , quantity : Int
      -- , price    : Float
      -- , dir      : String
      -- }
      Just "LIMIT" -> EM.Limit
        <$> req .: "tid"
        <*> req .: "ticker"
        <*> req .: "quantity"
        <*> req .: "price"
        <*> req .: "dir"

      -- { type     : "MARKET"
      -- , tid      : String
      -- , ticker   : String
      -- , quantity : Int
      -- , dir      : String
      -- }
      Just "MARKET" -> EM.Market
        <$> req .: "tid"
        <*> req .: "ticker"
        <*> req .: "quantity"
        <*> req .: "dir"

      -- { type   : "REGISTERS"
      -- , ticker : String
      -- }
      Just "REGISTERS" -> EM.RegisterS <$> req .: "ticker"

      -- { type   : "REGISTERT"
      -- , trader : String
      -- }
      Just "REGISTERT" -> EM.RegisterT <$> req .: "trader"

      -- { type   : "STATUS"
      -- }
      Just "STATUS" -> return EM.Status

      -- { type     : "CANCEL"
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
  let reqs = filter (\(x, _) -> x == "request") params
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
