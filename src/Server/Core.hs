--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Server.Core
  where

import Data.ByteString
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
--------------------------------------------------------------------------------

-- | Routes requests to the server correspondingly.
app :: Application
app request respond = case pathInfo request of
  -- "/": load the index page
  []          -> respond indexPage

  -- "/res/*": load a resource
  ("res":res) -> let req = request { pathInfo = res }
                 in  fileServer req respond 

  -- load a 404 for anything else
  _           -> respond notFound

-- | Serve static files (e.g. html/css/js) needed by the server.
fileServer :: Application
fileServer = staticApp (defaultFileServerSettings "res/")

indexPage :: Response
indexPage = responseFile
  status200
  [("Content-Type", "text/html")]
  "res/index.html"
  Nothing

notFound :: Response
notFound = responseLBS
  status404
  [("Content-Type", "text/plain")]
  "404 - Page not found."

main :: IO ()
main = run 8080 app
