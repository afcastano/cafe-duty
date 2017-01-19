{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty (scotty, middleware)
import Data.ByteString
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))
import Network.Wai.Middleware.StripHeaders (stripHeader)
import Network.Wai (Middleware, modifyResponse)

import App.Api (webApi)

main :: IO ()
main = scotty 3000 $ do 
  middleware $ removeHeader "X-FRAME-OPTIONS"
  middleware $ staticPolicy (noDots >-> addBase "static") -- set up the static folder
  webApi

removeHeader :: ByteString -> Middleware
removeHeader h = modifyResponse $ stripHeader h