module Main where
import Web.Scotty
import Network.Wai.Middleware.Static

import App.Api

main :: IO ()
main = scotty 3000 $ do 
  middleware $ staticPolicy (noDots >-> addBase "static")
  webApi
