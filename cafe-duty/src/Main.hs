module Main where
import Web.Scotty (scotty, middleware)
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))

import App.Api (webApi)

main :: IO ()
main = scotty 3000 $ do 
  middleware $ staticPolicy (noDots >-> addBase "static") -- set up the static folder
  webApi
