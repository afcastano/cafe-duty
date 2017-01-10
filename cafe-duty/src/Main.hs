module Main where
import Web.Scotty

import App.Api

main :: IO ()
main = scotty 3000 webApi
