module App.ErrorPageService (getErrorPage) where

import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy

---- UI specific stuff.
--- new team
getErrorPage :: String -> IO Text
getErrorPage msg = do
                let context "errorMsg" = MuVariable msg
                useTemplate "templates/Error.html" context


-- Consider extracting to helper
useTemplate :: String -> (String -> MuType IO) -> IO Text
useTemplate templateName context = hastacheFile defaultConfig templateName (mkStrContext context)
