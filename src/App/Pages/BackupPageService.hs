module App.Pages.BackupPageService (getBackupPage) where

import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy

---- UI specific stuff.
getBackupPage :: String -> IO Text
getBackupPage kind = do
                let context "kind" = MuVariable $ kind
                useTemplate "templates/backup.html" context


-- Consider extracting to helper
useTemplate :: String -> (String -> MuType IO) -> IO Text
useTemplate templateName context = hastacheFile defaultConfig templateName (mkStrContext context)
