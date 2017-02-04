module App.Helper.Strings (eqTrimIgnCase, toLowercase, trim, isEmpty) where
import qualified Data.Text as T
import Data.Char

trim :: String -> String
trim = T.unpack . T.strip . T.pack

toLowercase :: String -> String
toLowercase str = map toLower str

eqTrimIgnCase :: String -> String -> Bool
eqTrimIgnCase s1 s2 = let ns1 = (trim.toLowercase) s1
                          ns2 = (trim.toLowercase) s2
                      in ns1 == ns2

isEmpty :: String -> Bool
isEmpty s = all isSpace s


