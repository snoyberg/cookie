import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Web.Cookie
import Blaze.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import Control.Arrow ((***))

main :: IO ()
main = defaultMain
    [ testProperty "parse/render cookies" propParseRenderCookies
    ]

propParseRenderCookies :: Cookies' -> Bool
propParseRenderCookies cs' =
    parseCookies (builderToBs $ renderCookies cs) == cs
  where
    cs = map (go *** go) cs'
    go = S.pack . map unChar'

builderToBs :: Builder -> S.ByteString
builderToBs = S.concat . L.toChunks . toLazyByteString

type Cookies' = [([Char'], [Char'])]
newtype Char' = Char' { unChar' :: Word8 }
instance Show Char' where
    show (Char' w) = [toEnum $ fromEnum w]
    showList = (++) . show . concatMap show
instance Arbitrary Char' where
    arbitrary = fmap (Char' . toEnum) $ choose (62, 125)
