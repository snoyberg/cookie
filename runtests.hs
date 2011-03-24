import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit ((@=?), Assertion)

import Web.Cookie
import Blaze.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>))
import Data.Time (UTCTime (UTCTime))

main :: IO ()
main = defaultMain
    [ testProperty "parse/render cookies" propParseRenderCookies
    , testProperty "parse/render SetCookie" propParseRenderSetCookie
    , testCase "parseCookies" caseParseCookies
    ]

propParseRenderCookies :: Cookies' -> Bool
propParseRenderCookies cs' =
    parseCookies (builderToBs $ renderCookies cs) == cs
  where
    cs = map (fromUnChars *** fromUnChars) cs'

fromUnChars :: [Char'] -> S.ByteString
fromUnChars = S.pack . map unChar'

builderToBs :: Builder -> S.ByteString
builderToBs = S.concat . L.toChunks . toLazyByteString

type Cookies' = [([Char'], [Char'])]
newtype Char' = Char' { unChar' :: Word8 }
instance Show Char' where
    show (Char' w) = [toEnum $ fromEnum w]
    showList = (++) . show . concatMap show
instance Arbitrary Char' where
    arbitrary = fmap (Char' . toEnum) $ choose (62, 125)

propParseRenderSetCookie :: SetCookie -> Bool
propParseRenderSetCookie sc =
    parseSetCookie (builderToBs $ renderSetCookie sc) == sc

instance Arbitrary SetCookie where
    arbitrary = SetCookie <$> (fmap fromUnChars arbitrary)
                          <*> (fmap fromUnChars arbitrary)
                          <*> (fmap (fmap fromUnChars) arbitrary)
                          <*> fmap (parseCookieExpires . formatCookieExpires) (UTCTime <$> fmap toEnum arbitrary <*> return 0)
                          <*> (fmap (fmap fromUnChars) arbitrary)

caseParseCookies :: Assertion
caseParseCookies = do
    let input = S8.pack "a=a1;b=b2; c=c3"
        expected = [("a", "a1"), ("b", "b2"), ("c", "c3")]
    map (S8.pack *** S8.pack) expected @=? parseCookies input
