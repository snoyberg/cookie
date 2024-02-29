import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit ((@=?), Assertion)

import Web.Cookie
import Data.ByteString.Builder (Builder, word8, toLazyByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import Data.Monoid (mconcat)
import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>))
import Data.Time (UTCTime (UTCTime), toGregorian)
import qualified Data.Text as T

main :: IO ()
main = defaultMain $ testGroup "cookie"
    [ testProperty "parse/render cookies" propParseRenderCookies
    , testProperty "parse/render SetCookie" propParseRenderSetCookie
    , testProperty "parse/render cookies text" propParseRenderCookiesText
    , testCase "parseCookies" caseParseCookies
    , testCase "parseQuotedCookies" caseParseQuotedCookies
    , testCase "parseQuotedSetCookie" caseParseQuotedSetCookie
    , twoDigit 24 2024
    , twoDigit 69 2069
    , twoDigit 70 1970
    ]

propParseRenderCookies :: Cookies' -> Bool
propParseRenderCookies cs' =
    parseCookies (builderToBs $ renderCookies cs) == cs
  where
    cs = map (fromUnChars *** fromUnChars) cs'

propParseRenderCookiesText :: Cookies' -> Bool
propParseRenderCookiesText cs' =
    parseCookiesText (builderToBs $ renderCookiesText cs) == cs
  where
    cs = map (T.pack . map unChar'' *** T.pack . map unChar'') cs'
    unChar'' = toEnum . fromEnum . unChar'

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
newtype SameSiteOption' = SameSiteOption' { unSameSiteOption' :: SameSiteOption }
instance Arbitrary SameSiteOption' where
  arbitrary = fmap SameSiteOption' (elements [sameSiteLax, sameSiteStrict, sameSiteNone])

propParseRenderSetCookie :: SetCookie -> Bool
propParseRenderSetCookie sc =
    parseSetCookie (builderToBs $ renderSetCookie sc) == sc

instance Arbitrary SetCookie where
    arbitrary = do
        name <- fmap fromUnChars arbitrary
        value <- fmap fromUnChars arbitrary
        path <- fmap (fmap fromUnChars) arbitrary
        expires <- fmap (parseCookieExpires . formatCookieExpires)
                    (UTCTime <$> fmap toEnum arbitrary <*> return 0)
        domain <- fmap (fmap fromUnChars) arbitrary
        httponly <- arbitrary
        secure <- arbitrary
        sameSite <- fmap (fmap unSameSiteOption') arbitrary
        return def
            { setCookieName = name
            , setCookieValue = value
            , setCookiePath = path
            , setCookieExpires = expires
            , setCookieDomain = domain
            , setCookieHttpOnly = httponly
            , setCookieSecure = secure
            , setCookieSameSite = sameSite
            }

caseParseCookies :: Assertion
caseParseCookies = do
    let input = S8.pack "a=a1;b=b2; c=c3"
        expected = [("a", "a1"), ("b", "b2"), ("c", "c3")]
    map (S8.pack *** S8.pack) expected @=? parseCookies input

-- Tests for two digit years, see:
--
-- https://github.com/snoyberg/cookie/issues/5
twoDigit x y =
    testCase ("year " ++ show x) (y @=? year)
  where
    (year, _, _) = toGregorian day
    day =
        case setCookieExpires sc of
            Just (UTCTime day _) -> day
            Nothing -> error $ "setCookieExpires == Nothing for: " ++ show str
    sc = parseSetCookie str
    str = S8.pack $ concat
        [ "foo=bar; Expires=Mon, 29-Jul-"
        , show x
        , " 04:52:08 GMT"
        ]

caseParseQuotedCookies :: Assertion
caseParseQuotedCookies = do
    let input = S8.pack "a=\"a1\";b=\"b2\"; c=\"c3\""
        expected = [("a", "a1"), ("b", "b2"), ("c", "c3")]
    map (S8.pack *** S8.pack) expected @=? parseCookies input

caseParseQuotedSetCookie :: Assertion
caseParseQuotedSetCookie = do
    let input = S8.pack "a=\"a1\""
        result = parseSetCookie input
        resultNameAndValue = (setCookieName result, setCookieValue result)
        expected = (S8.pack "a", S8.pack "a1")
    expected @=? resultNameAndValue
