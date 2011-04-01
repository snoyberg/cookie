{-# LANGUAGE OverloadedStrings #-}
module Web.Cookie
    ( -- * Server to client
      SetCookie (..)
    , parseSetCookie
    , renderSetCookie
      -- * Client to server
    , Cookies
    , parseCookies
    , renderCookies
      -- ** UTF8 Version
    , CookiesText
    , parseCookiesText
    , renderCookiesText
      -- * Expires field
    , expiresFormat
    , formatCookieExpires
    , parseCookieExpires
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char (toLower)
import Blaze.ByteString.Builder (Builder, fromByteString, copyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.Monoid (mempty, mappend, mconcat)
import Data.Word (Word8)
import Data.Time (UTCTime, formatTime, parseTime)
import System.Locale (defaultTimeLocale)
import Control.Arrow (first)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Control.Arrow ((***))

-- | Textual cookies. Functions assume UTF8 encoding.
type CookiesText = [(Text, Text)]

parseCookiesText :: S.ByteString -> CookiesText
parseCookiesText =
    map (go *** go) . parseCookies
  where
    go = decodeUtf8With lenientDecode

-- FIXME to speed things up, skip encodeUtf8 and use fromText instead
renderCookiesText :: CookiesText -> Builder
renderCookiesText = renderCookies . map (encodeUtf8 *** encodeUtf8)

type Cookies = [(S.ByteString, S.ByteString)]

-- | Decode the value of a \"Cookie\" request header into key/value pairs.
parseCookies :: S.ByteString -> Cookies
parseCookies s
  | S.null s = []
  | otherwise =
    let (x, y) = breakDiscard 59 s -- semicolon
     in parseCookie x : parseCookies y

parseCookie :: S.ByteString -> (S.ByteString, S.ByteString)
parseCookie s =
    let (key, value) = breakDiscard 61 s -- equals sign
        key' = S.dropWhile (== 32) key -- space
     in (key', value)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.breakByte w s
     in (x, S.drop 1 y)

renderCookies :: Cookies -> Builder
renderCookies [] = mempty
renderCookies cs =
    foldr1 go $ map renderCookie cs
  where
    go x y = x `mappend` fromChar ';' `mappend` y

renderCookie :: (S.ByteString, S.ByteString) -> Builder
renderCookie (k, v) = fromByteString k `mappend` fromChar '='
                                       `mappend` fromByteString v

data SetCookie = SetCookie
    { setCookieName :: S.ByteString
    , setCookieValue :: S.ByteString
    , setCookiePath :: Maybe S.ByteString
    , setCookieExpires :: Maybe UTCTime
    , setCookieDomain :: Maybe S.ByteString
    }
    deriving (Eq, Show, Read)

renderSetCookie :: SetCookie -> Builder
renderSetCookie sc = mconcat
    [ fromByteString (setCookieName sc)
    , fromChar '='
    , fromByteString (setCookieValue sc)
    , case setCookiePath sc of
        Nothing -> mempty
        Just path -> copyByteString "; path="
                     `mappend` fromByteString path
    , case setCookieExpires sc of
        Nothing -> mempty
        Just e -> copyByteString "; expires=" `mappend`
                  fromByteString (formatCookieExpires e)
    , case setCookieDomain sc of
        Nothing -> mempty
        Just d -> copyByteString "; domain=" `mappend`
                  fromByteString d
    ]

parseSetCookie :: S.ByteString -> SetCookie
parseSetCookie a = SetCookie
    { setCookieName = key
    , setCookieValue = value
    , setCookiePath = lookup "path" pairs
    , setCookieExpires =
        lookup "expires" pairs >>= parseCookieExpires
    , setCookieDomain = lookup "domain" pairs
    }
  where
    (key, value, b) = parsePair a
    pairs = map (first $ S8.map toLower) $ parsePairs b
    parsePair bs =
        let (k, bs') = breakDiscard 61 bs -- equals sign
            (v, bs'') = breakDiscard 59 bs' -- semicolon
         in (k, v, S.dropWhile (== 32) bs'') -- space
    parsePairs bs =
        if S.null bs
            then []
            else let (k, v, bs') = parsePair bs
                  in (k, v) : parsePairs bs'

expiresFormat :: String
expiresFormat = "%a, %d-%b-%Y %X GMT"

-- | Format a 'UTCTime' for a cookie.
formatCookieExpires :: UTCTime -> S.ByteString
formatCookieExpires =
    S8.pack . formatTime defaultTimeLocale expiresFormat

parseCookieExpires :: S.ByteString -> Maybe UTCTime
parseCookieExpires = parseTime defaultTimeLocale expiresFormat . S8.unpack
