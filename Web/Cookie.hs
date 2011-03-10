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
      -- * Expires field
    , expiresFormat
    , formatCookieExpires
    , parseCookieExpires
    ) where

import qualified Data.ByteString as S
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.Monoid (mempty, mappend, mconcat)
import Data.Word (Word8)
import Data.Time (UTCTime, formatTime, parseTime)
import System.Locale (defaultTimeLocale)
import Control.Arrow (first)
import qualified Data.Ascii as A

type Cookies = [(A.Ascii, A.Ascii)]

-- | Decode the value of a \"Cookie\" request header into key/value pairs.
parseCookies :: A.Ascii -> Cookies
parseCookies = parseCookiesBS . A.toByteString

parseCookiesBS :: S.ByteString -> Cookies
parseCookiesBS s
  | S.null s = []
  | otherwise =
    let (x, y) = breakDiscard 59 s -- semicolon
     in parseCookie x : parseCookiesBS y

parseCookie :: S.ByteString -> (A.Ascii, A.Ascii)
parseCookie s =
    let (key, value) = breakDiscard 61 s -- equals sign
        key' = S.dropWhile (== 32) key -- space
     in (A.unsafeFromByteString key', A.unsafeFromByteString value)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.breakByte w s
     in (x, S.drop 1 y)

renderCookies :: Cookies -> A.AsciiBuilder
renderCookies [] = mempty
renderCookies cs =
    foldr1 go $ map renderCookie cs
  where
    go x y = x `mappend` A.unsafeFromBuilder (fromChar ';') `mappend` y

renderCookie :: (A.Ascii, A.Ascii) -> A.AsciiBuilder
renderCookie (k, v) =
    A.toAsciiBuilder k `mappend`
    A.unsafeFromBuilder (fromChar '=') `mappend`
    A.toAsciiBuilder v

data SetCookie = SetCookie
    { setCookieName :: A.Ascii
    , setCookieValue :: A.Ascii
    , setCookiePath :: Maybe A.Ascii
    , setCookieExpires :: Maybe UTCTime
    , setCookieDomain :: Maybe A.Ascii
    }
    deriving (Eq, Show, Read)

renderSetCookie :: SetCookie -> A.AsciiBuilder
renderSetCookie sc = mconcat
    [ A.toAsciiBuilder $ setCookieName sc
    , A.unsafeFromBuilder $ fromChar '='
    , A.toAsciiBuilder $ setCookieValue sc
    , case setCookiePath sc of
        Nothing -> mempty
        Just path -> A.toAsciiBuilder "; path="
                     `mappend` A.toAsciiBuilder path
    , case setCookieExpires sc of
        Nothing -> mempty
        Just e -> A.toAsciiBuilder "; expires=" `mappend`
                  A.toAsciiBuilder (formatCookieExpires e)
    , case setCookieDomain sc of
        Nothing -> mempty
        Just d -> A.toAsciiBuilder "; domain=" `mappend`
                  A.toAsciiBuilder d
    ]

parseSetCookie :: A.Ascii -> SetCookie
parseSetCookie a = SetCookie
    { setCookieName = key
    , setCookieValue = value
    , setCookiePath = lookup "path" pairs
    , setCookieExpires =
        lookup "expires" pairs >>= parseCookieExpires
    , setCookieDomain = lookup "domain" pairs
    }
  where
    (key, value, b) = parsePair $ A.toByteString a
    pairs = map (first $ A.toCIAscii) $ parsePairs b
    parsePair bs =
        let (k, bs') = breakDiscard 61 bs -- equals sign
            (v, bs'') = breakDiscard 59 bs' -- semicolon
         in (A.unsafeFromByteString k,
             A.unsafeFromByteString v,
             S.dropWhile (== 32) bs'') -- space
    parsePairs bs =
        if S.null bs
            then []
            else let (k, v, bs') = parsePair bs
                  in (k, v) : parsePairs bs'

expiresFormat :: String
expiresFormat = "%a, %d-%b-%Y %X GMT"

-- | Format a 'UTCTime' for a cookie.
formatCookieExpires :: UTCTime -> A.Ascii
formatCookieExpires =
    A.unsafeFromString . formatTime defaultTimeLocale expiresFormat

parseCookieExpires :: A.Ascii -> Maybe UTCTime
parseCookieExpires = parseTime defaultTimeLocale expiresFormat . A.toString
