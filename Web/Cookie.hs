module Web.Cookie
    ( -- * Server to client
      {-SetCookie (..)
    , parseSetCookie
    , renderSetCookie
      -- * Client to server
    , -}Cookies
    , parseCookies
    , renderCookies
    ) where

import qualified Data.ByteString as S
import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.Monoid (mempty, mappend)
import Data.Word (Word8)

type Cookies = [(S.ByteString, S.ByteString)]

-- | Decode the value of an HTTP_COOKIE header into key/value pairs.
parseCookies :: S.ByteString -> Cookies
parseCookies s
  | S.null s = []
  | otherwise =
    let (first, rest) = breakDiscard 59 s -- semicolon
     in parseCookie first : parseCookies rest

parseCookie :: S.ByteString -> (S.ByteString, S.ByteString)
parseCookie s =
    let (key, value) = breakDiscard 61 s -- equals sign
        key' = S.dropWhile (== 32) key -- space
     in (key', value)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

renderCookies :: Cookies -> Builder
renderCookies [] = mempty
renderCookies cs =
    foldr1 go $ map renderCookie cs
  where
    go x y = x `mappend` fromChar ';' `mappend` y

renderCookie :: (S.ByteString, S.ByteString) -> Builder
renderCookie (k, v) =
    fromByteString k `mappend` fromChar '=' `mappend` fromByteString v
