## 0.5.0

* Remove surrounding double quotes from cookie values when parsing [#31](https://github.com/snoyberg/cookie/pull/31)

  This is a breaking change, as it changes the behavior of `parseCookies` and `parseSetCookie` to no
  longer include the surrounding double quotes in the cookie value. This is the correct behavior
  according to the RFC.

## 0.4.6

* Resolve redundant import of Data.Monoid [#26](https://github.com/snoyberg/cookie/pull/26)
* Added `renderSetCookieBS` and `renderCookiesBS`

## 0.4.5

* Added `SameSite=None`

## 0.4.4

* Dropped dependency on blaze-builder
* Made cookie text rendering slightly more efficient

## 0.4.3

* Added `defaultSetCookie` [#16](https://github.com/snoyberg/cookie/pull/16)

## 0.4.2.1

* Clarified MIT license

## 0.4.2

* Added SameSite [#13](https://github.com/snoyberg/cookie/pull/13)
