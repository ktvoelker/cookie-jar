
module Web.CookieJar.Types
  ( Word8
  , module Data.CaseInsensitive
  , module Network.HTTP.Types
  , module Web.CookieJar.Types
  ) where

import qualified Data.ByteString as BS

import Data.CaseInsensitive (CI(), foldedCase, mk, original)
import Data.Time
import Data.Word (Word8)
import Network.HTTP.Types

import qualified Network.DNS.Public as P

type Time = UTCTime

type Bytes = BS.ByteString

-- |A cookie as stored by a user-agent (see section 5.3 of the RFC)
data Cookie =
  Cookie
  { cName     :: Bytes       -- ^The /name/ field
  , cValue    :: Bytes       -- ^The /value/ field
  , cExpires  :: Maybe Time  -- ^The /expiry-time/ field
  , cDomain   :: CI Bytes    -- ^The /domain/ field
  , cPath     :: Bytes       -- ^The /path/ field
  , cCreation :: Time        -- ^The /creation-time/ field
  , cAccess   :: Time        -- ^The /last-access-time/ field
  , cPersist  :: Bool        -- ^The /persistent-flag/ field
  , cHostOnly :: Bool        -- ^The /host-only-flag/ field
  , cSecure   :: Bool        -- ^The /secure-only-flag/ field
  , cHttpOnly :: Bool        -- ^The /http-only-flag/ field
  } deriving (Eq, Show)

-- |The state of a user-agent
data Jar =
  Jar
  { jarRules   :: P.Rules
  , jarCookies :: [Cookie]
  } deriving (Eq, Show)

-- |The initial state of a user-agent
emptyJar
  :: Maybe P.Rules  -- ^The rules for identifying public DNS suffixes
  -> Jar
emptyJar = flip Jar [] . maybe (P.parseRules "") id

modifyCookies :: ([Cookie] -> [Cookie]) -> Jar -> Jar
modifyCookies f jar = jar { jarCookies = f $ jarCookies jar }

-- |An external entity which can send Set-Cookie requests to a user-agent
data Endpoint =
  Endpoint
  { epDomain :: CI Bytes  -- ^The domain name
  , epPath   :: Bytes     -- ^The path
  , epHttp   :: Bool      -- ^True iff the endpoint uses HTTP
  , epSecure :: Bool      -- ^True iff communication with the endpoint is secure
  } deriving (Show)

-- |A request to set a cookie, usually the result of parsing a Set-Cookie
-- header
--
-- This type is based on the output produced by the header parsing
-- algorithm described in section 5.2 of the RFC, but represented in
-- a style more suited to Haskell. In particular, the RFC describes the
-- algorithm as appending parsed attributes to a list in the order they are
-- encountered; in the processing of these attributes (section 5.3), only
-- the last attribute with a particular name is ever considered.
--
-- The approach taken here is thus equivalent: if no attribute with
-- a particular name is encountered in the header, the corresponding field
-- in this record should be Nothing; if one or more attributes with
-- a particular name are encountered, the corresponding field in this
-- record should be 'Just' the value from the last occurrence.
--
-- Also, note that for the Boolean attributes, there is no need to
-- separately track the presence of the attribute with a 'Maybe' layer,
-- because presence makes the attribute 'True', whereas by default it is
-- 'False'.
data SetCookie =
  SetCookie
  { scName     :: Bytes             -- ^The /cookie-name/
  , scValue    :: Bytes             -- ^The /cookie-value/
  , scDomain   :: Maybe (CI Bytes)  -- ^The /domain/ attribute
  , scPath     :: Maybe Bytes       -- ^The /path/ attribute
  , scSecure   :: Bool              -- ^The /secure/ attribute
  , scHttpOnly :: Bool              -- ^The /http-only/ attribute
  , scExpires  :: Maybe Time        -- ^The /expires/ attribute
  , scMaxAge   :: Maybe Integer     -- ^The /max-age/ attribute
  } deriving (Eq, Show)

-- |A set-cookie request with a name and value but no attributes
emptySetCookie
  :: Bytes      -- ^The /cookie-name/
  -> Bytes      -- ^The /cookie-value/
  -> SetCookie
emptySetCookie n v = SetCookie n v Nothing Nothing False False Nothing Nothing

