
module Web.CookieJar.Types
  ( Word8
  , module Data.CaseInsensitive
  , module Network.HTTP.Types
  , module Web.CookieJar.Types
  ) where

import qualified Data.ByteString as BS

import Data.CaseInsensitive (CI(), foldedCase, mk)
import Data.Time
import Data.Word (Word8)
import Network.HTTP.Types

type Time = UTCTime

type Bytes = BS.ByteString

data Cookie =
  Cookie
  { cName     :: Bytes
  , cValue    :: Bytes
  , cExpires  :: Maybe Time
  , cDomain   :: Maybe (CI Bytes)
  , cPath     :: Maybe Bytes
  , cCreation :: Time
  , cAccess   :: Time
  , cPersist  :: Bool
  , cHostOnly :: Bool
  , cSecure   :: Bool
  , cHttpOnly :: Bool
  } deriving (Show)

newtype Jar = Jar { getCookies :: [Cookie] }

data Endpoint =
  Endpoint
  { epDomain :: CI Bytes
  , epPath   :: Bytes
  , epHttp   :: Bool
  , epSecure :: Bool
  } deriving (Show)

data SetCookiePath = DefaultPath | Path Bytes deriving (Show)

data SetCookie =
  SetCookie
  { scName     :: Bytes
  , scValue    :: Bytes
  , scDomain   :: Maybe (CI Bytes)
  , scPath     :: Maybe SetCookiePath
  , scSecure   :: Bool
  , scHttpOnly :: Bool
  , scExpires  :: Maybe Time
  , scMaxAge   :: Maybe Integer
  } deriving (Show)

emptySetCookie :: Bytes -> Bytes -> SetCookie
emptySetCookie n v = SetCookie n v Nothing Nothing False False Nothing Nothing

