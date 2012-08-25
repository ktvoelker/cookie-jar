
module Web.CookieJar
  ( Jar()
  , getCookies
  ) where

import qualified Data.ByteString as BS

import Data.Maybe
import Data.Time
import Network.HTTP.Types

type Time = UTCTime

type Bytes = BS.ByteString

-- TODO
type Cookie = ()

newtype Jar = Jar { getCookies :: [Cookie] }

data Endpoint =
  Endpoint
  { epDomain :: Bytes
  , epPath   :: Bytes
  } deriving (Show)

data SetCookie =
  SetCookie
  { scName     :: Bytes
  , scValue    :: Bytes
  , scDomain   :: Maybe Bytes
  , scPath     :: Maybe Bytes
  , scSecure   :: Maybe Bool
  , scHttpOnly :: Maybe Bool
  , scExpires  :: Maybe Time
  , scMaxAge   :: Maybe Integer
  }

parseSetCookie :: Bytes -> Maybe SetCookie
parseSetCookie bs = undefined

receive :: Time -> Endpoint -> SetCookie -> Jar -> Jar
receive = undefined

send :: Time -> Jar -> Endpoint -> [Cookie]
send = undefined

receiveHeaders :: Time -> Endpoint -> ResponseHeaders -> Jar -> Jar
receiveHeaders time host =
  flip (foldr $ receive time host)
  . catMaybes
  . map (parseSetCookie . snd)
  . filter ((== "Set-Cookie") . fst)

sendHeaders :: Time -> Jar -> Endpoint -> RequestHeaders
sendHeaders = undefined

