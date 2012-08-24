
module Web.CookieJar
  ( Jar()
  , getCookies
  ) where

import qualified Data.ByteString as BS

import Data.Maybe
import Network.HTTP.Types

-- TODO
type Cookie = ()

newtype Jar = Jar { getCookies :: [Cookie] }

-- TODO
type Endpoint = ()

-- TODO
type SetCookie = ()

parseSetCookie :: BS.ByteString -> Maybe SetCookie
parseSetCookie bs = undefined

receive :: Endpoint -> SetCookie -> Jar -> Jar
receive = undefined

send :: Jar -> Endpoint -> [Cookie]
send = undefined

receiveHeaders :: Endpoint -> ResponseHeaders -> Jar -> Jar
receiveHeaders host =
  flip (foldr $ receive host)
  . catMaybes
  . map (parseSetCookie . snd)
  . filter ((== "Set-Cookie") . fst)

sendHeaders :: Jar -> Endpoint -> RequestHeaders
sendHeaders = undefined

