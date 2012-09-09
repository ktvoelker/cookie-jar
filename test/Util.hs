
module Util where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.CaseInsensitive (CI)
import Data.Time
import Data.Time.Clock.POSIX
import System.Exit
import System.IO (hPutStr, hPutStrLn, stderr)
import Test.HUnit

import qualified Network.DNS.Public as P
import Web.CookieJar
import Web.CookieJar.Types

import Public

time :: Int -> UTCTime
time = posixSecondsToUTCTime . fromIntegral

second, minute, hour, day, week, year, year2000 :: Int
second = 1
minute = 60
hour = 60 * minute
day = 24 * hour
week = 7 * day
year = 365 * day
year2000 = 946684800

host1, host2, host1a, host1b, host2a, host2b, pubHost :: CI Bytes
host1 = "host1.example"
host2 = "host2.example"
host1a = "a.host1.example"
host1b = "b.host1.example"
host2a = "a.host2.example"
host2b = "b.host2.example"
pubHost = "example"

path1, path2, path1a, path1b, path2a, path2b :: Bytes
path1 = "/path1"
path2 = "/path2"
path1a = "/path1/a"
path1b = "/path1/b"
path2a = "/path2/a"
path2b = "/path2/b"

ep = Endpoint

type Session a = StateT Jar IO a

runSession :: P.Rules -> Session a -> IO a
runSession = flip evalStateT . emptyJar . Just

sessionTest :: String -> Session () -> P.Rules -> Test
sessionTest n s r = TestLabel n $ TestCase $ runSession r s

enableDebug = False

debug = if enableDebug then hPutStr stderr else const $ return ()

debugLn = if enableDebug then hPutStrLn stderr else const $ return ()

dump :: (MonadState Jar m, MonadIO m) => m ()
dump = get >>= liftIO . debugLn . show . jarCookies

recv :: Time -> Endpoint -> String -> Session ()
recv now ep str = do
    liftIO $ do
      debug "recv "
      debugLn str
    ( modify
      . receiveHeaders now ep
      . (: []) . ("Set-Cookie",)
      . E.encodeUtf8
      . T.pack
      $ str )
    dump

send :: Time -> Endpoint -> String -> Session ()
send now ep expect = do
  liftIO $ do
    debug "send "
    debugLn expect
  jar <- get
  let (hs, jar') = sendHeaders now jar ep
  liftIO $ case hs of
    [("Cookie", actual)] ->
      assertEqual "Cookie data sent" (E.encodeUtf8 $ T.pack expect) actual
    _ -> assertFailure "Did not send exactly one header named Cookie"
  put jar'
  dump

noSend :: Time -> Endpoint -> Session ()
noSend now ep = do
  liftIO $ debugLn "noSend"
  jar <- get
  let (hs, jar') = sendHeaders now jar ep
  liftIO $ assertEqual "Cookies sent" [] hs
  put jar'
  dump

