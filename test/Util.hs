
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
import Test.HUnit

import qualified Network.DNS.Public as P
import Web.CookieJar
import Web.CookieJar.Types

import Public

timeBounds :: (Int, Int)
timeBounds = (0, 60 * 60 * 24 * 8)

time :: Array Int UTCTime
time =
  listArray timeBounds
  $ map (posixSecondsToUTCTime . fromIntegral) [fst timeBounds .. snd timeBounds]

host1, host2 :: CI Bytes
host1 = "host1.example"
host2 = "host2.example"
host1a = "a.host1.example"
host1b = "b.host1.example"
host2a = "a.host2.example"
host2b = "b.host2.example"
pubHost = "example"

path1, path2 :: Bytes
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

dump :: (MonadState Jar m, MonadIO m) => m ()
dump = get >>= liftIO . print . jarCookies

recv :: Time -> Endpoint -> String -> Session ()
recv now ep str = do
    liftIO $ do
      putStr "recv "
      putStrLn str
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
    putStr "send "
    putStrLn expect
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
  liftIO $ do
    putStr "noSend"
  jar <- get
  let (hs, jar') = sendHeaders now jar ep
  liftIO $ assertEqual "Cookies sent" [] hs
  put jar'
  dump

