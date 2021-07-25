{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Tripwire.Server where

import Relude
import Control.Lens
import Tripwire.Config
import Tripwire.TH
import Tripwire.Util

import Network.RCON.Minecraft

import UnliftIO

import Network.HTTP.Req

import System.IO (hGetLine)
import System.Process
import System.Directory

import qualified Data.Text as T
import Control.Concurrent (forkIO)

downloadServer :: Url s -> IO LByteString
downloadServer serverUrl = do
    resp <- runReq defaultHttpConfig $ req GET serverUrl NoReqBody lbsResponse mempty
    case responseStatusCode resp of
        200 -> pure (responseBody resp)
        code -> fail $ "Server download failed with status: " <> show code <> " " <> decodeUtf8 (responseStatusMessage resp)


downloadAndInstallServer :: Url s -> TripwireM ()
downloadAndInstallServer serverUrl = do
    view tripwireHome >>= fmap (filter (`notElem`[".", ".."])) . (liftIO . getDirectoryContents) >>= traverse (liftIO . removePathForcibly)
    serverJar <- liftIO $ downloadServer serverUrl
    writeFileLBS "server.jar" serverJar
    writeFile "eula.txt" "eula=true"
    writeFile "server.properties" [file|server.properties|]

runWithServer :: Rcon a -> TripwireM a
runWithServer m = do
    javaMemoryBytes     <- view tripwireJavaMemoryBytes
    timeoutMicroseconds <- view tripwireTimeoutMicroseconds
    liftIO $ withCreateProcess (proc "java" ["-jar", "-Xmx" <> show javaMemoryBytes, "server.jar", "-nogui"]){std_out=CreatePipe}
        \_ (Just sout) _ _ ->
        bracket_
            (do
                repeatUntil ("RCON running on"`T.isInfixOf`) (toText <$> hGetLine sout)
                forkIO $ forever $ tryIO (hGetLine sout) >> pure () -- necessary hack to drain 'sout'. If we didn't do this,
                                                                    -- 'sout's buffer would fill up and the server would hang.
                copyFileOrDirectory False "world" "worldBACKUP")
            (do
                removePathForcibly "world"
                copyFileOrDirectory False "worldBACKUP" "world")
            (runRcon (ServerInfo{serverHost="localhost", serverPort=25575, serverPassword="tripwire"}) timeoutMicroseconds m)



