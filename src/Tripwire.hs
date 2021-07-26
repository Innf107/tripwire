module Tripwire (
    tripwire

,   setupServer
,   defaultServerURL
,   runTripwireM

,   ConfigF (ConfigF)
,   Config
,   config
,   sequenceConfig
,   tripwireHome
,   tripwireHomeF
,   tripwireServer
,   tripwireServerF
,   tripwireJavaMemoryBytes
,   tripwireJavaMemoryBytesF
,   tripwireTarget
,   tripwireTargetF

,   ServerState (..)

) where

import Relude
import Control.Lens
import qualified Data.Maybe as M
import Tripwire.Config
import Tripwire.Server
import Tripwire.Prompt
import Tripwire.Util
import Network.HTTP.Req
import System.Directory
import System.FilePath
import System.Clock

import Data.List (minimum, maximum)
import qualified Data.Text as T

import UnliftIO

import Text.URI

import Network.RCON.Minecraft

import Control.Concurrent (threadDelay)

defaultServerURL :: Url Http
defaultServerURL = fst . M.fromJust $ useHttpURI =<< mkURI "http://launcher.mojang.com/v1/objects/a16d67e5807f57fc4e550299cf20226194497dc2/server.jar"

setupServer :: TripwireM ()
setupServer = view tripwireServer >>= \case
    Installed _ -> pass
    InstallServer url -> downloadAndInstallServer url

runTripwireM :: Config -> TripwireM a -> IO a
runTripwireM c m = do
    oldCWD <- getCurrentDirectory
    unlessM (doesDirectoryExist (view tripwireHome c)) do
        createHomeDir <- promptBool ("Tripwire Home directory '" <> toText (view tripwireHome c) <> "' does not exist. Do you want to create it and its parents?")
        if createHomeDir
        then createDirectoryIfMissing True (view tripwireHome c)
        else putStrLn "Tripwire cannot work without a home directory.\nYou can supply a custom home directory with --home.\nExiting" >> exitFailure
    setCurrentDirectory (view tripwireHome c)
    runReaderT (unTripwire (setupServer *> m)) c <* setCurrentDirectory oldCWD

tripwire :: TripwireM ()
tripwire = do
    target <- view tripwireTarget
    home <- view tripwireHome
    log $ "Loading datapack '" <> toText target <> "'"
    liftIO $ createDirectoryIfMissing True "world/datapacks"
    liftIO $ copyFileToDir target "world/datapacks/"
    log $ "Waiting for the server to start... (See '" <> toText (home </> "logs/latest.log") <> "' for logs)"
    withRunInIO \run -> run $ runWithServer do
        let liftT :: TripwireM a -> Rcon a
            liftT = liftIO . run
        liftT $ log "Server ready!"

        void $ sendCommand $ "gamerule maxCommandChainLength 2147483647"

        whenJustM (liftT $ view tripwireSetupFunction) \setup -> do
            liftT $ log "Running setup..."
            void $ sendCommand $ "function " <> setup

        whenM (liftT $ view tripwireRequireInput) do
            putStrLn "Press Enter to start"
            void getLine

        liftT $ log "Running timing..."

        runFunction <- liftT $ view tripwireRunFunction

        iterations <- liftT $ view tripwireIterations

        times <- catMaybes <$> forM [1..iterations] \i -> do
            startTime <- liftIO $ getTime Monotonic
            (sendCommand ("function " <> runFunction) >>= \case
                Nothing -> do
                    liftT $ log $ "[" <> show i <> "]: TIMEOUT!"
                    pure Nothing
                Just _ -> do
                    endTime <- liftIO $ getTime Monotonic
                    let time = diffTimeSpec startTime endTime
                    liftT $ log $ "[" <> show i <> "]: " <> displayTime time
                    pure $ Just time)
                <* liftIO (threadDelay 0.5e6)


        putTextLn "\n"
        putTextLn $ "Average: " <> displayTime (fromNanoSecs $ averageInt (map toNanoSecs times))
        putTextLn $ "Min: " <> displayTime (minimum times)
        putTextLn $ "Max: " <> displayTime (maximum times)


displayTime :: TimeSpec -> Text
displayTime (TimeSpec secs nsecs) = show secs <> "." <> T.justifyRight 9 '0' (show nsecs) <> "s"





