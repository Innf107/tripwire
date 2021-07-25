module Main where

import Relude
import Control.Lens

import Options.Applicative

import Tripwire
import Network.HTTP.Req

import System.Directory
import System.FilePath

main :: IO ()
main = optParser >>= \p -> execParser (info (p <**> helper) desc) >>= postProcessConfig >>= flip runTripwireM tripwire

desc :: InfoMod Config
desc = fullDesc

optParser :: IO (Parser (Config))
optParser = do
    home <- getHomeDirectory
    pure $ config
            <$> strOption (long "home" <> metavar "DIR" <> value (home </> ".tripwire"))
            <*> option (InstallServer . http <$> str) (long "server" <> metavar "URL" <> value (InstallServer defaultServerURL))  --TODO: Replace 'http' with actual url parser
            <*> option auto (long "java-Xmx" <> metavar "BYTES" <> value 4e9)
            <*> option (Just <$> auto) (long "setup" <> metavar "FUNCTION" <> value Nothing)
            <*> switch (long "require-input")
            <*> option auto (long "iterations" <> metavar "COUNT" <> value 20)
            <*> option auto (long "timeout" <> metavar "MICROSECONDS" <> value 30e6)
            <*> strArgument (metavar "TARGET")
            <*> strArgument (metavar "RUNFUNCTION")

postProcessConfig :: Config -> IO Config
postProcessConfig = traverseOf tripwireTarget makeAbsolute

