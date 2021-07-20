module Tripwire.Prompt where

import Relude

import System.IO hiding (getLine)

import qualified Data.Text as T

promptBool :: Text -> IO Bool
promptBool p = do
    putText $ p <> " ([Y]es | [N]o) "
    hFlush stdout
    resp <- T.toCaseFold <$> getLine
    if  | resp `elem` map T.toCaseFold ["Y", "Yes"] -> pure True
        | resp `elem` map T.toCaseFold ["N", "No"] -> pure False
        | otherwise -> promptBool p