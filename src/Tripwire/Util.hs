module Tripwire.Util where

import Relude
import Control.Lens

import System.Directory
import System.FilePath

lensID :: (s -> Identity a) -> (s -> Identity b -> t) -> Lens s t a b
lensID getter setter = lens (runIdentity . getter) (\s x -> setter s (Identity x))

repeatUntil :: Monad m => (a -> Bool) -> m a -> m ()
repeatUntil done a = go
    where
        go = unlessM (done <$> a) go

copyFileToDir :: FilePath -> FilePath -> IO ()
copyFileToDir file dir = copyFile file (dir </> takeFileName file)

copyFileOrDirectory :: (MonadIO m) => Bool -> FilePath -> FilePath -> m ()
copyFileOrDirectory parents from to = liftIO $
    doesFileExist from >>= \case
        True -> copyFile from to
        False -> doesDirectoryExist from >>= \case
            True -> do
                createDirectoryIfMissing parents to
                files <- filter (`notElem` ["..", "."]) <$> getDirectoryContents from
                forM_ files $ \file -> copyFileOrDirectory parents (from </> file) (to </> file)
            False -> fail $ "copyFileOrDirectory: File does not exist: " <> from

