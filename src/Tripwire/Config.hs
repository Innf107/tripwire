{-# LANGUAGE UndecidableInstances, TemplateHaskell #-}
module Tripwire.Config where

import Relude
import Control.Lens
import Tripwire.TypeUtil
import Tripwire.Util
import Data.Aeson
import UnliftIO

import Network.HTTP.Req

import System.IO (hPutStrLn)

data ConfigF f = ConfigF {
    _tripwireHomeF :: f FilePath
,   _tripwireServerF :: f ServerState
,   _tripwireJavaMemoryBytesF :: f Integer
,   _tripwireSetupFunctionF :: f (Maybe Text)
,   _tripwireIterationsF :: f Int
,   _tripwireTargetF :: f FilePath
,   _tripwireRunFunctionF :: f Text
} deriving (Generic)
type ConfigDeps f = FMap f [FilePath, ServerState, Integer, Text, Maybe Text, Int]

deriving instance (AllC Show        (ConfigDeps f)) => Show     (ConfigF f)
deriving instance (AllC Eq          (ConfigDeps f)) => Eq       (ConfigF f)
instance          (AllC ToJSON      (ConfigDeps f)) => ToJSON   (ConfigF f)
instance          (AllC FromJSON    (ConfigDeps f)) => FromJSON (ConfigF f)

type Config = ConfigF Identity
config :: FilePath -> ServerState -> Integer -> Maybe Text -> Int -> FilePath -> Text -> Config
config home server mem setup it target rf =
    ConfigF (Identity home) (Identity server) (Identity mem) (Identity setup) (Identity it) (Identity target) (Identity rf)

data ServerState = InstallServer (Url Http)
                 | Installed FilePath
                 deriving (Show, Eq, Generic)

newtype TripwireM a = TripwireM {unTripwire :: ReaderT Config IO a}
    deriving (Generic, Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadUnliftIO)

makeLenses ''ConfigF

log :: Text -> TripwireM ()
log = liftIO . hPutStrLn stderr . toString


tripwireHome :: Lens' Config FilePath
tripwireHome = lensID _tripwireHomeF (\s x -> s{_tripwireHomeF=x})

tripwireServer :: Lens' Config ServerState
tripwireServer = lensID _tripwireServerF (\s x -> s{_tripwireServerF=x})

tripwireJavaMemoryBytes :: Lens' Config Integer
tripwireJavaMemoryBytes = lensID _tripwireJavaMemoryBytesF (\s x -> s{_tripwireJavaMemoryBytesF=x})

tripwireSetupFunction :: Lens' Config (Maybe Text)
tripwireSetupFunction = lensID _tripwireSetupFunctionF (\s x -> s{_tripwireSetupFunctionF=x})

tripwireIterations :: Lens' Config Int
tripwireIterations = lensID _tripwireIterationsF (\s x -> s{_tripwireIterationsF=x})

tripwireTarget :: Lens' Config FilePath
tripwireTarget = lensID _tripwireTargetF (\s x -> s{_tripwireTargetF=x})

tripwireRunFunction :: Lens' Config Text
tripwireRunFunction = lensID _tripwireRunFunctionF (\s x -> s{_tripwireRunFunctionF=x})



sequenceConfig :: (Applicative f) => ConfigF f -> f Config
sequenceConfig ConfigF{ _tripwireHomeF, _tripwireServerF, _tripwireJavaMemoryBytesF, _tripwireSetupFunctionF
                      , _tripwireIterationsF, _tripwireTargetF, _tripwireRunFunctionF
                      } =
    config
        <$> _tripwireHomeF
        <*> _tripwireServerF
        <*> _tripwireJavaMemoryBytesF
        <*> _tripwireSetupFunctionF
        <*> _tripwireIterationsF
        <*> _tripwireTargetF
        <*> _tripwireRunFunctionF
