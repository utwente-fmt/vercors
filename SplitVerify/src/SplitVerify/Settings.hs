{-# OPTIONS_GHC -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
module SplitVerify.Settings
  (Settings(..),Criterion(FilesVerCors,FileExists),critFiles,critVct,critReduceWS
  ,VctRun(VctRun)) where
{-
  Module for settings datatypes, built such that it only exports total functions.
  It is split to get rid of warnings about incomplete patterns.
-}
import GHC.Generics
import Data.Aeson
import qualified RIO.Text as T
import RIO
import Data.Time.Clock

-- | Entries used for VCT cache.
data VctRun
  = VctRun {
    start :: UTCTime,
    end :: UTCTime,
    ok :: Bool,
    display :: T.Text
  } deriving (Show,Generic)

-- | General settings
data Settings
  = Settings {
    goals :: [Criterion],
    cpuLimit :: Maybe Int,
    cache :: Maybe T.Text,
    temp :: Maybe T.Text
  } deriving (Show,Generic)

-- | A single goal to be checked
data Criterion
  = FilesVerCors { files :: [T.Text] , vct :: Maybe T.Text , reduceWhitespace :: Maybe Bool }
  | FileExists { file :: T.Text }
  deriving (Show,Generic)

-- | Accessor function to determine whether or not to reduce whitespace in cache (defaults to True for VerCors)
--   This function is total so it's safer to use (rather than using reduceWhitespace directly)
critReduceWS :: Criterion -> Bool
critReduceWS FilesVerCors{reduceWhitespace=Just x} = x
critReduceWS FilesVerCors{} = True
critReduceWS _ = False

-- | Accessor function to get a list of all the files named in a criterion.
--   This function is total so it's safer to use than using 'files' and 'file' directly
critFiles :: Criterion -> [Text]
critFiles (FilesVerCors{files = fs}) = fs
critFiles (FileExists{file=f}) = [f]
-- | Get the value of the VCT setting.
--   This function returns Nothing for recores that do not have such a field, making it total.
critVct :: Criterion -> Maybe Text
critVct (FilesVerCors{vct = v}) = v
critVct (FileExists{}) = Nothing

instance FromJSON VctRun where
instance FromJSON Settings where
instance FromJSON Criterion where
instance   ToJSON VctRun where
instance   ToJSON Settings where
instance   ToJSON Criterion where
