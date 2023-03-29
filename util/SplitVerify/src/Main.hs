{-# OPTIONS_GHC -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints #-}
module Main where
import System.Directory -- find home-dir for global cache/settings file
import Data.Aeson
import Data.Aeson.Types
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (splitOn)
import RIO
import RIO.Process
import System.Environment
import System.FilePath
import SplitVerify.Settings
import SplitVerify.Splitter
import qualified Data.Hashable as H
import qualified RIO.HashMap as HM
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Time.Clock
import Data.Time.LocalTime
import qualified RIO.List as L
import Control.Concurrent (forkIO)
import Control.Concurrent.QSem
import Paths_SplitVerify (version)

-- | The IO environment and the RIO environment is where side-effects live.
main :: IO ()
main = runSimpleApp run

-- This function handles command line arguments and calls runSettings
-- Todo: use a proper way to give command-line settings
run :: RIO SimpleApp ()
run = do
  args <- liftIO getArgs
  case args of
    [] -> do
      logError "Provide a settings file as an argument."
      logError "To see an example settings file, use --example."
      liftIO exitFailure
    ("--version":_) -> do logInfo$ displayShow version
                          exitSuccess
    ("--example":_) -> do logInfo$ "{ \"goals\" : [\n    { \"tag\" : \"FilesVerCors\"\n    , \"files\" : [ \"rbtsrc/Node.java\"\n, \"rbtsrc/Tree.java\"\n ]\n    , \"vct\" : \"/Path/To/vercors/unix/bin/vct\"\n    } ]\n, \"Use null fields like these and the one below for comments\" : null\n, \"Cache-filename and temp-directoryname are optional fields (it defaults to using your input file name with suffix .cache and .temp):\" : null\n, \"cache\" : \"cache\"\n, \"temp\" : \"temp\", \"Number of concurrent threads to use for calling verifiers. Zero for unlimited, use -1 for forcing single-threadedness and deterministic behavior.\" : null, \"cpu\" : 4 \n}"
                          exitSuccess
    [x] -> do
      st <- liftIO$ eitherDecodeFileStrict x 
      case st of
        Left err -> do
          logError$ display ("Could not parse "<>T.pack x)
          logError (display$T.pack err)
          logError "To see an example settings file, use --example."
          liftIO exitFailure
        Right v -> runSettings (T.pack x) v
    _ -> do
      logError "Too many arguments provided."
      liftIO exitFailure

-- | Helper function for doing stuff in parallel (in IO).
--   sleepSort acts like traverseTry, but concurrently, changing the order to (roughly) the order in which the threads finish.
--   Another way to see this function is as an alternative to parallelInterleavedE, but one that returns a list with the results lazily.
sleepSort,traverseTry :: (a -> IO b) -> [a] -> IO [Either SomeException b]
sleepSort f values = do
        chan <- newChan
        forM_ values (\time -> forkIO ((try (f time)) >>= writeChan chan))
        take (length values) <$> getChanContents chan
traverseTry f = traverse (try . f)

data ReadFileUtf8Exception = ReadFileUtf8Exception !FilePath !UnicodeException
  deriving (Show, Typeable)
instance Exception ReadFileUtf8Exception

-- for every goal, run runGoal.
runSettings :: (HasLogFunc a, HasProcessContext a) => T.Text -> Settings -> RIO a ()
runSettings name s
  = sequenceA_ . map (runGoals name s) . goals $ s

runGoals :: forall a. (HasLogFunc a, HasProcessContext a)
   => T.Text -> Settings -> Criterion -> RIO a ()
runGoals name settings crit
  = do exists <- traverse isFileCred (critFiles crit)
       unless (and exists) $ liftIO exitFailure
       case crit of
         FileExists{} -> return () -- already done
         FilesVerCors{} -> withFiles (handleVerCors getCache putCache tempD settings crit)
  where
    isFileCred f
      = do e <- liftIO$ doesFileExist (T.unpack f)
           unless e $ logError (display$ "File does not exist: "<>f)
           return e
    subtLookup fullCache subtbl = fromMaybe mempty (parseMaybe (fullCache .:) subtbl :: Maybe (HashMap Text Value))
    getCache subtbl
      = do fullCache <- getFullCache
           return (subtLookup fullCache subtbl)
    getFullCache
      = do cacheExists <- liftIO$doesFileExist (T.unpack cacheF)
           cacheOrErr <- if cacheExists then liftIO$ eitherDecodeFileStrict (T.unpack cacheF)
                         else return (Right mempty)
           case cacheOrErr of
              Left e -> do logError "Could not parse the cache file."
                           logError ("Make sure you have named the right file: "<>display cacheF)
                           logError ("Delete this file if necessary. The parse error is:")
                           logError (display (T.pack e))
                           liftIO exitFailure
              Right v -> return v
    -- todo: putCache should happen atomically, in case SplitVerify is called multiple times
    -- We currently only make a single call to putCache
    -- We can call this function much more aggressively if we use file-system atomicity
    putCache subtbl newCache
      = do fullCache <- getFullCache
           let newMap = Object$ foldr ($) (subtLookup fullCache subtbl) newCache
           liftIO$ encodeFile (T.unpack cacheF) (HM.insert subtbl newMap fullCache)
    cacheF = (fromMaybe (name <> ".cache") $ cache settings)
    tempD = (fromMaybe (name <> ".temp") $ temp settings)
    withFiles f = do files <- traverse (readFileUtf8 . T.unpack) (critFiles crit)
                     f files

-- | Get a git version for the thing a command-line thing is run from
getVersion :: (HasLogFunc env, HasProcessContext env)
              => FilePath -> RIO env Text
getVersion command
 = do (exitCode, vctversion0, errs) <- proc command ["--version"] readProcess
      vctversion <- if exitCode /= ExitSuccess then
                       do let prelimPath = takeDirectory $ command 
                          execPath <- if null prelimPath then do 
                              (_,o,_) <- proc "which" [command] readProcess
                              return (takeDirectory $ Char8.unpack o)
                            else return prelimPath
                          (exitCode2,versionBS,errs2) <- proc "git" ["-C",execPath,"rev-parse","HEAD"] readProcess
                          if exitCode2 /= ExitSuccess then do
                              logWarn "Could not retrieve vct-version via git. Updating vct will not cause a re-run!"
                              logError . display . T.pack $ show errs
                              logError . display . T.pack $ show errs2
                              return versionBS
                            else do logWarn $ "git-reported vct versions used!"
                                    return versionBS
                    else return vctversion0
      versionRes <- case decodeUtf8' $ DBL.toStrict vctversion of
                  Left e     -> do logError "Git hash not returned in UTF8"
                                   logError . display . T.pack $ show errs
                                   logError . display . T.pack $ show e
                                   error "Please report this as a bug (this might be a git issue but I'd like to find out if you get this)."
                  Right text -> return text
      return versionRes

handleVerCors :: (HasLogFunc env, HasProcessContext env)
  => (Text -> RIO env (HashMap Text Value))
  -> (Text -> [HashMap Text Value -> HashMap Text Value] -> RIO env a)
  -> Text
  -> Settings
  -> Criterion
  -> [Text]
  -> RIO env ()
-- | handle a bunch of files using VerCors
handleVerCors getCache putCache tempD settings crit files
 = do let vctCommand = maybe "vct" T.unpack $ critVct crit
      vctversion <- getVersion vctCommand
      timezone <- liftIO$ getCurrentTimeZone
      cacheTbl <- getCache vctversion
      -- TODO: use .:? instead, to properly parse the cache file
      let alltext = map (if critReduceWS crit then T.intercalate "    " . T.splitOn "\t" else id) files
      chunks <- case chunk crit alltext of
       Left e -> do logError ("Error parsing:  " <> displayShow e)
                    logError ("Proceeding as a single chunk for a possibly better error message (result should be Error)")
                    return [(0,(alltext,alltext))]
       Right lst -> do logInfo$ "Split into "<> displayShow (length lst)<>" chunk(s)"
                       return (zip [0..] lst)
      let (cpus,st) = case fromMaybe 0 (cpuLimit settings) of
                   0 -> (length chunks,False)
                   v | v > 0 -> (v,False)
                   _ -> (1,True)
      qsem <- liftIO$ newQSem cpus
      res <- withRunInIO $ \inIO ->
         let inCache v = HM.lookup v cacheTbl >>= parseMaybe parseJSON
             combineTxts = T.pack . show . H.hash
             cachedTxt = combineTxts . (if critReduceWS crit then snd else fst) -- TODO: use hash instead
             -- find in cache, if it does not exist, look it up.
             -- the lookup is bracketed using bracket_ and two semaphores.
             -- this limits the number of concurrent checkVCT calls.
             -- (bracket_ ensures the signalQSem is always executed, also in case of exceptions)
             findInCache (nr, txt)
               = case inCache (cachedTxt txt) of
                   Nothing -> bracket_ (waitQSem qsem) (signalQSem qsem)$
                                checkVCT nr txt
                   Just v -> return (Nothing,v)
             checkVCT :: Int -> ([Text],[Text]) -> IO (Maybe Text, VctRun)
             checkVCT nr txt@(fsts,_)
               = do let (fileNames,fileActions)
                          = L.unzip
                            [ ( fname_full, do createDirectoryIfMissing True (takeDirectory fname_full)
                                               writeFileUtf8 fname_full fcontent )
                            | (fname, fcontent) <- zip (critFiles crit) fsts
                            , let fname_full = (T.unpack tempD <> [pathSeparator] <>
                                                show nr <> "." <> T.unpack fname) ]
                    sequenceA_ fileActions
                    preTime <- getCurrentTime
                    -- actual vct call happens here:
                    (c, o, e) <- inIO $ proc vctCommand ("--silicon":fileNames) readProcess
                    postTime <- getCurrentTime
                    return (Just (cachedTxt txt)
                           ,VctRun preTime postTime (c == ExitSuccess && null (Char8.unpack e))
                                   (T.pack . Char8.unpack$o<>e))
             -- prints the result and returns a cache-entry
             printResult (Left exptn)
              = do inIO$ logError ("System error (not caused by a vct call).")
                   inIO$ logError (display exptn)
                   return (False,id)
             printResult (Right (b,r@(VctRun pr _ v o)))
              = do inIO$ logInfo$ (if b==Nothing then "Cached r" else "R") <>
                             "esult due to vct call at " <>
                             (display . T.pack . show . utcToLocalTime timezone $ pr)
                   inIO$ logInfo$ display o
                   return (v,maybe id (\k -> HM.insert k (toJSON r)) b)
        in traverse printResult =<< (if st then traverseTry else sleepSort) findInCache chunks
      let (_oks,newCache) = L.unzip res
      -- decision of fail/pass does not work right
      -- if and oks then logInfo $ "All tests Pass!" else logWarn $ "You still have failing tests!"
      _ <- putCache vctversion newCache
      return ()