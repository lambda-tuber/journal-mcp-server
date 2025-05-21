{-# LANGUAGE LambdaCase #-}

module JMS.App.Control where

import qualified Control.Exception.Safe as E
import Data.Default
import Data.Yaml
import Control.Lens
import System.Log.FastLogger
import System.IO
import System.Directory
import Data.Aeson

import qualified JMS.Domain.Model.Type as DM
import qualified JMS.Domain.Model.Utility as DM

import JMS.App.Type
import JMS.App.Utility
import JMS.App.Core
import JMS.App.Constant


-- |
--
run :: ArgData
    -> [DM.DomainContext ()]
    -> IO ()
run args apps = do
  hPutStrLn stderr "[INFO] JMS.App.Control.run called."

  conf <- maybe (pure def) decodeFileThrow (args^.yamlArgData)
  setCurrentDirectory $ conf^.workDirConfigData

  defDom <- DM.defaultDomainData
  let domDat = defDom {
               DM._logDirDomainData   = conf^.logDirConfigData
             , DM._logLevelDomainData = conf^.logLevelConfigData
             , DM._scriptsDirDomainData = conf^.scriptsDirConfigData
             }
      appDat = def {
               _appsAppData = apps
             }
  DM.createLogger domDat _LOG_FILE_NAME >>= runWithLogger domDat appDat


-- |
--
runWithLogger :: DM.DomainData -> AppData -> (TimedFastLogger, IO ()) -> IO ()
runWithLogger domDat appDat (logger, finalizeLogger) = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runApp domDat appDat logger app
    >>= \case
      Right _ -> return ()
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] JMS.App.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] JMS.App.Control.run exception occurred."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] JMS.App.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
