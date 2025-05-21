module JMS.Infra.Control where

import System.IO
import qualified Control.Exception.Safe as E
import System.Log.FastLogger

import qualified JMS.Domain.Model.Type as DM
import qualified JMS.Domain.Model.Utility as DM

import JMS.Infra.Type
import JMS.Infra.Constant
import JMS.Infra.Utility
import JMS.Infra.Core

-- |
--
run :: DM.DomainContext ()
run domDat = do
  hPutStrLn stderr "[INFO] JMS.Infra.Control.run called."
  appDat <- defaultAppData
  runWithAppData appDat domDat

-- |
--
runWithAppData :: AppData -> DM.DomainContext ()
runWithAppData appDat domDat = do
  logDat <- DM.createLogger domDat _LOG_FILE_NAME
  runWithLogger logDat appDat domDat

-- |
--
runWithLogger :: (TimedFastLogger, IO ()) -> AppData -> DM.DomainContext ()
runWithLogger (logger, finalizeLogger) appDat domDat = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runApp domDat appDat logger app
    >>= \case
      Right x -> return x
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] JMS.Infra.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] JMS.Infra.Control.run exception occurred."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] JMS.Infra.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
