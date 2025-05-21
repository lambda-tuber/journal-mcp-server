{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}


module JMS.Domain.Service.Control where

import System.IO
import qualified Control.Exception.Safe as E
import System.Log.FastLogger

import qualified JMS.Domain.Model.Type as DM
import qualified JMS.Domain.Model.Utility as DM

import JMS.Domain.Service.Constant
import JMS.Domain.Service.Type
import JMS.Domain.Service.Core
import JMS.Domain.Service.State.Start()
import JMS.Domain.Service.State.Run()
import JMS.Domain.Service.State.Stop()

import JMS.Domain.Service.Utility


-- |
--
run :: DM.DomainContext ()
run dat = do
  hPutStrLn stderr "[INFO] JMS.Domain.Service.Control.run called."

  let appDat = AppStateW StartState
  runWithAppData appDat dat

-- |
--
runWithAppData :: AppStateW -> DM.DomainContext ()
runWithAppData appDat domDat = do
  logDat <- DM.createLogger domDat _LOG_FILE_NAME
  runWithLogger logDat appDat domDat

-- |
--
runWithLogger :: (TimedFastLogger, IO ()) -> AppStateW -> DM.DomainContext ()
runWithLogger (logger, finalizeLogger) appDat domDat = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runAppState domDat appDat logger app
    >>= \case
      Right (x, _) -> return x
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] JMS.Domain.Service.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] JMS.Domain.Service.Control.run exception occurred."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] JMS.Domain.Service.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
