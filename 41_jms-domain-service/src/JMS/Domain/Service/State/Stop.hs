{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module JMS.Domain.Service.State.Stop where

import Control.Monad.Logger

import qualified JMS.Domain.Model.Constant as DM

import JMS.Domain.Service.Type
import JMS.Domain.Service.TH

-- |
--
instanceTH_IAppState ''StopStateData

-- |
--
instance IStateActivity StopStateData EntryEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Stop entry called."
    return Nothing

-- |
--
instance IStateActivity StopStateData ExitEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Stop exit called."
    return Nothing

-- |
--
instance IStateActivity StopStateData TransitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData InitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData LaunchEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData DisconnectEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData TerminateEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Stop terminate called. will exit."
    return Nothing

-- |
--
instance IStateActivity StopStateData InitializedEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData ToolsListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData ToolsCallEventData
  -- @see default implementation in Type module.
