{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module JMS.Domain.Service.State.Start.Disconnect where

import JMS.Domain.Service.Type

-- |
--
instance IStateActivity StartStateData DisconnectEventData
  -- @see default implementation in Type module.

