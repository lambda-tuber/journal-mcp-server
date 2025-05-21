{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module JMS.Domain.Service.State.Start.Terminate where

import JMS.Domain.Service.Type


-- |
--
instance IStateActivity StartStateData TerminateEventData
  -- @see default implementation in Type module.

