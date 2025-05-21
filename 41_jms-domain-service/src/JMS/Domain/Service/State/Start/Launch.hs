{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module JMS.Domain.Service.State.Start.Launch where

import JMS.Domain.Service.Type


-- |
--
instance IStateActivity StartStateData LaunchEventData
  -- @see default implementation in Type module.

