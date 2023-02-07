{-# LANGUAGE DeriveGeneric #-}
module Polysemy.Log.Types where

import Data.Text (Text)
import GHC.Generics

-- | Message priority, based on syslog priorities.
data Priority = Debug
              | Info
              | Notice
              | Warning
              | Error
              | Critical
              | Alert
              | Emergency
              deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Message to log. Contains priority and message text. Rendering the message is up to the user.
data Message = Message { priority :: !Priority, message :: !Text }
  deriving (Show, Eq, Ord, Generic)
