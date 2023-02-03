{-# LANGUAGE DeriveGeneric #-}
module Polysemy.Log.Types where

import Data.Text (Text)
import GHC.Generics

data Priority = Debug
              | Info
              | Notice
              | Warning
              | Error
              | Critical
              | Alert
              | Emergency
              deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data Message = Message { priority :: !Priority, message :: !Text }
  deriving (Show, Eq, Ord, Generic)
