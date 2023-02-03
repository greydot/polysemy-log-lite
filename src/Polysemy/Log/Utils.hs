{-# LANGUAGE FlexibleContexts #-}
module Polysemy.Log.Utils where

import Data.Text (Text)
import Polysemy
import Polysemy.Log.Effect
import Polysemy.Log.Types

logDebug, logInfo, logNotice,
  logWarning, logError, logCritical,
  logAlert, logEmergency :: Member Logger r => Text -> Sem r ()
logDebug = logMessage . Message Debug
logInfo = logMessage . Message Info
logNotice = logMessage . Message Notice
logWarning= logMessage . Message Warning
logError = logMessage . Message Error
logCritical = logMessage . Message Critical
logAlert = logMessage . Message Alert
logEmergency = logMessage . Message Emergency
