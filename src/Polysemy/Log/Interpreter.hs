{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Polysemy.Log.Interpreter where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Polysemy
import Polysemy.Log.Effect
import Polysemy.Log.Types
import System.IO (Handle, stdout, stderr)

runLogHandle :: Member (Embed IO) r => Handle -> (Message -> Text) -> Sem (Logger ': r) a -> Sem r a
runLogHandle h fmt = interpret f
  where f :: Member (Embed IO) r => Logger m a -> Sem r a
        f (LogMessage msg) = embed (Text.hPutStrLn h (fmt msg))

runLogStdout, runLogStderr :: Member (Embed IO) r => Sem (Logger ': r) a -> Sem r a
runLogStdout = runLogHandle stdout formatMsg
runLogStderr = runLogHandle stderr formatMsg

formatMsg :: Message -> Text
formatMsg (Message p msg) = mconcat [ "[", Text.toLower (Text.pack $ show p), "]"
                                    , msg
                                    ]
