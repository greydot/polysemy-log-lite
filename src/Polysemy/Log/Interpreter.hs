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
import System.IO (Handle, hFlush, stdout, stderr)

-- | Bare logging interpreter. Requires file handle, message formattng function, and a minimum priority to filter on.
runLogHandle :: Member (Embed IO) r => Handle -> (Message -> Text) -> (Message -> Bool) -> Sem (Logger ': r) a -> Sem r a
runLogHandle h fmt filt = interpret f
  where f :: Member (Embed IO) r => Logger m a -> Sem r a
        f (LogMessage msg) | filt msg = embed (Text.hPutStrLn h (fmt msg) >> hFlush h)
                           | otherwise = pure ()

-- | Log messages to 'stdout' or 'stderr'.
runLogStdout, runLogStderr :: Member (Embed IO) r => Priority -> Sem (Logger ': r) a -> Sem r a
runLogStdout prio = runLogHandle stdout formatMsg (filterPriority prio)
runLogStderr prio = runLogHandle stderr formatMsg (filterPriority prio)

-- | Log messages to 'stdout' or 'stderr'. Format them into JSON objects.
runLogJSONStdout, runLogJSONStderr :: Member (Embed IO) r => Priority -> Sem (Logger ': r) a -> Sem r a
runLogJSONStdout prio = runLogHandle stdout formatJSON (filterPriority prio)
runLogJSONStderr prio = runLogHandle stderr formatJSON (filterPriority prio)


filterPriority :: Priority -> Message -> Bool
filterPriority p msg = priority msg >= p

formatMsg :: Message -> Text
formatMsg (Message p msg) = mconcat [ "[", Text.toLower (Text.pack $ show p), "] "
                                    , msg
                                    ]

formatJSON :: Message -> Text
formatJSON (Message p msg) = mconcat [ "{ \"priority\": \"" <> Text.toLower (Text.pack $ show p) <> "\", "
                                     ,   "\"message\": \"" <> escape msg <> "\" }"
                                     ]
  where escape = Text.concatMap f
        f '"' = "\\\""
        f '\\' = "\\\\"
        f '\n' = "\\n"
        f '\r' = "\\r"
        f '\t' = "\\t"
        f c = Text.singleton c
