{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Log.Effect where

import Polysemy
import Polysemy.Log.Types

-- | Simple logging effect.
--   Note, while you can use the effect method directly, you should rather use
--   'logDebug' and other similar functions from 'Polysemy.Log.Utils'.
data Logger m a where
  LogMessage :: Message -> Logger m ()

makeSem ''Logger
