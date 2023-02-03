{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Log.Effect where

import Polysemy
import Polysemy.Log.Types

data Logger m a where
  LogMessage :: Message -> Logger m ()

makeSem ''Logger
