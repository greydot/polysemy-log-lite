{-# LANGUAGE OverloadedStrings #-}
module Main where

import Polysemy
import Polysemy.Log

main :: IO ()
main = runM
     $ runLogJSONStdout Debug
     $ logDebug "Hello!"
