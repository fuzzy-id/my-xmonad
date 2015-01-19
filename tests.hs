{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty

import qualified Test.Brightness as TB
import qualified Test.PidProg as TPD
import qualified Test.Pulse as TP

main :: IO ()
main = defaultMain $ testGroup "Collected" [ TPD.tests
                                           , TP.tests
                                           , TB.tests
                                           ]

