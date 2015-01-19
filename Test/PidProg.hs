{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Test.PidProg where

import System.Posix.Process
import Test.Tasty.HUnit
import Test.Tasty.TH

import My.PidProg

tests = $(testGroupGenerator)

case_doesPidProgRun_on_own_pid :: Assertion
case_doesPidProgRun_on_own_pid = 
  getProcessID >>= doesPidProgRun >>= (@=? True)

case_doesPidProgRun_on_non_existent_pid :: Assertion
case_doesPidProgRun_on_non_existent_pid = 
  doesPidProgRun (-1) >>= (@=? False)
