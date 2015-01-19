{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function
import Data.List
import System.Exit
import System.Posix.Process
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import My.Pulse
import My.PidProg

main :: IO ()
main = defaultMain tests

tests = $(testGroupGenerator)

instance Ord PulseItem where
  compare = compare `on` sinkName

-- Hang around some microseconds till settings propagate
delay :: IO ()
delay = threadDelay 6000

case_paSinkMuteToggle :: Assertion
case_paSinkMuteToggle = 
  bracket
    (head <$> paDumpSinks)
    paSetSink
    (\s -> do paSinkMuteToggle s
              delay
              s' <- paGetSinkByName . sinkName $ s
              (not . sinkMute) s' @?= sinkMute s)

case_exactly_one_default_sink :: Assertion
case_exactly_one_default_sink = 
  length . filter sinkDefault <$> paDumpSinks >>= (1 @=?)

case_default_sink_exists :: Assertion
case_default_sink_exists = do s <- getDefaultSink <$> paDumpSinks
                              sinkDefault s @?= True

case_paSetSink_does_not_alter_sinks :: Assertion
case_paSetSink_does_not_alter_sinks = 
  do sinks <- paDumpSinks
     mapM_ paSetSink sinks
     sinks' <- paDumpSinks
     sort sinks @=? sort sinks'

case_paUnmute :: Assertion
case_paUnmute = bracket (head <$> paDumpSinks) paSetSink t
  where t sink = do unless 
                      (sinkMute sink)
                      (paSetSink . toggleMute $ sink) >> delay
                    sink' <- paGetSinkByName . sinkName $ sink
                    sinkMute sink' @=? True
                    paUnmute sink'
                    delay
                    sink'' <- paGetSinkByName . sinkName $ sink
                    sinkMute sink'' @=? False

case_doesPidProgRun_on_own_pid :: Assertion
case_doesPidProgRun_on_own_pid = getProcessID >>= doesPidProgRun >>= (@=? True)

case_doesPidProgRun_on_non_existent_pid :: Assertion
case_doesPidProgRun_on_non_existent_pid = doesPidProgRun (-1) >>= (@=? False)
