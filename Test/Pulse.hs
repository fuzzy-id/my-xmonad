{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Pulse where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.Function
import Data.List
import Numeric
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import qualified Text.Parsec as P

import My.Pulse

tests = $(testGroupGenerator)

instance Arbitrary PulseItem where
  arbitrary = Sink 
              <$> arbitraryString `suchThat` (not . null)  -- sinkName
              <*> arbitrary                                -- sinkDefault
              <*> choose (0,maxBound)                      -- sinkVolume
              <*> arbitrary                                -- sinkMute

arbitraryString :: Gen String
arbitraryString = filter (`notElem` " \n\t\r\f\v\160") <$> arbitrary

class Serializable a where
  serialize :: a -> String

instance Serializable a => Serializable [a] where
  serialize = concatMap serialize

instance Serializable PulseItem where
  serialize Sink{..}
    | sinkDefault = body ++ "set-default-sink " ++ sinkName ++ "\n"
    | otherwise = body
    where body = unlines [serializeVolume, serializeMute]
          serializeVolume = 
            "set-sink-volume " ++ sinkName ++ " 0x" ++ showHex sinkVolume ""
          serializeMute 
            | sinkMute = muteBody ++ " yes"
            | otherwise = muteBody ++ " no"
            where muteBody = "set-sink-mute " ++ sinkName

instance Ord PulseItem where
  compare = compare `on` sinkName

(<&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(f <&> g) x = f x && g x

prop_pDump_on_serialized_eq_id :: PulseItem -> Bool
prop_pDump_on_serialized_eq_id s = [s] == result
  where result = (createSinks . getRight . P.parse pDump m) m
        m = serialize s

prop_pDump_on_serialized_list_eq_id :: [PulseItem] -> Bool
prop_pDump_on_serialized_list_eq_id s = (nubSinks . sort) s == sort result
  where result = (createSinks . getRight . P.parse pDump m) m
        m = serialize s
        nubSinks = nubBy ((==) `on` sinkName)

prop_toggleMute_alters_mute :: PulseItem -> Bool
prop_toggleMute_alters_mute s = sinkMute s /= sinkMute result
  where result = toggleMute s

prop_changeVolumePercent_result_lies_in_boundaries :: Int -> PulseItem -> Bool
prop_changeVolumePercent_result_lies_in_boundaries n =
  ((>= 0) <&> (<= maxVol)). sinkVolume . changeVolumePercent n

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

-- Hang around some microseconds till settings propagate
delay :: IO ()
delay = threadDelay 6000
