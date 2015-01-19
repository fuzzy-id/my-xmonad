{-# LANGUAGE TemplateHaskell #-}
module Test.Brightness where

import Test.Tasty (TestTree)
import Test.Tasty.TH
import Test.Tasty.HUnit

import My.Brightness

tests :: TestTree
tests = $(testGroupGenerator)

case_getBrightness_lies_in_borders :: Assertion
case_getBrightness_lies_in_borders = do b <- getBrightness
                                        minBrightness <= b @?= True
                                        maxBrightness >= b @?= True

case_setBrightness_to_getBrightness :: Assertion
case_setBrightness_to_getBrightness = do bOld <- getBrightness
                                         setBrightness bOld
                                         bNew <- getBrightness
                                         bOld @?= bNew
