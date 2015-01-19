{-# LANGUAGE TemplateHaskell #-}
module Test.Brightness where

import Test.Tasty.TH
import Test.Tasty.HUnit

tests = $(testGroupGenerator)

