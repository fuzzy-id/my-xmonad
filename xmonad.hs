module Main where

import MyConfig
import XMonad.Util.EntryHelper (withHelper)

main :: IO ()
main = withHelper myMain
