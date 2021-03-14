module Main where

import Test.HUnit
import Test.Types

main :: IO Counts
main = runTestTT $ TestList
  [ TestLabel "testInsert" testInsert
  , TestLabel "testInsert2" testInsert2
  , TestLabel "testUpdate" testUpdate
  ]
