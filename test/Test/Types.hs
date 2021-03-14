{-# Language NoImplicitPrelude #-}

module Test.Types where

import Graphics.Gloss ( Picture (Blank) )
import Prelude hiding (lookup)
import Types
import Test.HUnit

testInsert :: Test
testInsert = TestCase $ assertEqual "test testing" expected actual
  where
    expected = Square Blank 1 1 Nothing
    actual = lookup (Eight, A) . insert (Eight, A) expected $ terminalBoard

testInsert2 :: Test
testInsert2 = TestCase $ assertEqual "test testing" (expected, expected2) actual
  where
    expected = Square Blank 1 1 Nothing
    expected2 = Square Blank 2 2 Nothing
    m = insert (Eight, B) expected2 . insert (Eight, A) expected $ terminalBoard
    actual = (lookup (Eight, A) m, lookup (Eight, B) m)

testUpdate :: Test
testUpdate = TestCase $ assertEqual "" expected actual
  where
    expected = Square Blank 1 0 Nothing
    actual = lookup (Eight, A) . update (Eight, A) yOffset 1 $ terminalBoard



