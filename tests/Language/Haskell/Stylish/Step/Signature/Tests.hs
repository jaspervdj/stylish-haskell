{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Stylish.Step.Signature.Tests
    ( tests
    ) where

import           Language.Haskell.Stylish.Step.Signature
import           Language.Haskell.Stylish.Tests.Util (assertSnippet, testStep)
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@=?))

tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Signature.Tests"
    [ testCase "do not wrap signature if it fits max column length" case00
    , testCase "wrap signature if it does not fit max column length" case01
    , testCase "how it behaves when there is a list of constraints" case02
    -- , testCase "how it behaves when there is a explicit forall" case03
    -- , testCase "how it behaves when there is a explicit forall" case04
    -- , testCase "how it behaves when there is a large function in the argument" case05
    ]

config :: Int -> Config
config cMaxColumns = Config
  { cMaxColumns = MaxColumns cMaxColumns
  }

case00 :: Assertion
case00 = expected @=? testStep (step $ config 80) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "fooBar :: a -> b -> a"
      , "fooBar v _ = v"
      ]
    expected = input

case01 :: Assertion
case01 = expected @=? testStep (step $ config 20) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "fooBar :: a -> b -> a"
      , "fooBar v _ = v"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "fooBar ::"
      , "     a"
      , "  -> b"
      , "  -> a"
      , "fooBar v _ = v"
      ]

case02 :: Assertion
case02 = expected @=? testStep (step $ config 20) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "fooBar :: (Eq a, Show b) => a -> b -> a"
      , "fooBar v _ = v"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "fooBar ::"
      , "     (Eq a, Show b)"
      , "  => a"
      , "  -> b"
      , "  -> a"
      , "fooBar v _ = v"
      ]

case03 :: Assertion
case03 = expected @=? testStep (step $ config 20) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "fooBar :: forall a . b. (Eq a, Show b) => a -> b -> a"
      , "fooBar v _ = v"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "fooBar ::"
      , "     forall a . b."
      , "     (Eq a, Show b)"
      , "  => a"
      , "  -> b"
      , "  -> a"
      , "fooBar v _ = v"
      ]

case04 :: Assertion
case04 = expected @=? testStep (step $ config 20) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "fooBar :: forall a . b. c. (Eq a, Show b, Ord c) => a -> b -> c -> a"
      , "fooBar v _ _ = v"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "fooBar ::"
      , "     forall a . b. ("
      , "       Eq a"
      , "     , Show b"
      , "     , Ord c)"
      , "     )"
      , "  => a"
      , "  -> b"
      , "  -> a"
      , "fooBar v _ = v"
      ]

case05 :: Assertion
case05 = expected @=? testStep (step $ config 20) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "fooBar :: => a -> (forall c. Eq c => c -> a -> a) -> a"
      , "fooBar v _ = v"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "fooBar ::"
      , "  => a"
      , "  -> (   forall c. Eq c"
      , "      => c"
      , "      -> a"
      , "      -> a"
      , "     )"
      , "  -> a"
      , "fooBar v _ = v"
      ]
