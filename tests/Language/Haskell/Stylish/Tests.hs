--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Bifunctor                      (first)
import           Data.List                           (sort)
import           System.Directory                    (createDirectory)
import           System.FilePath                     (normalise, (</>))
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04" case04
    , testCase "case 05" case05
    , testCase "case 06" case06
    , testCase "case 07" case07
    , testCase "case 08" case08
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = (@?= result) =<< format Nothing Nothing input
  where
    input = "module Herp where\ndata Foo = Bar | Baz { baz :: Int }"
    result = Right $ lines input


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = withTestDirTree $ do
    writeFile "test-config.yaml" $ unlines
        [ "steps:"
        , "  - records:"
        , "      equals: \"indent 2\""
        , "      first_field: \"indent 2\""
        , "      field_comment: 2"
        , "      deriving: 2"
        , "      via: \"indent 2\""
        ]

    actual <- format (Just $ ConfigPath "test-config.yaml") Nothing input
    actual @?= result
  where
    input = "module Herp where\ndata Foo = Bar | Baz { baz :: Int }"
    result = Right [ "module Herp where"
                   , "data Foo"
                   , "  = Bar"
                   , "  | Baz"
                   , "      { baz :: Int"
                   , "      }"
                   ]

--------------------------------------------------------------------------------
case03 :: Assertion
case03 = withTestDirTree $ do
    writeFile "test-config.yaml" $ unlines
        [ "steps:"
        , "  - records:"
        , "      equals: \"same_line\""
        , "      first_field: \"same_line\""
        , "      field_comment: 2"
        , "      deriving: 2"
        , "      via: \"indent 2\""
        ]

    actual <- format (Just $ ConfigPath "test-config.yaml") Nothing input
    actual @?= result
  where
    input = unlines [ "module Herp where"
                    , "data Foo"
                    , "  = Bar"
                    , "  | Baz"
                    , "      { baz :: Int"
                    , "      }"
                    ]
    result = Right [ "module Herp where"
                   , "data Foo = Bar"
                   , "         | Baz { baz :: Int"
                   , "               }"
                   ]

--------------------------------------------------------------------------------
case04 :: Assertion
case04 = (@?= result) =<< format Nothing (Just fileLocation) input
  where
    fileLocation = "directory/File.hs"
    input = "module Herp"
    result = Left $
      fileLocation <> ": RealSrcSpan SrcSpanPoint \"directory/File.hs\" 2 1:"
      <> " parse error (possibly incorrect indentation or mismatched brackets)\n"

--------------------------------------------------------------------------------
-- | When providing current dir including folders and files.
case05 :: Assertion
case05 = withTestDirTree $ do
  createDirectory aDir >> writeFile c fileCont
  mapM_ (flip writeFile fileCont) fs
  result <- map fst <$> findHaskellFiles False input
  sort result @?= (sort $ map normalise expected)
  where
    input    = c : fs
    fs = ["b.hs", "a.hs"]
    c  = aDir </> "c.hs"
    aDir     = "aDir"
    expected = ["a.hs", "b.hs", c]
    fileCont = ""


--------------------------------------------------------------------------------
-- | When the input item is not file, do not recurse it.
case06 :: Assertion
case06 = withTestDirTree $ do
  mapM_ (flip writeFile "") input
  result <- map fst <$> findHaskellFiles False input
  result @?= expected
  where
    input    = ["b.hs"]
    expected = map normalise input


--------------------------------------------------------------------------------
-- | Empty input should result in empty output.
case07 :: Assertion
case07 = withTestDirTree $ do
  mapM_ (flip writeFile "") input
  result <- map fst <$> findHaskellFiles False input
  result @?= expected
  where
    input    = []
    expected = input


--------------------------------------------------------------------------------
-- | Should work for .hsc files.
case08 :: Assertion
case08 = withTestDirTree $ do
  createDirectory aDir >> writeFile c fileCont
  mapM_ (flip writeFile fileCont) fs
  result <- findHaskellFiles False input
  sort result @?= (sort $ map (first normalise) expected)
  where
    input    = c : fs
    fs = ["b.hsc", "a.hsc", "d.hs"]
    c  = aDir </> "c.hsc"
    aDir     = "aDir"
    expected = [("a.hsc", ["CPP"]), ("b.hsc", ["CPP"]), (c, ["CPP"]), ("d.hs", [])]
    fileCont = ""
