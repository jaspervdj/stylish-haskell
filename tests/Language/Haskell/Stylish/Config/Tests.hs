module Language.Haskell.Stylish.Config.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Types                    as Aeson
import qualified Data.ByteString.Lazy.Char8          as BL8
import qualified Data.Set                            as Set
import qualified Data.YAML.Aeson                     as Yaml
import           System.Directory
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, assert, (@?=))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Config"
    [ testCase "Extensions extracted correctly from .cabal file"
               testExtensionsFromDotCabal
    , testCase "Extensions extracted correctly from .stylish-haskell.yaml file"
               testExtensionsFromDotStylish
    , testCase "Extensions extracted correctly from .stylish-haskell.yaml and .cabal files"
               testExtensionsFromBoth
    , testCase "Correctly read .stylish-haskell.yaml file with default max column number"
               testDefaultColumns
    , testCase "Correctly read .stylish-haskell.yaml file with specified max column number"
               testSpecifiedColumns
    , testCase "Correctly read .stylish-haskell.yaml file with no max column number"
               testNoColumns
    , testCase "Backwards-compatible align options"
               testBoolSimpleAlign
    ]


--------------------------------------------------------------------------------
-- | Put an example config files (.cabal/.stylish-haskell.yaml/both)
-- into the current directory and extract extensions from it.
createFilesAndGetConfig :: [(FilePath, String)] -> IO Config
createFilesAndGetConfig files = withTestDirTree $ do
  mapM_ (\(k, v) -> writeFile k v) files
  -- create an empty directory and change into it
  createDirectory "src"
  setCurrentDirectory "src"
  -- from that directory read the config file and extract extensions
  -- to make sure the search for .cabal file works
  config <- loadConfig (const (pure ())) Nothing
  pure config


--------------------------------------------------------------------------------
testExtensionsFromDotCabal :: Assertion
testExtensionsFromDotCabal =
  assert $ (expected ==) . Set.fromList . configLanguageExtensions <$>
    createFilesAndGetConfig [("test.cabal", dotCabal True)]
    where
      expected = Set.fromList ["ScopedTypeVariables", "DataKinds"]


--------------------------------------------------------------------------------
testExtensionsFromDotStylish :: Assertion
testExtensionsFromDotStylish =
  assert $ (expected ==) . Set.fromList . configLanguageExtensions <$>
    createFilesAndGetConfig [(".stylish-haskell.yaml", dotStylish)]
    where
      expected = Set.fromList ["TemplateHaskell", "QuasiQuotes"]


--------------------------------------------------------------------------------
testExtensionsFromBoth :: Assertion
testExtensionsFromBoth =
  assert $ (expected ==) . Set.fromList . configLanguageExtensions <$>
    createFilesAndGetConfig [ ("test.cabal", dotCabal True)
                            , (".stylish-haskell.yaml", dotStylish)]
    where
      expected = Set.fromList
        ["ScopedTypeVariables", "DataKinds", "TemplateHaskell", "QuasiQuotes"]


--------------------------------------------------------------------------------
testSpecifiedColumns :: Assertion
testSpecifiedColumns =
  assert $ (expected ==) . configColumns <$>
    createFilesAndGetConfig [(".stylish-haskell.yaml", dotStylish)]
    where
      expected = Just 110


--------------------------------------------------------------------------------
testDefaultColumns :: Assertion
testDefaultColumns =
  assert $ (expected ==) . configColumns <$>
    createFilesAndGetConfig [(".stylish-haskell.yaml", dotStylish2)]
    where
      expected = Just 80


--------------------------------------------------------------------------------
testNoColumns :: Assertion
testNoColumns =
  assert $ (expected ==) . configColumns <$>
    createFilesAndGetConfig [(".stylish-haskell.yaml", dotStylish3)]
    where
      expected = Nothing


--------------------------------------------------------------------------------
testBoolSimpleAlign :: Assertion
testBoolSimpleAlign = do
    Right val <- pure $ Yaml.decode1 $ BL8.pack config
    Aeson.Success conf <- pure $ Aeson.parse parseConfig val
    length (configSteps conf) @?= 1
  where
    config = unlines
      [ "steps:"
      , "  - simple_align:"
      , "      cases: true"
      , "      top_level_patterns: always"
      , "      records: false"
      ]


-- | Example cabal file borrowed from
--   https://www.haskell.org/cabal/users-guide/developing-packages.html
--   with some default-extensions added
dotCabal :: Bool -> String
dotCabal includeExtensions = unlines $
  [ "name:            TestPackage"
  , "version:         0.0"
  , "synopsis:        Package with library and two programs"
  , "license:         BSD3"
  , "author:          Angela Author"
  , "build-type:      Simple"
  , "cabal-version:   >= 1.2"
  , ""
  , "library"
  , "   build-depends:   HUnit"
  , "   exposed-modules: A, B, C"
  ] ++
  [if includeExtensions then "   default-extensions: ScopedTypeVariables"
                        else ""]
  ++
  [ ""
  , "executable program1"
  , "   main-is:         Main.hs"
  , "   hs-source-dirs:  prog1"
  , "   other-modules:   A, B"
  ] ++
  [if includeExtensions then "   default-extensions: DataKinds"
                        else ""]

-- | Example .stylish-haskell.yaml
dotStylish :: String
dotStylish = unlines $
  [ "steps:"
  , "  - imports:"
  , "      align: none"
  , "      list_align: after_alias"
  , "      long_list_align: inline"
  , "      separate_lists: true"
  , "  - language_pragmas:"
  , "      style: vertical"
  , "      align: false"
  , "      remove_redundant: true"
  , "  - trailing_whitespace: {}"
  , "  - records:"
  , "      equals: \"same_line\""
  , "      first_field: \"indent 2\""
  , "      field_comment: 2"
  , "      via: \"indent 2\""
  , "      deriving_indent: 4"
  , "      deriving_same_line: false"
  , "columns: 110"
  , "language_extensions:"
  , "  - TemplateHaskell"
  , "  - QuasiQuotes"
  ]

-- | Example .stylish-haskell.yaml
dotStylish2 :: String
dotStylish2 = unlines $
  [ "steps:"
  , "  - imports:"
  , "      align: none"
  , "      list_align: after_alias"
  , "      long_list_align: inline"
  , "      separate_lists: true"
  , "  - language_pragmas:"
  , "      style: vertical"
  , "      align: false"
  , "      remove_redundant: true"
  , "  - trailing_whitespace: {}"
  , "language_extensions:"
  , "  - TemplateHaskell"
  , "  - QuasiQuotes"
  ]

-- | Example .stylish-haskell.yaml
dotStylish3 :: String
dotStylish3 = unlines $
  [ "steps:"
  , "  - imports:"
  , "      align: none"
  , "      list_align: after_alias"
  , "      long_list_align: inline"
  , "      separate_lists: true"
  , "  - language_pragmas:"
  , "      style: vertical"
  , "      align: false"
  , "      remove_redundant: true"
  , "  - trailing_whitespace: {}"
  , "columns: null"
  , "language_extensions:"
  , "  - TemplateHaskell"
  , "  - QuasiQuotes"
  ]
