--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                                         (defaultMain)


--------------------------------------------------------------------------------
import qualified Language.Haskell.Stylish.Config.Tests
import qualified Language.Haskell.Stylish.Parse.Tests
import qualified Language.Haskell.Stylish.Step.Data.Tests
import qualified Language.Haskell.Stylish.Step.Imports.Tests
import qualified Language.Haskell.Stylish.Step.Imports.FelixTests
import qualified Language.Haskell.Stylish.Step.ModuleHeader.Tests
import qualified Language.Haskell.Stylish.Step.LanguagePragmas.Tests
import qualified Language.Haskell.Stylish.Step.Signature.Tests
import qualified Language.Haskell.Stylish.Step.SimpleAlign.Tests
import qualified Language.Haskell.Stylish.Step.Squash.Tests
import qualified Language.Haskell.Stylish.Step.Tabs.Tests
import qualified Language.Haskell.Stylish.Step.TrailingWhitespace.Tests
import qualified Language.Haskell.Stylish.Step.UnicodeSyntax.Tests
import qualified Language.Haskell.Stylish.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ Language.Haskell.Stylish.Parse.Tests.tests
    , Language.Haskell.Stylish.Config.Tests.tests
    , Language.Haskell.Stylish.Step.Data.Tests.tests
    , Language.Haskell.Stylish.Step.Imports.Tests.tests
    , Language.Haskell.Stylish.Step.Imports.FelixTests.tests
    , Language.Haskell.Stylish.Step.LanguagePragmas.Tests.tests
    , Language.Haskell.Stylish.Step.ModuleHeader.Tests.tests
    , Language.Haskell.Stylish.Step.Signature.Tests.tests
    , Language.Haskell.Stylish.Step.SimpleAlign.Tests.tests
    , Language.Haskell.Stylish.Step.Squash.Tests.tests
    , Language.Haskell.Stylish.Step.Tabs.Tests.tests
    , Language.Haskell.Stylish.Step.TrailingWhitespace.Tests.tests
    , Language.Haskell.Stylish.Step.UnicodeSyntax.Tests.tests
    , Language.Haskell.Stylish.Tests.tests
    ]
