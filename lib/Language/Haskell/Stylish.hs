{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish
    ( -- * Run
      runSteps
      -- * Steps
    , simpleAlign
    , imports
    , languagePragmas
    , tabs
    , trailingWhitespace
    , unicodeSyntax
      -- ** Helpers
    , findHaskellFiles
    , considerFiles
    , considerFile
    , stepName
      -- * Config
    , module Language.Haskell.Stylish.Config
      -- * Misc
    , module Language.Haskell.Stylish.Verbose
    , version
    , format
    , ConfigPath(..)
    , Lines
    , Step
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                                    (foldM)
import           Data.Maybe                                       (maybeToList,
                                                                   mapMaybe)
import           System.Directory                                 (doesDirectoryExist,
                                                                   doesFileExist,
                                                                   listDirectory)
import           System.FilePath                                  (takeExtension,
                                                                   (</>))

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config
import           Language.Haskell.Stylish.Parse
import           Language.Haskell.Stylish.Step
import qualified Language.Haskell.Stylish.Step.Imports            as Imports
import qualified Language.Haskell.Stylish.Step.LanguagePragmas    as LanguagePragmas
import qualified Language.Haskell.Stylish.Step.SimpleAlign        as SimpleAlign
import qualified Language.Haskell.Stylish.Step.Tabs               as Tabs
import qualified Language.Haskell.Stylish.Step.TrailingWhitespace as TrailingWhitespace
import qualified Language.Haskell.Stylish.Step.UnicodeSyntax      as UnicodeSyntax
import           Language.Haskell.Stylish.Verbose
import           Paths_stylish_haskell                            (version)


--------------------------------------------------------------------------------
simpleAlign :: Maybe Int  -- ^ Columns
            -> SimpleAlign.Config
            -> Step
simpleAlign = SimpleAlign.step


--------------------------------------------------------------------------------
imports :: Maybe Int -- ^ columns
        -> Imports.Options
        -> Step
imports = Imports.step


--------------------------------------------------------------------------------
languagePragmas :: Maybe Int -- ^ columns
                -> LanguagePragmas.Style
                -> Bool -- ^ Pad to same length in vertical mode?
                -> Bool -- ^ remove redundant?
                -> String -- ^ language prefix
                -> Step
languagePragmas = LanguagePragmas.step


--------------------------------------------------------------------------------
tabs :: Int -- ^ number of spaces
     -> Step
tabs = Tabs.step


--------------------------------------------------------------------------------
trailingWhitespace :: Step
trailingWhitespace = TrailingWhitespace.step


--------------------------------------------------------------------------------
unicodeSyntax :: Bool -- ^ add language pragma?
              -> String -- ^ language prefix
              -> Step
unicodeSyntax = UnicodeSyntax.step


--------------------------------------------------------------------------------
runStep :: Extensions -> Maybe FilePath -> Lines -> Step -> Either String Lines
runStep exts mfp ls = \case
  Step _name step ->
    step ls <$> parseModule exts mfp (unlines ls)

--------------------------------------------------------------------------------
runSteps ::
     Extensions
  -> Maybe FilePath
  -> [Step]
  -> Lines
  -> Either String Lines
runSteps exts mfp steps ls =
 foldM (runStep exts mfp) ls steps

newtype ConfigPath = ConfigPath { unConfigPath :: FilePath }

-- |Formats given contents optionally using the config provided as first param.
-- The second file path is the location from which the contents were read.
-- If provided, it's going to be printed out in the error message.
format :: Maybe ConfigPath -> Maybe FilePath -> String -> IO (Either String Lines)
format maybeConfigPath maybeFilePath contents = do
  conf <- loadConfig (makeVerbose True) (fmap unConfigPath maybeConfigPath)
  pure $ runSteps (configLanguageExtensions conf) maybeFilePath (configSteps conf) $ lines contents


--------------------------------------------------------------------------------
-- | Searches Haskell source files in any given folder recursively.
-- Includes any extra extensions to add on top of the config.
findHaskellFiles :: Bool -> [FilePath] -> IO [(FilePath, [String])]
findHaskellFiles v fs = mapM (findFilesR v) fs >>= return . concat


--------------------------------------------------------------------------------
findFilesR :: Bool -> FilePath -> IO [(FilePath, [String])]
findFilesR _ []   = return []
findFilesR v path = do
  doesFileExist path >>= \case
    True -> return . maybeToList $ considerFile path
    _    -> doesDirectoryExist path >>= \case
      True  -> mapMaybe considerFile <$> findFilesRecursive path
      False -> do
        makeVerbose v ("Input folder does not exists: " <> path)
        findFilesR v []
  where
    findFilesRecursive :: FilePath -> IO [FilePath]
    findFilesRecursive = listDirectoryFiles findFilesRecursive

    listDirectoryFiles :: (FilePath -> IO [FilePath])
                       -> FilePath -> IO [FilePath]
    listDirectoryFiles go topdir = do
      ps <- listDirectory topdir >>=
        mapM (\x -> do
                 let dir = topdir </> x
                 doesDirectoryExist dir >>= \case
                   True  -> go dir
                   False -> return [dir])
      return $ concat ps

-- | Filter out files that can be formatted and also any extra extensions they may use.
-- Currently supported: .hs .hsc
considerFiles :: [FilePath] -> [(FilePath, [String])]
considerFiles = mapMaybe considerFile

considerFile :: FilePath -> Maybe (FilePath, [String])
considerFile x =
  case takeExtension x of
    ".hs"  -> Just (x, [])
    ".hsc" -> Just (x, ["CPP"])
    _      -> Nothing
