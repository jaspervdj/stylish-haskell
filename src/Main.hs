{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad            (forM_, unless, when)
import qualified Data.ByteString.Char8    as BC8
import           Data.List                (nub)
import           Data.Version             (showVersion)
import qualified Options.Applicative      as OA
import           System.Exit              (exitFailure)
import qualified System.IO                as IO
import qualified System.IO.Strict         as IO.Strict

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ < 808
import           Data.Monoid              ((<>))
#endif

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish


--------------------------------------------------------------------------------
data StylishArgs = StylishArgs
    { saVersion   :: Bool
    , saConfig    :: Maybe FilePath
    , saRecursive :: Bool
    , saVerbose   :: Bool
    , saDefaults  :: Bool
    , saInPlace   :: Bool
    , saNoUtf8    :: Bool
    , saFiles     :: [FilePath]
    } deriving (Show)


--------------------------------------------------------------------------------
parseStylishArgs :: OA.Parser StylishArgs
parseStylishArgs = StylishArgs
    <$> OA.switch (
            OA.help  "Show version information" <>
            OA.long  "version"                  <>
            OA.hidden)
    <*> OA.optional (OA.strOption $
            OA.metavar "CONFIG"              <>
            OA.help    "Configuration file"  <>
            OA.long    "config"              <>
            OA.short   'c'                   <>
            OA.hidden)
    <*> OA.switch (
            OA.help    "Recursive file search" <>
            OA.long    "recursive"             <>
            OA.short   'r'                     <>
            OA.hidden)
    <*> OA.switch (
            OA.help  "Run in verbose mode" <>
            OA.long  "verbose"             <>
            OA.short 'v'                   <>
            OA.hidden)
    <*> OA.switch (
            OA.help  "Dump default config and exit" <>
            OA.long  "defaults"                     <>
            OA.short 'd'                            <>
            OA.hidden)
    <*> OA.switch (
            OA.help  "Overwrite the given files in place" <>
            OA.long  "inplace"                            <>
            OA.short 'i'                                  <>
            OA.hidden)
    <*> OA.switch (
            OA.help  "Don't force UTF-8 stdin/stdout" <>
            OA.long  "no-utf8"                        <>
            OA.hidden)
    <*> OA.many (OA.strArgument $
            OA.metavar "FILENAME" <>
            OA.help    "Input file(s)")


--------------------------------------------------------------------------------
stylishHaskellVersion :: String
stylishHaskellVersion = "stylish-haskell " <> showVersion version


--------------------------------------------------------------------------------
parserInfo :: OA.ParserInfo StylishArgs
parserInfo = OA.info (OA.helper <*> parseStylishArgs) $
    OA.fullDesc <>
    OA.header stylishHaskellVersion


--------------------------------------------------------------------------------
main :: IO ()
main = OA.execParser parserInfo >>= stylishHaskell


--------------------------------------------------------------------------------
stylishHaskell :: StylishArgs -> IO ()
stylishHaskell sa = do
    unless (saNoUtf8 sa) $
        mapM_ (`IO.hSetEncoding` IO.utf8) [IO.stdin, IO.stdout]
    if saVersion sa then
        putStrLn stylishHaskellVersion

        else if saDefaults sa then do
            verbose' "Dumping embedded config..."
            BC8.putStr defaultConfigBytes

        else do
            conf <- loadConfig verbose' (saConfig sa)
            filesR <- case (saRecursive sa) of
              True -> findHaskellFiles (saVerbose sa) (saFiles sa)
              _    -> return $ considerFiles $ saFiles sa
            let steps = configSteps conf
            forM_ steps $ \s -> verbose' $ "Enabled " ++ stepName s ++ " step"
            verbose' $ "Extra language extensions: " ++
                show (configLanguageExtensions conf)
            res <- case files' filesR of
              Nothing -> file sa conf Nothing
              Just xs -> foldMap (\(fp, exts) -> file sa (extend conf exts) (Just fp)) xs

            verbose' $ "Exit code behavior: " ++ show (configExitCode conf)
            when (configExitCode conf == ErrorOnFormatExitBehavior && res == DidFormat) exitFailure
  where
    verbose' = makeVerbose (saVerbose sa)
    extend conf exts = conf { configLanguageExtensions = nub $ exts <> configLanguageExtensions conf }
    files' x = case (saRecursive sa, null x) of
      (True,True) -> Just [] -- No file to format and recursive enabled.
      (_,True)    -> Nothing -- Involving IO.stdin.
      (_,False)   -> Just x  -- Process available files.

data FormattingResult
  = DidFormat
  | NoChange
  deriving (Eq)

instance Semigroup FormattingResult where
  _ <> DidFormat = DidFormat
  DidFormat <> _ = DidFormat
  _ <> _ = NoChange

instance Monoid FormattingResult where
  mempty = NoChange

--------------------------------------------------------------------------------
-- | Processes a single file, or stdin if no filepath is given
file :: StylishArgs -> Config -> Maybe FilePath -> IO FormattingResult
file sa conf mfp = do
    contents <- maybe getContents readUTF8File mfp
    let
      inputLines =
        lines contents
      result =
        runSteps (configLanguageExtensions conf) mfp (configSteps conf) inputLines
    case result of
        Right ok  -> do
            write contents (unlines ok)
            pure $ if ok /= inputLines then DidFormat else NoChange
        Left  err -> do
            IO.hPutStrLn IO.stderr err
            exitFailure
  where
    write old new = case mfp of
                Nothing -> putStrNewline new
                Just _    | not (saInPlace sa) -> putStrNewline new
                Just path | not (null new) && old /= new  ->
                    IO.withFile path IO.WriteMode $ \h -> do
                        setNewlineMode h
                        IO.hPutStr h new
                _ -> return ()
    setNewlineMode h = do
      let nl = configNewline conf
      let mode = IO.NewlineMode IO.nativeNewline nl
      IO.hSetNewlineMode h mode
    putStrNewline txt = setNewlineMode IO.stdout >> putStr txt

readUTF8File :: FilePath -> IO String
readUTF8File fp =
     IO.withFile fp IO.ReadMode $ \h -> do
        IO.hSetEncoding h IO.utf8
        IO.Strict.hGetContents h
