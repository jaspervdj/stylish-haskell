{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
-- | Utility functions for working with the GHC AST
module Language.Haskell.Stylish.GHC
  ( dropAfterLocated
  , dropBeforeLocated
  , dropBeforeAndAfter
    -- * Unsafe getters
  , unsafeGetRealSrcSpan
  , getEndLineUnsafe
  , getEndColumnUnsafe
  , getStartLineUnsafe
    -- * Standard settings
  , baseDynFlags
    -- * Positions
  , unLocated
    -- * Outputable operators
  , showOutputable
  , compareOutputable
  ) where

--------------------------------------------------------------------------------
import           Data.Function     (on)

--------------------------------------------------------------------------------
import           DynFlags          (Settings (..), defaultDynFlags)
import qualified DynFlags          as GHC
import           FileSettings      (FileSettings (..))
import           GHC.Fingerprint   (fingerprint0)
import           GHC.Platform
import           GHC.Version       (cProjectVersion)
import           GhcNameVersion    (GhcNameVersion (..))
import qualified Outputable        as GHC
import           PlatformConstants (PlatformConstants (..))
import           SrcLoc            (GenLocated (..), Located, RealLocated,
                                    RealSrcSpan, SrcSpan (..), srcSpanEndLine,
                                    srcSpanStartLine, srcSpanEndCol)
import           ToolSettings      (ToolSettings (..))

unsafeGetRealSrcSpan :: Located a -> RealSrcSpan
unsafeGetRealSrcSpan = \case
  (L (RealSrcSpan s) _) -> s
  _                     -> error "could not get source code location"

getStartLineUnsafe :: Located a -> Int
getStartLineUnsafe = srcSpanStartLine . unsafeGetRealSrcSpan

getEndLineUnsafe :: Located a -> Int
getEndLineUnsafe = srcSpanEndLine . unsafeGetRealSrcSpan

getEndColumnUnsafe :: Located a -> Int
getEndColumnUnsafe = srcSpanEndCol . unsafeGetRealSrcSpan

dropAfterLocated :: Maybe (Located a) -> [RealLocated b] -> [RealLocated b]
dropAfterLocated loc xs = case loc of
  Just (L (RealSrcSpan rloc) _) ->
    filter (\(L x _) -> srcSpanEndLine rloc >= srcSpanStartLine x) xs
  _ -> xs

dropBeforeLocated :: Maybe (Located a) -> [RealLocated b] -> [RealLocated b]
dropBeforeLocated loc xs = case loc of
  Just (L (RealSrcSpan rloc) _) ->
    filter (\(L x _) -> srcSpanStartLine rloc <= srcSpanEndLine x) xs
  _ -> xs

dropBeforeAndAfter :: Located a -> [RealLocated b] -> [RealLocated b]
dropBeforeAndAfter loc = dropBeforeLocated (Just loc) . dropAfterLocated (Just loc)

baseDynFlags :: GHC.DynFlags
baseDynFlags = defaultDynFlags fakeSettings llvmConfig
  where
    fakeSettings = GHC.Settings
      { sGhcNameVersion = GhcNameVersion "stylish-haskell" cProjectVersion
      , sFileSettings = FileSettings {}
      , sToolSettings = ToolSettings
        { toolSettings_opt_P_fingerprint = fingerprint0,
          toolSettings_pgm_F = ""
        }
      , sPlatformConstants = PlatformConstants
        { pc_DYNAMIC_BY_DEFAULT = False
        , pc_WORD_SIZE = 8
        }
      , sTargetPlatform = Platform
        { platformMini = PlatformMini
          { platformMini_arch = ArchUnknown
          , platformMini_os = OSUnknown
          }
        , platformWordSize = PW8
        , platformUnregisterised = True
        , platformHasIdentDirective = False
        , platformHasSubsectionsViaSymbols = False
        , platformIsCrossCompiling = False
        }
      , sPlatformMisc = PlatformMisc {}
      , sRawSettings = []
      }

    llvmConfig = GHC.LlvmConfig [] []

unLocated :: Located a -> a
unLocated (L _ a) = a

showOutputable :: GHC.Outputable a => a -> String
showOutputable = GHC.showPpr baseDynFlags

compareOutputable :: GHC.Outputable a => a -> a -> Ordering
compareOutputable = compare `on` showOutputable
