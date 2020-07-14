{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Module
  ( Module
  , ModuleHeader
  , Imports
  , Decls
  , Comments
  , Lines
  , moduleHeader
  , moduleImports
  , makeModule
  , moduleDecls
  , moduleComments

    -- * Internal API getters
  , rawComments
  , rawImports
  , rawModuleName
  , rawModuleExports
  , rawModuleHaddocks
  ) where

--------------------------------------------------------------------------------
import qualified ApiAnnotation                   as GHC
import           Data.Maybe                      (mapMaybe)
import qualified Lexer                           as GHC
import qualified GHC.Hs                          as GHC
import           GHC.Hs.Extension                (GhcPs)
import           GHC.Hs.Decls                    (LHsDecl)
import           GHC.Hs.ImpExp                   (LImportDecl)
import qualified SrcLoc                          as GHC
import qualified Module                          as GHC

--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete module type
data Module = Module
  { parsedComments :: [GHC.RealLocated GHC.AnnotationComment]
  , parsedModule :: GHC.Located (GHC.HsModule GhcPs)
  }

newtype Decls = Decls [LHsDecl GhcPs]

data Imports = Imports [LImportDecl GhcPs]

data Comments = Comments [GHC.RealLocated GHC.AnnotationComment]

data ModuleHeader = ModuleHeader
  { name :: Maybe (GHC.Located GHC.ModuleName)
  , exports :: Maybe (GHC.Located [GHC.LIE GhcPs])
  , haddocks :: Maybe GHC.LHsDocString
  }

makeModule :: GHC.PState -> GHC.Located (GHC.HsModule GHC.GhcPs) -> Module
makeModule pstate = Module comments
  where
    comments
      = filterRealLocated
      $ GHC.comment_q pstate ++ (GHC.annotations_comments pstate >>= snd)

    filterRealLocated = mapMaybe \case
      GHC.L (GHC.RealSrcSpan s) e -> Just (GHC.L s e)
      GHC.L (GHC.UnhelpfulSpan _) _ -> Nothing

moduleDecls :: Module -> Decls
moduleDecls = Decls . GHC.hsmodDecls . unLocated . parsedModule

moduleComments :: Module -> Comments
moduleComments = Comments . parsedComments

moduleImports :: Module -> Imports
moduleImports = Imports . GHC.hsmodImports . unLocated . parsedModule

moduleHeader :: Module -> ModuleHeader
moduleHeader (Module _ (GHC.L _ m)) = ModuleHeader
  { name = GHC.hsmodName m
  , exports = GHC.hsmodExports m
  , haddocks = GHC.hsmodHaddockModHeader m
  }

unLocated :: GHC.Located a -> a
unLocated (GHC.L _ a) = a

--------------------------------------------------------------------------------
-- | Getter for internal components in imports newtype
--
--   /Note:/ this function might be
rawImports :: Imports -> [LImportDecl GhcPs]
rawImports (Imports xs) = xs

rawModuleName :: ModuleHeader -> Maybe (GHC.Located GHC.ModuleName)
rawModuleName = name

rawModuleExports :: ModuleHeader -> Maybe (GHC.Located [GHC.LIE GhcPs])
rawModuleExports = exports

rawModuleHaddocks :: ModuleHeader -> Maybe GHC.LHsDocString
rawModuleHaddocks = haddocks

rawComments :: Comments -> [GHC.RealLocated GHC.AnnotationComment]
rawComments (Comments xs) = xs