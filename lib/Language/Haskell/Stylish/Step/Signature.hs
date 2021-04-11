{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Step.Signature where

import           RdrName                          (RdrName)
import           SrcLoc                           (GenLocated (..), Located)
import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Types
import           GHC.Hs.Extension                 (GhcPs)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Module
import Language.Haskell.Stylish.Editor (change, noop)
import           Language.Haskell.Stylish.GHC (getStartLineUnsafe, getEndLineUnsafe, getEndColumnUnsafe)
import           Language.Haskell.Stylish.Editor (Change, applyChanges)
import           Language.Haskell.Stylish.Printer

-- TODO unify with type alias from Data.hs
type ChangeLine = Change String

data MaxColumns
  = MaxColumns !Int
  | NoMaxColumns
  deriving (Show, Eq)

fits :: Int -> MaxColumns -> Bool
fits _ NoMaxColumns = True
fits v (MaxColumns limit) = v <= limit

data Config = Config
  { cMaxColumns :: MaxColumns
  }

step :: Config -> Step
step cfg = makeStep "Signature" (\ls m -> applyChanges (changes cfg m) ls)

changes :: Config -> Module -> [ChangeLine]
changes cfg m = fmap (formatSignatureDecl cfg m) (topLevelFunctionSignatures m)

topLevelFunctionSignatures :: Module -> [Located SignatureDecl]
topLevelFunctionSignatures = queryModule @(Located (HsDecl GhcPs)) \case
  L pos (SigD _ (TypeSig _ [name] (HsWC _ (HsIB _ (L _ funTy@(HsFunTy _ _ _ )))))) ->
    [L pos $ MkSignatureDecl name (listParameters funTy) []]
  L pos (SigD _ (TypeSig _ [name] (HsWC _ (HsIB _ (L _ (HsQualTy _ (L _ contexts) (L _ funTy))))))) ->
    [L pos $ MkSignatureDecl name (listParameters funTy) (contexts >>= listContexts)]
  _ -> []

listParameters :: HsType GhcPs -> [Located RdrName]
listParameters (HsFunTy _ (L _ arg2) (L _ arg3)) = listParameters arg2 <> listParameters arg3
listParameters (HsTyVar _ _promotionFlag name) = [name]
listParameters _ = []

listContexts :: Located (HsType GhcPs) -> [Located RdrName]
listContexts (L _ (HsTyVar _ _ name)) = [name]
listContexts (L _ (HsAppTy _ arg1 arg2)) = listContexts arg1 <> listContexts arg2
listContexts _ = []

data SignatureDecl = MkSignatureDecl
  { sigName :: Located RdrName
  , sigParameters :: [Located RdrName]
  , sigConstraints :: [Located RdrName]
  }

formatSignatureDecl :: Config -> Module -> Located SignatureDecl -> ChangeLine
formatSignatureDecl cfg@Config{..} m ldecl@(L _ decl)
  | fits declLength cMaxColumns = noop block
  | otherwise = change block (const (printDecl cfg m decl))

  where
    block = Block (getStartLineUnsafe ldecl) (getEndLineUnsafe ldecl)
    declLength = getEndColumnUnsafe ldecl

printDecl :: Config -> Module -> SignatureDecl -> Lines
printDecl Config{..} m MkSignatureDecl{..} = runPrinter_ printerConfig [] m do
  printFirstLine
  printSecondLine
  printRemainingLines
  where

    ----------------------------------------------------------------------------------------

    printFirstLine =
      putRdrName sigName >> space >> putText "::" >> newline

    ----------------------------------------------------------------------------------------

    printSecondLine =
      if hasConstraints then printConstraints
      else printFirstParameter

    printConstraints =
        spaces 5 >> putText "("
        >> (traverse (\ctr -> printConstraint ctr >> putText ", ") (init groupConstraints))
        >> (printConstraint $ last groupConstraints)
        >> putText ")" >> newline

    groupConstraints = zip (dropEvery sigConstraints 2) (dropEvery (tail sigConstraints) 2)

    printConstraint (tc, tp) = putRdrName tc >> space >> putRdrName tp

    printFirstParameter =
        spaces 5 >> (putRdrName $ head sigParameters) >> newline

    ----------------------------------------------------------------------------------------

    printRemainingLines =
      if hasConstraints then
        printRemainingLine "=>" (head sigParameters)
        >> traverse (printRemainingLine "->") (tail sigParameters)
      else
        traverse (printRemainingLine "->") (tail sigParameters)

    printRemainingLine prefix parameter =
      spaces 2 >> putText prefix >> space >> (putRdrName parameter) >> newline

    ----------------------------------------------------------------------------------------

    printerConfig = PrinterConfig
      { columns = case cMaxColumns of
          NoMaxColumns -> Nothing
          MaxColumns n -> Just n
      }

    hasConstraints = not $ null sigConstraints

-- 99 problems :)
dropEvery :: [a] -> Int -> [a]
dropEvery xs n
  | length xs < n = xs
  | otherwise     = take (n-1) xs ++ dropEvery (drop n xs) n
