{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Step.Data
  ( Config(..)
  , defaultConfig

  , Indent(..)
  , MaxColumns(..)
  , step
  ) where

--------------------------------------------------------------------------------
import           Prelude                          hiding (init)

--------------------------------------------------------------------------------
import           Control.Monad                    (forM_, unless, when)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import           Data.List                        (sortBy)
import           Data.Maybe                       (listToMaybe)

--------------------------------------------------------------------------------
import           ApiAnnotation                    (AnnotationComment)
import           BasicTypes                       (LexicalFixity (..))
import           GHC.Hs.Decls                     (ConDecl (..),
                                                   DerivStrategy (..),
                                                   HsDataDefn (..), HsDecl (..),
                                                   HsDerivingClause (..),
                                                   NewOrData (..),
                                                   TyClDecl (..))
import           GHC.Hs.Extension                 (GhcPs, NoExtField (..),
                                                   noExtCon)
import           GHC.Hs.Types                     (ConDeclField (..),
                                                   ForallVisFlag (..),
                                                   HsConDetails (..), HsContext,
                                                   HsImplicitBndrs (..),
                                                   HsTyVarBndr (..),
                                                   HsType (..), LHsQTyVars (..))
import           RdrName                          (RdrName)
import           SrcLoc                           (GenLocated (..), Located,
                                                   RealLocated)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step

data Indent
    = SameLine
    | Indent !Int
  deriving (Show, Eq)

data MaxColumns
  = MaxColumns !Int
  | NoMaxColumns
  deriving (Show, Eq)

data Config = Config
    { cEquals                  :: !Indent
      -- ^ Indent between type constructor and @=@ sign (measured from column 0)
    , cFirstField              :: !Indent
      -- ^ Indent between data constructor and @{@ line (measured from column with data constructor name)
    , cFieldComment            :: !Int
      -- ^ Indent between column with @{@ and start of field line comment (this line has @cFieldComment = 2@)
    , cDeriving                :: !Int
      -- ^ Indent before @deriving@ lines (measured from column 0)
    , cBreakEnums              :: !Bool
      -- ^ Break enums by newlines and follow the above rules
    , cBreakNewtypes           :: !Bool
      -- ^ Break newtypes by newlines and follow the above rules
    , cBreakSingleConstructors :: !Bool
      -- ^ Break single constructors when enabled, e.g. @Indent 2@ will not cause newline after @=@
    , cVia                     :: !Indent
      -- ^ Indentation between @via@ clause and start of deriving column start
    , cCurriedContext          :: !Bool
      -- ^ If true, use curried context. E.g: @allValues :: Enum a => Bounded a => Proxy a -> [a]@
    , cSortDeriving            :: !Bool
      -- ^ If true, will sort type classes in a @deriving@ list.
    , cMaxColumns              :: !MaxColumns
    } deriving (Show)

-- | TODO: pass in MaxColumns?
defaultConfig :: Config
defaultConfig = Config
    { cEquals          = Indent 4
    , cFirstField      = Indent 4
    , cFieldComment    = 2
    , cDeriving        = 4
    , cBreakEnums      = True
    , cBreakNewtypes   = True
    , cBreakSingleConstructors = False
    , cVia             = Indent 4
    , cSortDeriving    = True
    , cMaxColumns      = NoMaxColumns
    , cCurriedContext    = False
    }

step :: Config -> Step
step cfg = makeStep "Data" \ls m -> applyChanges (changes m) ls
  where
    changes :: Module -> [ChangeLine]
    changes m = fmap (formatDataDecl cfg m) (dataDecls m)

    dataDecls :: Module -> [Located DataDecl]
    dataDecls = queryModule \case
      L pos (TyClD _ (DataDecl _ name tvars fixity defn)) -> pure . L pos $ MkDataDecl
        { dataDeclName = name
        , dataTypeVars = tvars
        , dataDefn = defn
        , dataFixity = fixity
        }
      _ -> []

type ChangeLine = Change String

formatDataDecl :: Config -> Module -> Located DataDecl -> ChangeLine
formatDataDecl cfg@Config{..} m ldecl@(L declPos decl) =
  change originalDeclBlock (const printedDecl)
  where
    relevantComments :: [RealLocated AnnotationComment]
    relevantComments
      = moduleComments m
      & rawComments
      & dropBeforeAndAfter ldecl

    defn = dataDefn decl

    originalDeclBlock =
      Block (getStartLineUnsafe ldecl) (getEndLineUnsafe ldecl)

    printerConfig = PrinterConfig
      { columns = case cMaxColumns of
          NoMaxColumns -> Nothing
          MaxColumns n -> Just n
      }

    printedDecl = runPrinter_ printerConfig relevantComments m do
      putText (newOrData decl)
      space
      putName decl

      when (isGADT decl) (space >> putText "where")

      when (hasConstructors decl) do
        breakLineBeforeEq <- case (cEquals, cFirstField) of
          (_, Indent x) | isEnum decl && cBreakEnums -> do
            putEolComment declPos
            newline >> spaces x
            pure True
          (_, _) | if isNewtype decl then not cBreakNewtypes else singleConstructor decl && not cBreakSingleConstructors ->
            False <$ space
          (Indent x, _)
            | isEnum decl && not cBreakEnums -> False <$ space
            | otherwise -> do
              putEolComment declPos
              newline >> spaces x
              pure True
          (SameLine, _) -> False <$ space

        lineLengthAfterEq <- fmap (+2) getCurrentLineLength

        if isEnum decl && not cBreakEnums then
          putText "=" >> space >> putUnbrokenEnum cfg decl
        else if isNewtype decl then
          putText "=" >> space >> forM_ (dd_cons defn) (putNewtypeConstructor cfg)
        else
          case dd_cons defn of
            [] -> pure ()
            lcon@(L pos _) : consRest -> do
              when breakLineBeforeEq do
                removeCommentTo pos >>= mapM_ \c -> putComment c >> consIndent lineLengthAfterEq

              unless
                (isGADT decl)
                (putText "=" >> space)

              putConstructor cfg lineLengthAfterEq lcon
              forM_ consRest \con@(L conPos _) -> do
                unless (cFirstField == SameLine) do
                  removeCommentTo conPos >>= mapM_ \c -> consIndent lineLengthAfterEq >> putComment c
                consIndent lineLengthAfterEq

                unless
                  (isGADT decl)
                  (putText "|" >> space)

                putConstructor cfg lineLengthAfterEq con
                putEolComment conPos

        when (hasDeriving decl) do
          if isEnum decl && not cBreakEnums then
            space
          else do
            removeCommentTo (defn & dd_derivs & \(L pos _) -> pos) >>=
              mapM_ \c -> newline >> spaces cDeriving >> putComment c
            newline
            spaces cDeriving

        sep (newline >> spaces cDeriving) $ defn & dd_derivs & \(L pos ds) -> ds <&> \d -> do
          putAllSpanComments (newline >> spaces cDeriving) pos
          putDeriving cfg d

    consIndent eqIndent = newline >> case (cEquals, cFirstField) of
      (SameLine, SameLine) -> spaces (eqIndent - 2)
      (SameLine, Indent y) -> spaces (eqIndent + y - 4)
      (Indent x, Indent _) -> spaces x
      (Indent x, SameLine) -> spaces x

data DataDecl = MkDataDecl
  { dataDeclName :: Located RdrName
  , dataTypeVars :: LHsQTyVars GhcPs
  , dataDefn     :: HsDataDefn GhcPs
  , dataFixity   :: LexicalFixity
  }

putDeriving :: Config -> Located (HsDerivingClause GhcPs) -> P ()
putDeriving Config{..} (L pos clause) = do
  putText "deriving"

  forM_ (deriv_clause_strategy clause) \case
    L _ StockStrategy    -> space >> putText "stock"
    L _ AnyclassStrategy -> space >> putText "anyclass"
    L _ NewtypeStrategy  -> space >> putText "newtype"
    L _ (ViaStrategy _)  -> pure ()

  putCond
    withinColumns
    oneLinePrint
    multilinePrint

  forM_ (deriv_clause_strategy clause) \case
    L _ (ViaStrategy tp) -> do
      case cVia of
        SameLine -> space
        Indent x -> newline >> spaces (x + cDeriving)

      putText "via"
      space
      putType (getType tp)
    _ -> pure ()

  putEolComment pos

  where
    getType = \case
      HsIB _ tp          -> tp
      XHsImplicitBndrs x -> noExtCon x

    withinColumns PrinterState{currentLine} =
      case cMaxColumns of
        MaxColumns maxCols -> length currentLine <= maxCols
        NoMaxColumns       -> True

    oneLinePrint = do
      space
      putText "("
      sep
        (comma >> space)
        (fmap putOutputable tys)
      putText ")"

    multilinePrint = do
      newline
      spaces indentation
      putText "("

      forM_ headTy \t ->
        space >> putOutputable t

      forM_ tailTy \t -> do
        newline
        spaces indentation
        comma
        space
        putOutputable t

      newline
      spaces indentation
      putText ")"

    indentation =
      cDeriving + case cFirstField of
        Indent x -> x
        SameLine -> 0

    tys
      = clause
      & deriv_clause_tys
      & unLocated
      & (if cSortDeriving then sortBy compareOutputable else id)
      & fmap hsib_body

    headTy =
      listToMaybe tys

    tailTy =
      drop 1 tys

putUnbrokenEnum :: Config -> DataDecl -> P ()
putUnbrokenEnum cfg decl =
  sep
    (space >> putText "|" >> space)
    (fmap (putConstructor cfg 0) . dd_cons . dataDefn $ decl)

putName :: DataDecl -> P ()
putName decl@MkDataDecl{..} =
  if isInfix decl then do
    forM_ firstTvar (\t -> putOutputable t >> space)
    putRdrName dataDeclName
    space
    forM_ secondTvar putOutputable
  else do
    putRdrName dataDeclName
    forM_ (hsq_explicit dataTypeVars) (\t -> space >> putOutputable t)

  where
    firstTvar :: Maybe (Located (HsTyVarBndr GhcPs))
    firstTvar
      = dataTypeVars
      & hsq_explicit
      & listToMaybe

    secondTvar :: Maybe (Located (HsTyVarBndr GhcPs))
    secondTvar
      = dataTypeVars
      & hsq_explicit
      & drop 1
      & listToMaybe

putConstructor :: Config -> Int -> Located (ConDecl GhcPs) -> P ()
putConstructor cfg consIndent (L _ cons) = case cons of
  ConDeclGADT{..} -> do
    -- Put argument to constructor first:
    case con_args of
      PrefixCon _ -> do
        sep
          (comma >> space)
          (fmap putRdrName con_names)

      InfixCon arg1 arg2 -> do
        putType arg1
        space
        forM_ con_names putRdrName
        space
        putType arg2
      RecCon _ ->
        error . mconcat $
          [ "Language.Haskell.Stylish.Step.Data.putConstructor: "
          , "encountered a GADT with record constructors, not supported yet"
          ]

    -- Put type of constructor:
    space
    putText "::"
    space

    putForAll con_forall $ hsq_explicit con_qvars
    forM_ con_mb_cxt (putContext cfg . unLocated)
    putType con_res_ty

  XConDecl x ->
    noExtCon x
  ConDeclH98{..} -> do
    putForAll con_forall con_ex_tvs
    forM_ con_mb_cxt (putContext cfg . unLocated)
    case con_args of
      InfixCon arg1 arg2 -> do
        putType arg1
        space
        putRdrName con_name
        space
        putType arg2
      PrefixCon xs -> do
        putRdrName con_name
        unless (null xs) space
        sep space (fmap putOutputable xs)
      RecCon (L recPos (L posFirst firstArg : args)) -> do
        putRdrName con_name
        skipToBrace
        bracePos <- getCurrentLineLength
        putText "{"
        let fieldPos = bracePos + 2
        space

        -- Unless everything's configured to be on the same line, put pending
        -- comments
        unless (cFirstField cfg == SameLine) do
          removeCommentTo posFirst >>= mapM_ \c -> putComment c >> sepDecl bracePos

        -- Put first decl field
        pad fieldPos >> putConDeclField cfg firstArg
        unless (cFirstField cfg == SameLine) (putEolComment posFirst)

        -- Put tail decl fields
        forM_ args \(L pos arg) -> do
          sepDecl bracePos
          removeCommentTo pos >>= mapM_ \c ->
            spaces (cFieldComment cfg) >> putComment c >> sepDecl bracePos
          comma
          space
          putConDeclField cfg arg
          putEolComment pos

        -- Print docstr after final field
        removeCommentToEnd recPos >>= mapM_ \c ->
          sepDecl bracePos >> spaces (cFieldComment cfg) >> putComment c

        -- Print whitespace to closing brace
        sepDecl bracePos >> putText "}"
      RecCon (L _ []) -> do
        skipToBrace >> putText "{"
        skipToBrace >> putText "}"

    where
      -- Jump to the first brace of the first record of the first constructor.
      skipToBrace = case (cEquals cfg, cFirstField cfg) of
        (_, Indent y) | not (cBreakSingleConstructors cfg) -> newline >> spaces y
        (SameLine, SameLine) -> space
        (Indent x, Indent y) -> newline >> spaces (x + y + 2)
        (SameLine, Indent y) -> newline >> spaces (consIndent + y)
        (Indent _, SameLine) -> space

      -- Jump to the next declaration.
      sepDecl bracePos = newline >> spaces case (cEquals cfg, cFirstField cfg) of
        (_, Indent y) | not (cBreakSingleConstructors cfg) -> y
        (SameLine, SameLine) -> bracePos
        (Indent x, Indent y) -> x + y + 2
        (SameLine, Indent y) -> bracePos + y - 2
        (Indent x, SameLine) -> bracePos + x - 2

putNewtypeConstructor :: Config -> Located (ConDecl GhcPs) -> P ()
putNewtypeConstructor cfg (L _ cons) = case cons of
  ConDeclH98{..} ->
    putRdrName con_name >> case con_args of
      PrefixCon xs -> do
        unless (null xs) space
        sep space (fmap putOutputable xs)
      RecCon (L _ [L _posFirst firstArg]) -> do
        space
        putText "{"
        space
        putConDeclField cfg firstArg
        space
        putText "}"
      RecCon (L _ _args) ->
        error . mconcat $
          [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
          , "encountered newtype with several arguments"
          ]
      InfixCon {} ->
        error . mconcat $
          [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
          , "infix newtype constructor"
          ]
  XConDecl x ->
    noExtCon x
  ConDeclGADT{} ->
    error . mconcat $
      [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
      , "GADT encountered in newtype"
      ]

putForAll :: Located Bool -> [Located (HsTyVarBndr GhcPs)] -> P ()
putForAll forall ex_tvs =
  when (unLocated forall) do
    putText "forall"
    space
    sep space (fmap putOutputable ex_tvs)
    dot
    space

putContext :: Config -> HsContext GhcPs -> P ()
putContext Config{..} = suffix (space >> putText "=>" >> space) . \case
  [L _ (HsParTy _ tp)] | cCurriedContext ->
    putType tp
  [ctx] ->
    putType ctx
  ctxs | cCurriedContext ->
    sep (space >> putText "=>" >> space) (fmap putType ctxs)
  ctxs ->
    parenthesize $ sep (comma >> space) (fmap putType ctxs)

putConDeclField :: Config -> ConDeclField GhcPs -> P ()
putConDeclField cfg = \case
  ConDeclField{..} -> do
    sep
      (comma >> space)
      (fmap putOutputable cd_fld_names)
    space
    putText "::"
    space
    putType' cfg cd_fld_type
  XConDeclField{} ->
    error . mconcat $
      [ "Language.Haskell.Stylish.Step.Data.putConDeclField: "
      , "XConDeclField encountered"
      ]

-- | A variant of 'putType' that takes 'cCurriedContext' into account
putType' :: Config -> Located (HsType GhcPs) -> P ()
putType' cfg = \case
  L _ (HsForAllTy NoExtField vis bndrs tp) -> do
    putText "forall"
    space
    sep space (fmap putOutputable bndrs)
    putText
      if vis == ForallVis then "->"
      else "."
    space
    putType' cfg tp
  L _ (HsQualTy NoExtField ctx tp) -> do
    putContext cfg (unLocated ctx)
    putType' cfg tp
  other -> putType other

newOrData :: DataDecl -> String
newOrData decl = if isNewtype decl then "newtype" else "data"

isGADT :: DataDecl -> Bool
isGADT = any isGADTCons . dd_cons . dataDefn
  where
    isGADTCons = \case
      L _ (ConDeclGADT {}) -> True
      _                    -> False

isNewtype :: DataDecl -> Bool
isNewtype = (== NewType) . dd_ND . dataDefn

isInfix :: DataDecl -> Bool
isInfix = (== Infix) . dataFixity

isEnum :: DataDecl -> Bool
isEnum = all isUnary . dd_cons . dataDefn
  where
    isUnary = \case
      L _ (ConDeclH98 {..}) -> case con_args of
        PrefixCon [] -> True
        _            -> False
      _ -> False

hasConstructors :: DataDecl -> Bool
hasConstructors = not . null . dd_cons . dataDefn

singleConstructor :: DataDecl -> Bool
singleConstructor = (== 1) . length . dd_cons . dataDefn

hasDeriving :: DataDecl -> Bool
hasDeriving = not . null . unLocated . dd_derivs . dataDefn
