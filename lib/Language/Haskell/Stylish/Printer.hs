{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Printer
  ( Printer(..)
  , PrinterConfig(..)
  , PrinterState(..)

    -- * Alias
  , P

    -- * Functions to use the printer
  , runPrinter
  , runPrinter_

    -- ** Combinators
  , comma
  , dot
  , getAnnot
  , getCurrentLine
  , getCurrentLineLength
  , getDocstrPrev
  , newline
  , parenthesize
  , peekNextCommentPos
  , prefix
  , putComment
  , putEolComment
  , putOutputable
  , putAllSpanComments
  , putCond
  , putType
  , putRdrName
  , putText
  , removeCommentTo
  , removeCommentToEnd
  , removeLineComment
  , sep
  , groupAttachedComments
  , groupWithoutComments
  , space
  , spaces
  , suffix
  , pad

    -- ** Advanced combinators
  , withColumns
  , modifyCurrentLine
  , wrapping
  ) where

--------------------------------------------------------------------------------
import           Prelude                         hiding (lines)

--------------------------------------------------------------------------------
import           ApiAnnotation                   (AnnKeywordId(..), AnnotationComment(..))
import           BasicTypes                      (PromotionFlag(..))
import           GHC.Hs.Extension                (GhcPs, NoExtField(..))
import           GHC.Hs.Types                    (HsType(..))
import           Module                          (ModuleName, moduleNameString)
import           RdrName                         (RdrName(..))
import           SrcLoc                          (GenLocated(..), RealLocated)
import           SrcLoc                          (Located, SrcSpan(..))
import           SrcLoc                          (srcSpanStartLine, srcSpanEndLine)
import           Outputable                      (Outputable)

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, replicateM_)
import           Control.Monad.Reader            (MonadReader, ReaderT(..), asks, local)
import           Control.Monad.State             (MonadState, State)
import           Control.Monad.State             (runState)
import           Control.Monad.State             (get, gets, modify, put)
import           Data.Foldable                   (find, toList)
import           Data.Functor                    ((<&>))
import           Data.List                       (delete, isPrefixOf)
import           Data.List.NonEmpty              (NonEmpty(..))

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Module (Module, Lines, lookupAnnotation)
import           Language.Haskell.Stylish.GHC    (showOutputable, unLocated)

-- | Shorthand for 'Printer' monad
type P = Printer

-- | Printer that keeps state of file
newtype Printer a = Printer (ReaderT PrinterConfig (State PrinterState) a)
  deriving (Applicative, Functor, Monad, MonadReader PrinterConfig, MonadState PrinterState)

-- | Configuration for printer, currently empty
data PrinterConfig = PrinterConfig
    { columns :: !(Maybe Int)
    }

-- | State of printer
data PrinterState = PrinterState
  { lines :: !Lines
  , linePos :: !Int
  , currentLine :: !String
  , pendingComments :: ![RealLocated AnnotationComment]
  , parsedModule :: !Module
  }

-- | Run printer to get printed lines out of module as well as return value of monad
runPrinter :: PrinterConfig -> [RealLocated AnnotationComment] -> Module -> Printer a -> (a, Lines)
runPrinter cfg comments m (Printer printer) =
  let
    (a, PrinterState parsedLines _ startedLine _ _) = runReaderT printer cfg `runState` PrinterState [] 0 "" comments m
  in
    (a, parsedLines <> if startedLine == [] then [] else [startedLine])

-- | Run printer to get printed lines only
runPrinter_ :: PrinterConfig -> [RealLocated AnnotationComment] -> Module -> Printer a -> Lines
runPrinter_ cfg comments m printer = snd (runPrinter cfg comments m printer)

-- | Print text
putText :: String -> P ()
putText txt = do
  l <- gets currentLine
  modify \s -> s { currentLine = l <> txt }

-- | Check condition post action, and use fallback if false
putCond :: (PrinterState -> Bool) -> P b -> P b -> P b
putCond p action fallback = do
  prevState <- get
  res <- action
  currState <- get
  if p currState then pure res
  else put prevState >> fallback

-- | Print an 'Outputable'
putOutputable :: Outputable a => a -> P ()
putOutputable = putText . showOutputable

-- | Put all comments that has positions within 'SrcSpan' and separate by
--   passed @P ()@
--
-- 1. Gets all comments associated with a source span
-- 2. Prints them
--
-- Separated into getSpanComments / putAllSpanComments
putAllSpanComments :: P () -> SrcSpan -> P ()
putAllSpanComments suff = \case
  UnhelpfulSpan _ -> pure ()
  RealSrcSpan rspan -> do
    cmts <- removeComments \(L rloc _) ->
      srcSpanStartLine rloc >= srcSpanStartLine rspan &&
      srcSpanEndLine rloc <= srcSpanEndLine rspan

    forM_ cmts (\c -> putComment c >> suff)

-- | Print any comment
putComment :: AnnotationComment -> P ()
putComment = \case
  AnnLineComment s -> putText s
  AnnDocCommentNext s -> putText s
  AnnDocCommentPrev s -> putText s
  AnnDocCommentNamed s -> putText s
  AnnDocSection _ s -> putText s
  AnnDocOptions s -> putText s
  AnnBlockComment s -> putText s

-- | Given the current start line of 'SrcSpan', remove and put EOL comment for same line
putEolComment :: SrcSpan -> P ()
putEolComment = \case
  RealSrcSpan rspan -> do
    cmt <- removeComment \case
      L rloc (AnnLineComment s) ->
        and
          [ srcSpanStartLine rspan == srcSpanStartLine rloc
          , not ("-- ^" `isPrefixOf` s)
          , not ("-- |" `isPrefixOf` s)
          ]
      _ -> False
    forM_ cmt (\c -> space >> putComment c)
  UnhelpfulSpan _ -> pure ()

-- | Print a 'RdrName'
putRdrName :: Located RdrName -> P ()
putRdrName (L pos n) = case n of
  Unqual name -> do
    annots <- getAnnot pos
    if AnnOpenP `elem` annots then do
      putText "("
      putText (showOutputable name)
      putText ")"
    else if AnnBackquote `elem` annots then do
      putText "`"
      putText (showOutputable name)
      putText "`"
    else if AnnSimpleQuote `elem` annots then do
      putText "'"
      putText (showOutputable name)
    else
      putText (showOutputable name)
  Qual modulePrefix name ->
    putModuleName modulePrefix >> dot >> putText (showOutputable name)
  Orig _ name ->
    putText (showOutputable name)
  Exact name ->
    putText (showOutputable name)

-- | Print module name
putModuleName :: ModuleName -> P ()
putModuleName = putText . moduleNameString

-- | Print type
putType :: Located (HsType GhcPs) -> P ()
putType ltp = case unLocated ltp of
  HsFunTy NoExtField argTp funTp -> do
    putOutputable argTp
    space
    putText "->"
    space
    putType funTp
  HsAppTy NoExtField t1 t2 ->
    putType t1 >> space >> putType t2
  HsExplicitListTy NoExtField _ xs -> do
    putText "'["
    sep
      (comma >> space)
      (fmap putType xs)
    putText "]"
  HsExplicitTupleTy NoExtField xs -> do
    putText "'("
    sep
      (comma >> space)
      (fmap putType xs)
    putText ")"
  HsOpTy NoExtField lhs op rhs -> do
    putType lhs
    space
    putRdrName op
    space
    putType rhs
  HsTyVar NoExtField flag rdrName -> do
    case flag of
      IsPromoted  -> putText "'"
      NotPromoted -> pure ()
    putRdrName rdrName
  HsTyLit _ tp ->
    putOutputable tp
  HsParTy _ tp -> do
    putText "("
    putType tp
    putText ")"
  HsTupleTy NoExtField _ xs -> do
    putText "("
    sep
      (comma >> space)
      (fmap putType xs)
    putText ")"
  HsForAllTy NoExtField _ _ _ ->
    putOutputable ltp
  HsQualTy NoExtField _ _ ->
    putOutputable ltp
  HsAppKindTy _ _ _ ->
    putOutputable ltp
  HsListTy _ _ ->
    putOutputable ltp
  HsSumTy _ _ ->
    putOutputable ltp
  HsIParamTy _ _ _ ->
    putOutputable ltp
  HsKindSig _ _ _ ->
    putOutputable ltp
  HsStarTy _ _ ->
    putOutputable ltp
  HsSpliceTy _ _ ->
    putOutputable ltp
  HsDocTy _ _ _ ->
    putOutputable ltp
  HsBangTy _ _ _ ->
    putOutputable ltp
  HsRecTy _ _ ->
    putOutputable ltp
  HsWildCardTy _ ->
    putOutputable ltp
  XHsType _ ->
    putOutputable ltp

-- | Get a docstring on the start line of 'SrcSpan' that is a @-- ^@ comment
getDocstrPrev :: SrcSpan -> P (Maybe AnnotationComment)
getDocstrPrev = \case
  UnhelpfulSpan _ -> pure Nothing
  RealSrcSpan rspan -> do
    removeComment \case
      L rloc (AnnLineComment s) ->
        and
          [ srcSpanStartLine rspan == srcSpanStartLine rloc
          , "-- ^" `isPrefixOf` s
          ]
      _ -> False

-- | Print a newline
newline :: P ()
newline = do
  l <- gets currentLine
  modify \s -> s { currentLine = "", linePos = 0, lines = lines s <> [l] }

-- | Print a space
space :: P ()
space = putText " "

-- | Print a number of spaces
spaces :: Int -> P ()
spaces i = replicateM_ i space

-- | Print a dot
dot :: P ()
dot = putText "."

-- | Print a comma
comma :: P ()
comma = putText ","

-- | Add parens around a printed action
parenthesize :: P a -> P a
parenthesize action = putText "(" *> action <* putText ")"

-- | Add separator between each element of the given printers
sep :: P a -> [P a] -> P ()
sep _ [] = pure ()
sep s (first : rest) = first >> forM_ rest ((>>) s)

-- | Prefix a printer with another one
prefix :: P a -> P b -> P b
prefix pa pb = pa >> pb

-- | Suffix a printer with another one
suffix :: P a -> P b -> P a
suffix pa pb = pb >> pa

-- | Indent to a given number of spaces.  If the current line already exceeds
-- that number in length, nothing happens.
pad :: Int -> P ()
pad n = do
    len <- length <$> getCurrentLine
    spaces $ n - len

-- | Gets comment on supplied 'line' and removes it from the state
removeLineComment :: Int -> P (Maybe AnnotationComment)
removeLineComment line =
  removeComment (\(L rloc _) -> srcSpanStartLine rloc == line)

-- | Removes comments from the state up to start line of 'SrcSpan' and returns
--   the ones that were removed
removeCommentTo :: SrcSpan -> P [AnnotationComment]
removeCommentTo = \case
  UnhelpfulSpan _ -> pure []
  RealSrcSpan rspan -> removeCommentTo' (srcSpanStartLine rspan)

-- | Removes comments from the state up to end line of 'SrcSpan' and returns
--   the ones that were removed
removeCommentToEnd :: SrcSpan -> P [AnnotationComment]
removeCommentToEnd = \case
  UnhelpfulSpan _ -> pure []
  RealSrcSpan rspan -> removeCommentTo' (srcSpanEndLine rspan)

-- | Removes comments to the line number given and returns the ones removed
removeCommentTo' :: Int -> P [AnnotationComment]
removeCommentTo' line =
  removeComment (\(L rloc _) -> srcSpanStartLine rloc < line) >>= \case
    Nothing -> pure []
    Just c -> do
      rest <- removeCommentTo' line
      pure (c : rest)

-- | Removes comments from the state while given predicate 'p' is true
removeComments :: (RealLocated AnnotationComment -> Bool) -> P [AnnotationComment]
removeComments p =
  removeComment p >>= \case
    Just c -> do
      rest <- removeComments p
      pure (c : rest)
    Nothing -> pure []

-- | Remove a comment from the state given predicate 'p'
removeComment :: (RealLocated AnnotationComment -> Bool) -> P (Maybe AnnotationComment)
removeComment p = do
  comments <- gets pendingComments

  let
    foundComment =
      find p comments

    newPendingComments =
      maybe comments (`delete` comments) foundComment

  modify \s -> s { pendingComments = newPendingComments }
  pure $ fmap (\(L _ c) -> c) foundComment

-- | Get all annotations for 'SrcSpan'
getAnnot :: SrcSpan -> P [AnnKeywordId]
getAnnot spn = gets (lookupAnnotation spn . parsedModule)

-- | Get current line
getCurrentLine :: P String
getCurrentLine = gets currentLine

-- | Get current line length
getCurrentLineLength :: P Int
getCurrentLineLength = fmap length getCurrentLine

-- | Peek at the next comment in the state
peekNextCommentPos :: P (Maybe SrcSpan)
peekNextCommentPos = do
  gets pendingComments <&> \case
    (L next _ : _) -> Just (RealSrcSpan next)
    [] -> Nothing

-- | Get attached comments belonging to '[Located a]' given
groupAttachedComments :: [Located a] -> P [([AnnotationComment], NonEmpty (Located a))]
groupAttachedComments = go
  where
    go :: [Located a] -> P [([AnnotationComment], NonEmpty (Located a))]
    go (L rspan x : xs) = do
      comments <- removeCommentTo rspan
      nextGroupStartM <- peekNextCommentPos

      let
        sameGroupOf = maybe xs \nextGroupStart ->
          takeWhile (\(L p _)-> p < nextGroupStart) xs

        restOf = maybe [] \nextGroupStart ->
          dropWhile (\(L p _) -> p <= nextGroupStart) xs

      restGroups <- go (restOf nextGroupStartM)
      pure $ (comments, L rspan x :| sameGroupOf nextGroupStartM) : restGroups

    go _ = pure []

-- | A view on 'groupAttachedComments': return 'Just' when there is just a
--   one big group without any comments.
groupWithoutComments
    :: [([AnnotationComment], NonEmpty (Located a))]
    -> Maybe [Located a]
groupWithoutComments grouped
    | all (null . fst) grouped
    = Just $ concatMap (toList . snd) grouped
    | otherwise
    = Nothing

modifyCurrentLine :: (String -> String) -> P ()
modifyCurrentLine f = do
    s0 <- get
    put s0 {currentLine = f $ currentLine s0}

wrapping
    :: P a  -- ^ First printer to run
    -> P a  -- ^ Printer to run if first printer violates max columns
    -> P a  -- ^ Result of either the first or the second printer
wrapping p1 p2 = do
    maxCols <- asks columns
    case maxCols of
        -- No wrapping
        Nothing -> p1
        Just c  -> do
            s0 <- get
            x <- p1
            s1 <- get
            if length (currentLine s1) <= c
                -- No need to wrap
                then pure x
                else do
                    put s0
                    y <- p2
                    s2 <- get
                    if length (currentLine s1) == length (currentLine s2)
                        -- Wrapping didn't help!
                        then put s1 >> pure x
                        -- Wrapped
                        else pure y

withColumns :: Maybe Int -> P a -> P a
withColumns c = local $ \pc -> pc {columns = c}
