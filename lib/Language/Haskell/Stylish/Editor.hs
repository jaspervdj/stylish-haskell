{-# language LambdaCase #-}

--------------------------------------------------------------------------------
-- | This module provides you with a line-based editor. It's main feature is
-- that you can specify multiple changes at the same time, e.g.:
--
-- > [deleteLine 3, changeLine 4 ["Foo"]]
--
-- when this is evaluated, we take into account that 4th line will become the
-- 3rd line before it needs changing.
module Language.Haskell.Stylish.Editor
    ( Change
    , applyChanges

    , change
    , changeLine
    , delete
    , deleteLine
    , insert
    , noop
    ) where


--------------------------------------------------------------------------------
import           Data.List                      (intercalate, sortOn)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block


--------------------------------------------------------------------------------
-- | Changes the lines indicated by the 'Block' into the given 'Lines'
data Change a = Change
    { changeBlock :: Block a
    , changeLines :: [a] -> [a]
    }


--------------------------------------------------------------------------------
moveChange :: Int -> Change a -> Change a
moveChange offset (Change block ls) = Change (moveBlock offset block) ls


--------------------------------------------------------------------------------
applyChanges :: [Change a] -> [a] -> [a]
applyChanges changes0
    | overlapping blocks = error $
        "Language.Haskell.Stylish.Editor.applyChanges: " ++
        "refusing to make overlapping changes on lines " ++
        intercalate ", " (map printBlock blocks)
    | otherwise          = go 1 changes1
  where
    changes1 = sortOn (blockStart . changeBlock) changes0
    blocks   = map changeBlock changes1

    printBlock b = show (blockStart b) ++ "-" ++ show (blockEnd b)

    go _ []                ls = ls
    go n (ch : chs) ls =
        -- Divide the remaining lines into:
        --
        -- > pre
        -- > old  (lines that are affected by the change)
        -- > post
        --
        -- And generate:
        --
        -- > pre
        -- > new
        -- > (recurse)
        --
        let block       = changeBlock ch
            (pre, ls')  = splitAt (blockStart block - n) ls
            (old, post) = splitAt (blockLength block) ls'
            new         = changeLines ch old
            extraLines  = length new - blockLength block
            chs'        = map (moveChange extraLines) chs
            n'          = blockStart block + blockLength block + extraLines
        in pre ++ new ++ go n' chs' post


--------------------------------------------------------------------------------
-- | Change a block of lines for some other lines
change :: Block a -> ([a] -> [a]) -> Change a
change = Change

--------------------------------------------------------------------------------
noop :: Block a -> Change a
noop = flip change $ id

--------------------------------------------------------------------------------
-- | Change a single line for some other lines
changeLine :: Int -> (a -> [a]) -> Change a
changeLine start f = change (Block start start) $ \case
    []      -> []
    (x : _) -> f x


--------------------------------------------------------------------------------
-- | Delete a block of lines
delete :: Block a -> Change a
delete block = Change block $ const []


--------------------------------------------------------------------------------
-- | Delete a single line
deleteLine :: Int -> Change a
deleteLine start = delete (Block start start)


--------------------------------------------------------------------------------
-- | Insert something /before/ the given lines
insert :: Int -> [a] -> Change a
insert start = Change (Block start (start - 1)) . const
