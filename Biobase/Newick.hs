
module Biobase.Newick
  ( module Biobase.Newick.Types
  , module Biobase.Newick.Import
  , newicksToText
  )
  where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List (intersperse)

import Biobase.Newick.Import
import Biobase.Newick.Types



-- | Render a Newick tree to a newick string.

newicksToText :: [NewickTree] -> Text
newicksToText = T.unlines . map ((`T.snoc` ';') . go)
  where go (NLeaf    lbl len) = lbl `T.append` goLen len
        go (NNode cs lbl len) = T.concat $ ["("] ++ intersperse "," (map go cs) ++ [")",lbl,goLen len]
        goLen NoLen   = ""
        goLen (Len d) = ':' `T.cons` (T.pack $ show d)

prop_Newick (t :: NewickTree) = Right [t] == ss where
  ss = newicksFromText (newicksToText [t])

