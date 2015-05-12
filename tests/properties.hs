
module Main where

import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

import           Biobase.Newick
import qualified Biobase.Newick as N



-- * Serialization to and from the canonical Newick format.

prop_Newick (t :: NewickTree) = Right [t] == ss where
  ss = newicksFromText (newicksToText [t])

-- * Serialization with default Haskell machinery

-- | Correct @Binary@ serialization
--
-- TODO waits for instance

prop_Binary (t :: NewickTree) = t == s where
  s = t

-- | Correct @Cerial@ serialization
--
-- TODO waits for instance

prop_Serialize (t :: NewickTree) = t == s where
  s = t

main :: IO ()
main = $(defaultMainGenerator)

