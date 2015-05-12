
module Biobase.Newick.Types where

import Data.Monoid
import Data.Text (Text)
import Test.QuickCheck
import Control.Monad



data NewickLength
  = Len { len :: {-# Unpack #-} !Double }
  | NoLen
  deriving (Eq,Ord,Show,Read)

instance Monoid NewickLength where
  mempty = NoLen
  mappend (Len a) (Len b) = Len $ a+b
  mappend (Len a) _       = Len a
  mappend _       (Len b) = Len b
  mappend _       _       = NoLen
  {-# Inline mempty  #-}
  {-# Inline mappend #-}

data NewickTree
  = NLeaf { label     :: {-# Unpack #-} !Text
          , nlength   :: !NewickLength
          }
  | NNode { children  :: [NewickTree]
          , label     :: {-# Unpack #-} !Text
          , nlength   :: !NewickLength
          }
  deriving (Eq,Ord,Show,Read)



instance Arbitrary NewickLength where
  arbitrary = maybe NoLen Len <$> arbitrary
  shrink NoLen   = []
  shrink (Len _) = [NoLen]

instance Arbitrary NewickTree where
  arbitrary = sized arbNewickTree
  shrink (NLeaf    lbl len) = map (NLeaf lbl) $ shrink len
  shrink (NNode cs lbl len) = [NNode ds lbl ln | ds <- shrink cs, ln <- shrink len]

-- empty (root only) tree
arbNewickTree 0 = NNode [] "" <$> arbitrary
-- single leaf
arbNewickTree 1 = NLeaf "" <$> arbitrary
-- recursive tree
arbNewickTree k = do
  n  <- choose (0,5)
  ds <- replicateM n $ choose (1,k-1)
  cs <- mapM arbNewickTree ds
  NNode cs "" <$> arbitrary

