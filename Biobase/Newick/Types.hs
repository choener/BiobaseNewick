
module Biobase.Newick.Types where

import Data.Text (Text)



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

