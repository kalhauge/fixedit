{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Algebra.Common where

-- base
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import GHC.Generics (Generic)

import qualified Data.List.NonEmpty as NE

-- fixedit
import Data.Algebra

-- | A `SemigroupF` functor is a simple fold fixpoint for an non-empty
-- container.
data SemigroupF b f 
  = One b
  | More b f
  deriving (Functor, Foldable, Traversable, Generic)

instance Bifunctor SemigroupF where
  bimap f g = \case
    More a b -> More (f a) (g b)
    One b -> One (f b)

instance Bifoldable SemigroupF where
  bifoldMap f g = \case
    More a b -> (f a) <> (g b)
    One a -> (f a)

instance Bitraversable SemigroupF where

instance Projectable (SemigroupF b) (NE.NonEmpty b) where
  project (NE.uncons -> (a, x)) = 
    maybe (One a) (More a) x
  {-# INLINE project #-}

instance Embedable (SemigroupF b) (NE.NonEmpty b) where
  embed = \case
    One a -> a NE.:| []
    More a rest -> a NE.<| rest
  {-# INLINE embed #-}


-- | A `MonoidF` functor is a simple fold fixpoint.
data MonoidF b f 
  = None
  | Many b f
  deriving (Functor, Foldable, Traversable, Generic)

instance Bifunctor MonoidF where
  bimap f g = \case
    Many a b -> Many (f a) (g b)
    None -> None

instance Bifoldable MonoidF where
  bifoldMap f g = \case
    Many a b -> (f a) <> (g b)
    None -> mempty

instance Bitraversable MonoidF where

instance Projectable (MonoidF b) [b] where
  project = \case
    [] -> None
    a:rest -> Many a rest

instance Embedable (MonoidF b) [b] where
  embed = \case
    None -> []
    Many a rest -> a:rest
  {-# INLINE embed #-}

instance Catamorphic (MonoidF b) [b] 
instance Anamorphic (MonoidF b) [b] 
instance Fixed (MonoidF b) [b] 

-- This allows us to Embed items that are Embedable SemigroupF
-- into MonoidF. 
newtype IsNonEmpty a = 
  IsNonEmpty { fromNonEmpty :: a }

instance Embedable (MonoidF b) a => Embedable (SemigroupF b) (IsNonEmpty a) where
  embed = \case
    One  a   -> 
      IsNonEmpty $ embed (Many a (embed None))
    More a (IsNonEmpty r) -> 
      IsNonEmpty $ embed (Many a r)

-- | Convert any ListLike into MonoidF
intoNonEmpty :: (Catamorphic (SemigroupF x) a, Embedable (MonoidF x) b) => a -> b
intoNonEmpty = fromNonEmpty . hylo

-- | A Partition of a structure, In the split case every element on the 
-- right side is above the element in the middle.
data PartitionF a f 
  = Split f a f 
  | Leaf 
  deriving (Functor, Foldable, Traversable, Generic)



