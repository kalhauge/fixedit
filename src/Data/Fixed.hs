{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Fixed
  where

-- base
import GHC.Generics (Generic)
import qualified Data.List as L

{- | We define the Fixed of an element as something that can 
has both a catamorphism and anamorphism into this a functor.

The goal here is to allow other monomorphic data-structures to be 
de constructed into more interesting shapes.
-}
class Functor f => Fixed f a | a -> f where
  cata :: (f x -> x) -> a -> x
  ana  :: (x -> f x) -> x -> a

-- | If two itemes can be fixed to the same functor, then we can convert
-- freely between them.
hylo :: (Fixed f a, Fixed f b) => a -> b
hylo = ana unFix . cata Fix
{-# INLINE hylo #-}

-- | The standard definition of a fix data type.
data Fix f = Fix { unFix :: f (Fix f) }
  deriving (Generic)

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

instance Functor f => Fixed f (Fix f) where
  cata f = f . fmap (cata f) . unFix 
  {-# INLINE cata #-}
  ana f = Fix . fmap (ana f) . f
  {-# INLINE ana #-}


-- * MonoidF
-- Basic folds and unfolds defined as a language.

-- | An `Uncons` functor is a simple fold fixpoint.
data MonoidF b a 
  = MAppend b a
  | MEmpty
  deriving Functor 

-- | Convert a 'MonoidF' into a uncons operation.
asUncons :: MonoidF b a -> Maybe (b, a)
asUncons = \case
  MAppend b a -> Just (b, a)
  MEmpty  -> Nothing

-- | A list can be fixed as a Uncons functor.
instance Fixed (MonoidF b) [b] where
  cata f = L.foldr (curry (f . uncurry MAppend)) (f MEmpty)
  {-# INLINE cata #-}
  ana  f = L.unfoldr (asUncons . f)
  {-# INLINE ana #-}

