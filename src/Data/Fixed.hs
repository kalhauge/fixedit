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
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable

{- | A Catamorphism is essentially a fold, but this 
fold is over any F-algebra f.
-}
class Functor f => Catamorphic f a | a -> f where
  -- | A fixed
  unfix :: a -> f a

  -- | A catamorphism is a fold
  cata :: (f x -> x) -> a -> x
  cata fx = fx . fmap (cata fx) . unfix
  {-# INLINE cata #-}

-- | An Anamorphism is a unfold, but this allows any F-algebra f.
class Functor f => Anamorphic f a | a -> f where
  -- | A u
  fix :: f a -> a

  -- | A anamorphism is a unfold.
  ana  :: (x -> f x) -> x -> a
  ana fx = fix . fmap (ana fx) . fx 
  {-# INLINE ana #-}

-- {- | We define the Fixed of an element as something that can 
-- has both a catamorphism and anamorphism into this a functor.
-- 
-- The goal here is to allow other monomorphic data-structures to be 
-- de constructed into more interesting shapes.
-- -}
class (Anamorphic f a, Catamorphic f a) => Fixed f a | a -> f where

-- | If two itemes can be fixed to the same functor, then we can convert
-- freely between them.
hylo :: (Catamorphic f a, Anamorphic f b) => a -> b
hylo = cata fix
{-# INLINE hylo #-}

-- | The standard definition of a fix data type.
data Fix f = Fix { unFix :: f (Fix f) }
  deriving (Generic)

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

instance Functor f => Catamorphic f (Fix f) where unfix = unFix
instance Functor f => Anamorphic f (Fix f) where fix = Fix

-- * Operations 
-- We can derive a lot of operations from the catamorphism and anamorphism.

-- | Map can be defined using a catamorphism and an anamorphism
mapF :: 
  (Catamorphic f a, Anamorphic g b) 
  => (f b -> g b) 
  -> a -> b
mapF f = cata (fix . f)
{-# INLINE mapF #-}

-- | Map can be defined using a catamorphism and an anamorphism
mapFx :: 
  (Bifunctor p, Catamorphic (p x) a, Anamorphic (p y) b) 
  => (x -> y) 
  -> a -> b
mapFx f = mapF (first f)
{-# INLINE mapFx #-}

foldF :: 
  (Bifoldable p, Monoid x, Catamorphic (p x) a) 
  => a -> x
foldF = cata bifold
{-# INLINE foldF #-}

foldMapF :: 
  (Monoid x, Catamorphic f a) 
  => (f x -> x)
  -> a -> x
foldMapF = cata
{-# INLINE foldMapF #-}

foldMapFx :: 
  (Bifoldable p, Monoid y, Catamorphic (p x) a) 
  => (x -> y)
  -> a -> y
foldMapFx f = cata (bifoldMap f id)
{-# INLINE foldMapFx #-}

traverseF :: 
  (Applicative t, Catamorphic f a, Anamorphic g b) 
  => (f (t b) -> t (g b))
  -> a -> t b
traverseF f = cata (fmap fix . f) 
{-# INLINE traverseF #-}

traverseFx :: 
  (Bitraversable p, Applicative f, Catamorphic (p x) a, Anamorphic (p y) b) 
  => (x -> f y)
  -> a -> f b
traverseFx f = cata (fmap fix . bitraverse f id) 
{-# INLINE traverseFx #-}

-- * Common F-Algebras

-- | An `Uncons` functor is a simple fold fixpoint.
data MonoidF b a 
  = MAppend b a
  | MEmpty
  deriving (Functor, Generic)

instance Bifunctor MonoidF where
  bimap f g = \case
    MAppend a b -> MAppend (f a) (g b)
    MEmpty -> MEmpty

instance Bifoldable MonoidF where
  bifoldMap f g = \case
    MAppend a b -> (f a) <> (g b)
    MEmpty -> mempty

instance Bitraversable MonoidF where

instance Catamorphic (MonoidF b) [b] where
  unfix = \case
    [] -> MEmpty
    a:rest -> MAppend a rest

instance Anamorphic (MonoidF b) [b] where
  fix = \case
    MEmpty -> []
    MAppend a rest -> a:rest
