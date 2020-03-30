{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
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
{-| 
Module      : Data.Algebra
Copyright   : (c) Christian Gram Kalhauge, 2020
License     : BSD-3
Maintainer  : Christian Gram Kalhauge <christian@kalhauge.dk>

-}
module Data.Algebra
  ( -- * Algebras
    Algebra
  , Coalgebra

  -- ** Default Algebras

  , Embedable (..)
  , Projectable (..)
  , Catamorphic (..)
  , Anamorphic (..)
  , Fixed 
  
  -- ** Lifted Default Algebras
  -- Many data structures is functors over the same items as their default algebras. 
  -- These structures illustrate this. They are the same as the structures 
  -- in the default algebras.
  -- For every X1, it should be the case that X is also true.
  , Embedable1 (..)
  , Projectable1 (..)
  , Catamorphic1 (..)
  , Anamorphic1 (..)

  -- ** Algebraic Utilities
  
  , hylo

  -- * Common Algebras

  ) where

-- base
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import GHC.Generics (Generic)
import qualified Data.List as L

-- | An algebra is a functor which we can embed into a value.
-- For example, we can embed N + N into N, by adding the values.
type Algebra   f a = f a -> a
  
-- | A Coalgebra is an reverse Algebra. It means that we can 
-- project a value into a functor.
-- For example we can project N into primefactors of N.
type Coalgebra f a = a -> f a

-- | Some data-structures have default algebras.
class Functor f => Embedable f a | a -> f where
  embed :: Algebra f a

class Bifunctor f => Embedable1 f a | a -> f where
  embed1 :: forall x. Algebra (f x) (a x)

-- | Some data-structures have default coalgebras.
class Functor f => Projectable f a | a -> f where
  project :: Coalgebra f a 

class Bifunctor f => Projectable1 f a | a -> f where
  project1 :: forall x. Coalgebra (f x) (a x)

{- | A Catamorphism is essentially a fold, but this 
fold is over any F-algebra f.
-}
class Catamorphic f a | a -> f where
  -- | A catamorphism is a fold
  cata :: Algebra f x -> a -> x
  default cata :: Projectable f a => (f x -> x) -> a -> x
  cata fx = fx . fmap (cata fx) . project
  {-# INLINE cata #-}

class Catamorphic1 f a | a -> f where
  -- | A catamorphism is a fold
  cata1 :: Algebra (f y) x -> a y -> x
  default cata1 :: Projectable1 f a => (f y x -> x) -> a y -> x
  cata1 fx = fx . second (cata1 fx) . project1
  {-# INLINE cata1 #-}

-- | An Anamorphism is an unfold, but this allows any F-algebra f.
class Embedable f a => Anamorphic f a | a -> f where
  -- | A anamorphism is an unfold.
  ana  :: Coalgebra f x -> x -> a
  default ana :: Embedable f a => (x -> f x) -> x -> a
  ana fx = embed . fmap (ana fx) . fx 
  {-# INLINE ana #-}

-- | An Anamorphism is a unfold, but this allows any F-algebra f.
class Embedable1 f a => Anamorphic1 f a | a -> f where
  -- | A anamorphism is a unfold.
  ana1  :: Coalgebra (f y) x -> x -> a y
  default ana1 :: Embedable1 f a => (x -> f y x) -> x -> a y
  ana1 fx = embed1 . second (ana1 fx) . fx 
  {-# INLINE ana1 #-}

-- {- | We define the Fixed of an element as something that can 
-- has both a catamorphism and anamorphism into this a functor.
-- 
-- The goal here is to allow other monomorphic data-structures to be 
-- de constructed into more interesting shapes.
-- -}
class (Anamorphic f a, Catamorphic f a) => Fixed f a | a -> f where

class (Anamorphic1 f a, Catamorphic1 f a) => Fixed1 f a | a -> f where

-- | If two itemes can be fixed to the same functor, then we can convert
-- freely between them.
hylo :: (Catamorphic f a, Embedable f b) => a -> b
hylo = cata embed
{-# INLINE hylo #-}

-- | If two itemes can be fixed to the same functor, then we can convert
-- freely between them.
hylo1 :: (Catamorphic1 f a, Embedable1 f b) => a x -> b x
hylo1 = cata1 embed1
{-# INLINE hylo1 #-}

-- * Operations 
-- We can derive a lot of operations from the catamorphism and anamorphism.

-- | Map can be defined using a catamorphism and an anamorphism
mapF :: 
  (Catamorphic f a, Embedable g b) 
  => (f b -> g b) 
  -> a -> b
mapF f = cata (embed . f)
{-# INLINE mapF #-}

-- | Map can be defined using a catamorphism and an anamorphism
mapFx :: 
  (Bifunctor p, Catamorphic (p x) a, Embedable (p y) b) 
  => (x -> y) 
  -> a -> b
mapFx f = mapF (first f)
{-# INLINE mapFx #-}

-- | Given that the  
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
  (Applicative t, Catamorphic f a, Embedable g b) 
  => (f (t b) -> t (g b))
  -> a -> t b
traverseF f = cata (fmap embed . f) 
{-# INLINE traverseF #-}

traverseFx :: 
  (Bitraversable p, Applicative f, Catamorphic (p x) a, Embedable (p y) b) 
  => (x -> f y)
  -> a -> f b
traverseFx f = cata (fmap embed . bitraverse f id) 
{-# INLINE traverseFx #-}

-- | The standard definition of a fix data type.
data Fix f = Fix { unFix :: f (Fix f) }
  deriving (Generic)

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

instance Functor f => Projectable f (Fix f) where project = unFix
instance Functor f => Embedable f (Fix f) where embed = Fix
instance Functor f => Catamorphic f (Fix f) 
instance Functor f => Anamorphic f (Fix f) 
instance Functor f => Fixed f (Fix f) 

data Fix1 f x = Fix1 { unFix1 :: f x (Fix1 f x) }
  deriving (Generic)

instance Bifunctor f => Projectable1 f (Fix1 f) where project1 = unFix1
instance Bifunctor f => Embedable1 f (Fix1 f) where embed1 = Fix1
instance Bifunctor f => Catamorphic1 f (Fix1 f) 
instance Bifunctor f => Anamorphic1 f (Fix1 f) 
instance Bifunctor f => Fixed1 f (Fix1 f) 



-- | Turn a regular data structure into a fix structure.
-- This is mostly used to do derive via.
newtype AsFix a x = AsFix { fixedItem :: a x }

instance (Embedable1 p a, Catamorphic1 p a) => Functor (AsFix a) where
  fmap f = AsFix . cata1 (embed1 . first f) . fixedItem
  {-# INLINE fmap #-}

instance (Bifoldable p, Catamorphic1 p a) => Foldable (AsFix a) where
  foldMap f = cata1 (bifoldMap f id) . fixedItem
  {-# INLINE foldMap #-}

instance (Bitraversable p, Embedable1 p a, Catamorphic1 p a) => Traversable (AsFix a) where
  traverse f = fmap AsFix . cata1 (fmap embed1 . bitraverse f id) . fixedItem
  {-# INLINE traverse #-}










