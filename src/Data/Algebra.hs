{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
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
module Data.Algebra
  where

-- base
import GHC.Generics (Generic)
import qualified Data.List as L
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable

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

-- | Some data-structures have default coalgebras.
class Functor f => Projectable f a | a -> f where
  project :: Coalgebra f a 

{- | A Catamorphism is essentially a fold, but this 
fold is over any F-algebra f.
-}
class Catamorphic f a | a -> f where
  -- | A catamorphism is a fold
  cata :: (f x -> x) -> a -> x
  default cata :: Projectable f a => (f x -> x) -> a -> x
  cata fx = fx . fmap (cata fx) . project
  {-# INLINE cata #-}

-- | An Anamorphism is a unfold, but this allows any F-algebra f.
class Embedable f a => Anamorphic f a | a -> f where
  -- | A anamorphism is a unfold.
  ana  :: (x -> f x) -> x -> a
  default ana :: Embedable f a => (x -> f x) -> x -> a
  ana fx = embed . fmap (ana fx) . fx 
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
hylo :: (Catamorphic f a, Embedable f b) => a -> b
hylo = cata embed
{-# INLINE hylo #-}

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














