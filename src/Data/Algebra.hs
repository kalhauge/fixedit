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

type Algebra   f a = f a -> a
type Coalgebra f a = a -> f a

class Functor f => Embedable f a | a -> f where
  embed :: Algebra f a

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

-- * Common F-Algebras

-- | An `SemigroupF` functor is a simple fold fixpoint.
data SemigroupF b a 
  = One b
  | More b a
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

-- | An `MonoidF` functor is a simple fold fixpoint.
data MonoidF b a 
  = None
  | Many b a
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

instance Catamorphic (MonoidF b) [b] 
instance Anamorphic (MonoidF b) [b] 
instance Fixed (MonoidF b) [b] 

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


