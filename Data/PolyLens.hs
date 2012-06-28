module Data.PolyLens (
    module TH

  , PolyLens
  , Lens

  , Getting
  , Setting

  , (^.)
  , (^=)
  , (%=)

  , Getter
  , Setter
  , Modifier

  , getting
  , setting
  , modifying

  , lens
  , iso
  ) where

import Data.PolyLensTH as TH


type PolyLens a a' b b' f =
  (b -> f b') -> a -> f a'

type Lens a b f = PolyLens a a b b f


newtype Getting b a = Getting { got :: b }
instance Functor (Getting b) where
  fmap _ (Getting b) = Getting b


newtype Setting a = Setting { unsetting :: a }
instance Functor Setting where
  fmap f (Setting a) = Setting (f a)


-- getter
infixl 8 ^.
(^.) :: a -> PolyLens a a' b b' (Getting b) -> b
x ^. l = got $ l Getting x

-- modifier
infixr 4 %=
(%=) :: PolyLens a a' b b' Setting -> (b -> b') -> a -> a'
l %= f = unsetting . l (Setting . f)

-- setter
infixr 4 ^=
(^=) :: PolyLens a a' b b' Setting -> b' -> a -> a'
l ^= v = l %= const v


type Getter a b r a' b' = PolyLens a a' b  b' (Getting r)
type Setter a b' a'     = PolyLens a a' () b' Setting
type Modifier a a' b b' = PolyLens a a' b  b' Setting


getting :: (a -> b) -> Getter a b r a' b'
getting g f = Getting . got . f . g

setting :: (a -> b' -> a') -> Setter a b' a'
setting f g a = Setting (f a (unsetting (g ())))

modifying :: ((b -> b') -> a -> a') -> Modifier a a' b b'
modifying f g a = Setting (f (unsetting . g) a)


lens :: Functor f => (a -> b) -> (a -> b' -> a') -> PolyLens a a' b b' f
lens f g h a = fmap (g a) (h (f a))

iso :: Functor f => (a -> b) -> (b' -> a') -> PolyLens a a' b b' f
iso f g h a = fmap g (h (f a))
