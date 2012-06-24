{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Data.PolyLens (
    mkPolyLens
  , mkPolyLensBy
  
  , (^.)
  , (%=)
  , (^=)
  ) where

import Control.Applicative
import Control.Applicative (Const(..), (<$>))
import Control.Monad.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import Language.Haskell.TH

mkPolyLens :: Name -> Q [Dec]
mkPolyLens = mkPolyLensBy (drop 1)

mkPolyLensBy :: (String -> String) -> Name -> Q [Dec]
mkPolyLensBy nameTransform datatype = do
  i <- reify datatype
  let constructorFields = case i of
        TyConI (DataD    _ _ _ [RecC _ fs] _) -> fs
        TyConI (NewtypeD _ _ _ (RecC _ fs) _) -> fs
        _ -> error "Deriving van Laarhoven lens failed"
  mapM (derive nameTransform) $ map fst' constructorFields
  where fst' (x, _, _) = x

-- given a record field name,
-- produces a single function declaration:
-- lensName p f = (\x -> p { field = x }) <$> f (field p)
derive :: (String -> String) -> Name -> Q Dec
derive nameTransform field = funD lensName [defLine]
  where
    lensName = mkName (nameTransform (nameBase field))
    p = mkName "p"
    f = mkName "f"
    defLine = clause pats (normalB body) []
    pats = [varP f, varP p]
    body = [| (\x -> $(record p field [|x|]) )
              <$> $(appE (varE f) (appE (varE field) (varE p))) |]
    record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]

-- getter
infixl 9 ^.
(^.) :: a -> ((b -> Const b b') -> a -> Const b a') -> b
x ^. l = getConst $ l Const x

-- modifier
infixr 4 %=
(%=) :: ((b -> Identity b') -> a -> Identity a') -> (b -> b') -> a -> a'
l %= f = runIdentity . l (Identity . f)

-- setter
infixr 4 ^=
(^=) :: ((b -> Identity b') -> a -> Identity a') -> b' -> a -> a'
l ^= v = l %= (const v)
