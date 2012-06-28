{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Data.PolyLensTH (
    mkPolyLenses
  , mkPolyLensesBy
  , derivePolyLens
  ) where

import Language.Haskell.TH

mkPolyLenses :: Name -> Q [Dec]
mkPolyLenses = mkPolyLensesBy (drop 1)

mkPolyLensesBy :: (String -> String) -> Name -> Q [Dec]
mkPolyLensesBy nameTransform datatype = do
  i <- reify datatype
  let constructorFields = case i of
        TyConI (DataD    _ _ _ [RecC _ fs] _) -> fs
        TyConI (NewtypeD _ _ _ (RecC _ fs) _) -> fs
        TyConI TySynD{} ->
          error $ "Can't derive PolyLens for type synonym: " ++ datatypeStr
        TyConI DataD{}  ->
          error $ "Can't derive PolyLens for tagged union: " ++ datatypeStr
        _ ->
          error $ "Not sure how to derive a PolyLens for: "  ++ datatypeStr
  concat `fmap` mapM (derivePolyLens nameTransform . fst') constructorFields
  where
    fst' (x, _, _) = x
    datatypeStr = nameBase datatype

derivePolyLens :: (String -> String) -> Name -> Q [Dec]
derivePolyLens nameTransform field = do
  body <- derivePolyLensBody nameTransform field
  -- todo
  -- sig <- derivePolyLensSig field
  -- return [sig, body]
  return [body]


-- given a record field name,
-- produces a single function declaration:
-- lensName f a = (\x -> a { field = x }) <$> f (field a)
derivePolyLensBody :: (String -> String) -> Name -> Q Dec
derivePolyLensBody nameTransform field = funD lensName [defLine]
  where
    lensName = mkName (nameTransform (nameBase field))
    a = mkName "a"
    f = mkName "f"
    defLine = clause pats (normalB body) []
    pats = [varP f, varP a]
    body = [| (\x -> $(record a field [|x|]))
              `fmap` $(appE (varE f) (appE (varE field) (varE a)))
            |]
    record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]
