{-# LANGUAGE TemplateHaskell #-}

-- Code from Russel O'Connor's blog post
-- Polymorphic Update with van Laarhoven Lenses
-- http://r6.ca/blog/20120623T104901Z.html

import Control.Applicative
import Data.PolyLens

data Pair a b = Pair { _pi1 :: a, _pi2 :: b } deriving Show
$(mkPolyLens ''Pair)

examplePair :: Pair Int Char
examplePair = Pair 1 'b'

nestedPair :: Pair (Pair Int Char) String
nestedPair = Pair examplePair "FTW!"


data Complicated a b = Complicated {field1 :: a, field2 :: a, field3 :: b}
                     deriving Show
$(mkPolyLens ''Complicated)

complexExample :: Complicated Int Char
complexExample = Complicated {field1 = 1, field2 = 2, field3 = 'c'}

handmadeLens :: Functor f
             => ((a, a) -> f (a', a'))
             -> Complicated a b
             -> f (Complicated a' b)
handmadeLens g (Complicated f1 f2 f3) =
  (\(n1, n2) -> Complicated n1 n2 f3) <$> g (f1, f2)

