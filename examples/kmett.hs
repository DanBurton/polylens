-- code from Edward Kmett's blog post
-- Mirrored Lenses
-- http://comonad.com/reader/2012/mirrored-lenses/

-- Slightly adapted since Data.PolyLens doesn't use Rank 2 types.
-- Mostly this means adding "Functor f =>" everywhere

import Data.Complex
import Data.PolyLens

realLens :: (RealFloat a, Functor f) => Lens (Complex a) a f
realLens f (r :+ i) = fmap (:+ i) (f r)

imagLens :: (RealFloat a, Functor f) => Lens (Complex a) a f
imagLens f (r :+ i) = fmap (r :+) (f i)


fstLens :: Functor f => PolyLens (a,c) (b,c) a b f
fstLens f (a, b) = fmap (\x -> (x, b)) (f a)

sndLens :: Functor f => PolyLens (a,b) (a,c) b c f
sndLens f (a, b) = fmap ((,) a) (f b)


swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

swapped :: Functor f => PolyLens (a, b) (c, d) (b, a) (d, c) f
swapped = iso swap swap


negated :: (Num a, Functor f) => Lens a a f
negated = iso negate negate


traverseLens :: ((c -> c) -> a -> b) -> a -> b
traverseLens f = f id


getFst :: Getter (a, b) a r
getFst = getting fst

getSnd :: Getter (a, b) b r
getSnd = getting snd


getPhase :: RealFloat a => Getter (Complex a) a r
getPhase = getting phase

getAbs, getSignum :: Num a => Getter a a r
getAbs = getting abs
getSignum = getting signum


plus, times :: Num a => Setter a a a
plus = setting (+)
times = setting (*)


reals :: (RealFloat a, RealFloat b) => Modifier (Complex a) (Complex b) a b
reals = modifying (\f (r :+ i) -> f r :+ f i)

