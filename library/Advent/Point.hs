{-# LANGUAGE StrictData #-}

module Advent.Point
  ( Point(..)
  , x
  , y
  )
where

import Advent.Prelude

import GHC.Generics (Generic)
import Lens.Micro
import Text.Show

data Point a = Point
  { _x :: a
  , _y :: a
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

instance Applicative Point where
  pure a = Point a a
  Point f g <*> Point a b = Point (f a) (g b)

instance Show a => Show (Point a) where
  showsPrec _ Point {..} =
    showChar '(' . shows _x . showString ", " . shows _y . showChar ')'

x :: Lens' (Point a) a
x = lens _x $ \p v -> p { _x = v }

y :: Lens' (Point a) a
y = lens _y $ \p v -> p { _y = v }

-- brittany-disable-next-binding

instance Num a => Num (Point a) where
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
