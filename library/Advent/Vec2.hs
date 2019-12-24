{-# LANGUAGE StrictData #-}

module Advent.Vec2
  ( Vec2(..)
  , x
  , y
  , scalars
  , neighbors
  )
where

import Advent.Prelude

import GHC.Generics (Generic)
import Lens.Micro
import Text.Show

data Vec2 a = Vec2
  { _x :: a
  , _y :: a
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

instance Applicative Vec2 where
  pure a = Vec2 a a
  Vec2 f g <*> Vec2 a b = Vec2 (f a) (g b)

instance Show a => Show (Vec2 a) where
  showsPrec _ Vec2 {..} =
    showChar '(' . shows _x . showString ", " . shows _y . showChar ')'

x :: Lens' (Vec2 a) a
x = lens _x $ \p v -> p { _x = v }

y :: Lens' (Vec2 a) a
y = lens _y $ \p v -> p { _y = v }

scalars :: Vec2 a -> [a]
scalars v = (v ^.) <$> [x, y]

neighbors :: Num a => Vec2 a -> [Vec2 a]
neighbors (Vec2 x0 y0) =
  [Vec2 (x0 + 1) y0, Vec2 (x0 - 1) y0, Vec2 x0 (y0 + 1), Vec2 x0 (y0 - 1)]

-- brittany-disable-next-binding

instance Num a => Num (Vec2 a) where
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
