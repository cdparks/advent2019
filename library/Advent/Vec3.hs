{-# LANGUAGE StrictData #-}

module Advent.Vec3
  ( Vec3(..)
  , x
  , y
  , z
  , scalars
  )
where

import Advent.Prelude

import GHC.Generics (Generic)
import Lens.Micro
import Text.Show

data Vec3 a = Vec3
  { _x :: a
  , _y :: a
  , _z :: a
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

instance Applicative Vec3 where
  pure a = Vec3 a a a
  Vec3 f g h <*> Vec3 a b c = Vec3 (f a) (g b) (h c)

instance Show a => Show (Vec3 a) where
  showsPrec _ (Vec3 {..}) =
    showChar '('
      . shows _x
      . showString ", "
      . shows _y
      . showString ", "
      . shows _z
      . showChar ')'

x :: Lens' (Vec3 a) a
x = lens _x $ \v s -> v { _x = s }

y :: Lens' (Vec3 a) a
y = lens _y $ \v s -> v { _y = s }

z :: Lens' (Vec3 a) a
z = lens _z $ \v s -> v { _z = s }

scalars :: Vec3 a -> [a]
scalars v = (v ^.) <$> [x, y, z]

-- brittany-disable-next-binding

instance Num a => Num (Vec3 a) where
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
