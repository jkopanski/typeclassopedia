{-# OPTIONS_GHC -Wall #-}

module Monad where

import Prelude
-- import Data.List (map, concat)
-- import Data.Functor
-- import Data.Function (const, id)
-- import Control.Applicative
import Control.Monad (join)
-- import Prelude (Maybe (..), repeat, zipWith, ($))

-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b
--   (>>)   :: m a -> m b -> m b
--   m >> n = m >>= \_ -> n

-- instance Monad [] where
--   return x = ([x])
--   xs >>= g = concat [ g x | x <- xs ]

-- instance Monad ((->) e) where
--   return  = const
--   -- x >>= g = g . x
--   (>>=) x g y = g (x y) y

data Free f a = Var a
              | Node (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap g (Var a)  = Var (g a)
  fmap g (Node a) = Node (fmap (fmap g) a)

instance (Functor f) => Applicative (Free f) where
  pure                  = Var
  (Var g)  <*> (Var a)  = Var $ g a
  (Var g)  <*> (Node a) = Node (fmap (fmap g) a)
  (Node g) <*> a        = Node $ fmap (<*> a) g

instance (Functor f) => Monad (Free f) where
  return = Var
  (Var a)  >>= g = g a
  (Node a) >>= g = Node $ fmap (>>= g) a

bind' :: (Monad m, Functor m) => m a -> (a -> m b) -> m b
bind' x k = join $ fmap k x

join' :: (Monad m, Functor m) => m (m a) -> m a
join' a = a >>= id

fmap' :: (Monad m, Functor m) => (a -> b) -> m a -> m b
fmap' g a = a >>= (\x -> return $ g x)

-- let g >=> h = \x -> g x >>= h then

-- return >=> g = \x -> return x >>= g
--              = return x >>= g

-- g >=> return = \x -> g x >>= return
--              = g >>= return

-- (g >=> h) >=> k = p >=> k = \x -> p x >>= k
-- p = g >=> h = \y -> g y >>= h

-- \x -> g x >>= h >>= k
