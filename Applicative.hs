{-# OPTIONS_GHC -Wall #-}

module Applicative where

import Data.Functor
import Prelude (Maybe (..), repeat, zipWith, ($))

class Functor f => Applicative f where
  pure :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
{-
 - prove that
 -
 - pure f <*> x = pure (flip ($)) <*> x <*> pure f
 - let u = pure f
 - let x = pure y
 -
 - then from interchange law:
 - u <*> pure y = pure ($ y) <*> u
 -
 - pure f <*> x =
 - pure ($ y) <*> u
 -
 - using Homomorphism:
 - pure f <*> pure x = pure (f x)
 -
 - pure ($ y) =
 - pure ((flip ($)) y) =
 - pure (flip ($)) <*> pure y
 -
 - putting it all together:
 - pure f <*> x =
 - pure ($ y) <*> u =
 - pure (flip ($)) <*> pure y <*> u =
 - pure (flip ($)) <*> x <*> pure f
 -
 -}

instance Applicative Maybe where
  pure = Just
  Just f <*> Just a = Just (f a)
  _ <*> _ = Nothing

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (f <$> xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a,b)

pure' :: Monoidal f => a -> f a
pure' a = fmap (\_ -> a) unit

(<*>.) :: Monoidal f => f (a -> b) -> f a -> f b
(<*>.) g f = fmap (\(h, x) -> h x) (g ** f)
