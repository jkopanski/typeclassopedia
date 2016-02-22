{-# OPTIONS_GHC -Wall #-}
module Functor where

import Data.Either

instance Functor (Either e) where
  fmap _ (Left a) = Left a
  fmap g (Right a) = Right (g a)

instance Functor ((->) e) where
  fmap = (.)

instance Functor ((,) e) where
  fmap g (a,b) = (g a,b)

data Pair a = Pair a a

instance Functor Pair where
  fmap g (Pair a b) = Pair (g a) (g b)

-- type Pred a = a -> Bool
-- true [[a]]

instance Functor 
