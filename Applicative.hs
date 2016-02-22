{-# OPTIONS_GHC -Wall #-}
module Functor where

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

