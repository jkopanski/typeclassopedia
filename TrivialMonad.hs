{-# OPTIONS_GHC -Wall #-}

module TrivialMonad where

newtype W a = W a deriving Show

return' :: a -> W a
return' x = W x

fmap :: (a -> b) -> (W a -> W b)
fmap f (W x) = W (f x)

inc :: Int -> W Int
inc x = W (x+1)

bind :: (a -> W b) -> (W a -> W b)
bind f (W x) = f x

add :: Int -> W Int -> W Int
-- add x (W y) = W (x + y)
add x w = bind (return' . (+x)) w

add' :: W Int -> W Int -> W Int
-- add' (W x) (W y) = W (x + y)
add' x y = bind (\b -> add b y) x

join :: W (W a) -> W a
join w = bind w id
