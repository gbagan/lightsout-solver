module Lightsout.Solver where

import Prelude
import Data.Maybe (Maybe)
import Data.Ord (abs)
import LinearAlgebra.Matrix (Matrix)
import LinearAlgebra.Matrix as M
import LinearAlgebra.Vector as V

newtype F2 = F2 Boolean
derive instance Eq F2

instance Semiring F2 where
  add (F2 x) (F2 y) = F2 (x /= y)
  mul (F2 x) (F2 y) = F2 (x && y)
  zero = F2 false
  one = F2 true

instance Ring F2 where
  sub = add

instance EuclideanRing F2 where
  div = mul
  mod _ _ = zero
  degree _ = 1

instance CommutativeRing F2
instance DivisionRing F2 where
  recip x = x

unF2 :: F2 -> Boolean
unF2 (F2 x) = x

generateMatrix :: Int -> Int -> Matrix F2
generateMatrix n m = M.fromFunction (n * m) (n * m) \i j -> F2 (neighbor i j) 
  where
  neighbor i j = abs (x1 - x2) + abs (y1 - y2) <= 1 
    where
    x1 = i `mod` m
    y1 = i / m
    x2 = j `mod` m
    y2 = j / m

solve :: Int -> Int -> Array Boolean -> Maybe (Array Boolean)
solve n m conf = do
  {sol} <- M.solveLinearSystem (generateMatrix n m) (V.fromArray $ F2 <$> conf)
  pure $ unF2 <$> V.toArray sol