module Lib.Solve (approximate, math2haskell, eulerMethod) where

import qualified Data.Map as Map
import Data.Maybe
import Lib.Types
import Text.Parsec.Expr.Math

approximate :: EvalType -> Int -> EvalType
approximate x n = fromIntegral (round (x * m) :: Int) / m
  where
    m = 10 ^ n

math2haskell :: Expr EvalType -> EvalType -> EvalType -> EvalType
math2haskell expr x y = fromJust (evaluate vars (Just expr))
  where
    vars = Map.fromList [("x", x), ("y", y)]

eulerMethod :: EulerParams -> [EulerStep]
eulerMethod params =
  let (a, b) = eulerLimits params
   in if a >= b + eulerStep params
        then []
        else eulerMethod'
  where
    eulerMethod' =
      let (x, b) = eulerLimits params
          y = eulerInitial params

          val = eulerFunction params x y
          f = case eulerPrecision params of
            Just prec -> approximate val prec
            Nothing -> val

          h = eulerStep params
          d = h * f
          step =
            EulerStep
              { stepX = x,
                stepY = y,
                stepF = f,
                stepD = d
              }
          params' =
            params
              { eulerLimits = (x + h, b),
                eulerInitial = y + d
              }
       in step : eulerMethod params'
