module Lib
  ( someFunc,
  )
where

import Data.List (intersperse)
import Numeric (showFFloat)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type EvalType = Double

data EulerParams = EulerParams
  { eulerFunction :: EvalType -> EvalType -> EvalType,
    eulerLimits :: (EvalType, EvalType),
    eulerInitial :: EvalType,
    eulerStep :: EvalType
  }

data EulerStep = EulerStep
  { stepX :: EvalType,
    stepY :: EvalType,
    stepF :: EvalType,
    stepD :: EvalType
  }
  deriving (Show)

eulerShow :: [EulerStep] -> String
eulerShow = unlines . (firstRow :) . map showStep
  where
    firstRow = "x\t\t y\t\t f(x, y)\t d = hf(x, y)"

showStep :: EulerStep -> String
showStep step =
  unwords . intersperse "\t"
    . map
      showF
    $ [stepX, stepY, stepF, stepD]
  where
    precision = 5
    showF getter = showFFloat (Just precision) (getter step) ""

eulerMethod :: EulerParams -> [EulerStep]
eulerMethod params =
  let (a, b) = eulerLimits params
   in if a >= b + eulerStep params
        then []
        else eulerMethod' params
  where
    eulerMethod' params =
      let (x, b) = eulerLimits params
          y = eulerInitial params
          f = eulerFunction params x y
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
