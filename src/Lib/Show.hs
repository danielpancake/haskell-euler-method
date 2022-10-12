module Lib.Show (eulerShow, showStep, showStepWithIndent, showEulerMarkdown) where

import Data.List (intersperse)
import Lib.Types
import Numeric (showFFloat)

eulerShow :: Maybe Int -> [EulerStep] -> String
eulerShow precision = unlines . (firstRow :) . map (showStep precision)
  where
    firstRow = "x\t\t y\t\t f(x, y)\t d = hf(x, y)"

showStep :: Maybe Int -> EulerStep -> String
showStep = showStepWithIndent "\t"

showStepWithIndent :: String -> Maybe Int -> EulerStep -> String
showStepWithIndent indent precision step =
  unwords . intersperse indent
    . map
      showF
    $ [stepX, stepY, stepF, stepD]
  where
    showF getter = showFFloat precision (getter step) ""

showEulerMarkdown :: Maybe Int -> [EulerStep] -> String
showEulerMarkdown precision = unlines . (\rows -> firstRow : "| --- |" : rows) . map printRow
  where
    firstRow = "| x | y | f(x, y) | d = hf(x, y) |"
    printRow row = "| " ++ showStepWithIndent "|" precision row ++ " |"
