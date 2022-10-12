module Main (main) where

import Lib.Show
import Lib.Solve
import Lib.Types
import Text.Parsec.Expr.Math

main :: IO ()
main = do
  putStrLn "Enter the first derivative of the function:"
  f <- getLine

  putStrLn "Enter the left bound of the interval:"
  intervalLeft <- getLine

  putStrLn "Enter the right bound of the interval:"
  intervalRight <- getLine

  putStrLn "Enter the precision:"
  precision <- getLine

  putStrLn "Enter the initial value:"
  initialValue <- getLine

  putStrLn "Enter the maximum number of iterations:"
  maxIterations <- getLine

  let maxIterationsValue = read maxIterations :: Int

  let params =
        EulerParams
          { eulerFunction = math2haskell (unwrap (parse f)),
            eulerLimits = (read intervalLeft, read intervalRight),
            eulerPrecision = Just (read precision),
            eulerInitial = read initialValue,
            eulerStep = (intervalLeft + intervalRight) / maxIterationsValue
          }

  putStrLn (showEulerMarkdown (eulerMethod params))
  where
    unwrap (Right x) = x
