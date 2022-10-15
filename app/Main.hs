module Main (main) where

import           Lib.Show
import           Lib.Solve
import           Lib.Types
import           System.IO
import           Text.Parsec.Expr.Math

main :: IO ()
main = do
  putStrLn "Enter the first derivative of the function y(x):"
  putStr "y' = "
  hFlush stdout
  f <- getLine

  let function = unwrap (parse f)

  putStrLn "Enter the initial value of x:"
  putStr "x0 = "
  hFlush stdout
  x0 <- getLine

  putStrLn "Enter the initial value of y(x0):"
  putStr "y(x0) = "
  hFlush stdout
  y0 <- getLine

  putStrLn "Enter the point of approximation:"
  putStr "x = "
  hFlush stdout
  xfinal <- getLine

  putStrLn "Enter the step:"
  putStr "h = "
  hFlush stdout
  h <- getLine

  putStrLn "Enter the precision (optional):"
  precision <- getLine

  let precisionValue = if precision == "" then Nothing else Just (read precision)

  let params = EulerParams
        { eulerFunction = math2haskell function
        , eulerLimits = (read x0, read xfinal)
        , eulerPrecision = precisionValue
        , eulerInitial = read y0
        , eulerStep = read h
        }

  putStrLn (showEulerMarkdown precisionValue (eulerMethod params))
  putStrLn "Done."

  where
    unwrap (Right x) = x
    unwrap (Left x) = error (show x)
