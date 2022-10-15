module Lib.Types (EvalType, EulerParams (..), EulerStep (..)) where

type EvalType = Double

data EulerParams = EulerParams
  { eulerFunction  :: EvalType -> EvalType -> EvalType,
    eulerLimits    :: (EvalType, EvalType),
    eulerPrecision :: Maybe Int,
    eulerInitial   :: EvalType,
    eulerStep      :: EvalType
  }

data EulerStep = EulerStep
  { stepX :: EvalType,
    stepY :: EvalType,
    stepF :: EvalType,
    stepD :: EvalType
  }
  deriving (Show)
