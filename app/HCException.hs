{-# LANGUAGE DeriveDataTypeable #-}

module HCException (HCException(..),hcErrorMessage) where

import Data.Typeable
import Control.Exception

data HCException = HCDivideByZero |
                   HCNonIntegerPower |
                   HCSubstituteNotVariable |
                   HCWrongNumberOfParameters String Int
  deriving (Show, Typeable)

instance Exception HCException

hcErrorMessage :: HCException -> String
hcErrorMessage HCDivideByZero = "Error: division by zero."
hcErrorMessage HCNonIntegerPower = "Error: exponents must be integers."
hcErrorMessage HCSubstituteNotVariable = "Error: the first parameter to " ++
  "substitute must be a variable."
hcErrorMessage (HCWrongNumberOfParameters f n) = "Error: function " ++ f ++
  " takes " ++ show n ++ " parameter" ++ (if n /= 1 then "s" else "") ++ "."
