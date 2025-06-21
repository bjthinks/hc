{-# LANGUAGE DeriveDataTypeable #-}

module HCException (HCException(..), HCExit(..), hcErrorMessage) where

import Data.Typeable
import Control.Exception

data HCException = HCAssignmentLoop |
                   HCDivideByZero |
                   HCNonIntegerPower |
                   HCReservedWord |
                   HCSubstituteNotVariable |
                   HCWrongNumberOfParameters String Int
  deriving (Show, Typeable)

data HCExit = HCExit
  deriving (Show, Typeable)

instance Exception HCException

instance Exception HCExit

hcErrorMessage :: HCException -> String
hcErrorMessage HCAssignmentLoop = "Error: assignments form a loop."
hcErrorMessage HCDivideByZero = "Error: division by zero."
hcErrorMessage HCNonIntegerPower = "Error: exponents must be integers."
hcErrorMessage HCReservedWord =
  "Error: that name is reserved for built-in functionality."
hcErrorMessage HCSubstituteNotVariable = "Error: the first parameter to " ++
  "substitute must be a variable."
hcErrorMessage (HCWrongNumberOfParameters f n) = "Error: function " ++ f ++
  " takes " ++ show n ++ " parameter" ++ (if n /= 1 then "s" else "") ++ "."
