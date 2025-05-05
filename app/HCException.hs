{-# LANGUAGE DeriveDataTypeable #-}

module HCException (HCException(..),hcErrorMessage) where

import Data.Typeable
import Control.Exception

data HCException = HCDivideByZero |
                   HCNonIntegerPower |
                   HCUnknownFunction String
                 deriving (Show, Typeable)

instance Exception HCException

hcErrorMessage :: HCException -> String
hcErrorMessage HCDivideByZero = "Error: division by zero"
hcErrorMessage HCNonIntegerPower = "Error: exponents must be integers"
hcErrorMessage (HCUnknownFunction f) = "Error: unknown function " ++ f
