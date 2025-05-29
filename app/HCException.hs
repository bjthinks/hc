{-# LANGUAGE DeriveDataTypeable #-}

module HCException (HCException(..),hcErrorMessage) where

import Data.Typeable
import Control.Exception

data HCException = HCDivideByZero |
                   HCNonIntegerPower
  deriving (Show, Typeable)

instance Exception HCException

hcErrorMessage :: HCException -> String
hcErrorMessage HCDivideByZero = "Error: division by zero"
hcErrorMessage HCNonIntegerPower = "Error: exponents must be integers"
