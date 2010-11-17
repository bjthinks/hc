{-# LANGUAGE DeriveDataTypeable #-}

module HCException (HCException(..)) where

import Data.Typeable
import Control.Exception

data HCException = HCDivideByZero
                 deriving (Show, Typeable)

instance Exception HCException
