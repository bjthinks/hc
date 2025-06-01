module Factor.Defs where

type Coeff = Integer
type Exponent = Integer
data Term = Term { termCoeff :: Coeff, termExponent :: Exponent }
  deriving (Eq, Show, Read)
data Factorization p = Factorization Coeff [(p,Exponent)]
  deriving (Eq, Show, Read)
