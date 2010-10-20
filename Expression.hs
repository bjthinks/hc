module Expression (eInt, eVar, eSum, eProd, ePower,
                   eMatch,
                   useThisVariableOnlyForTestingTheExpressionConstructors,
                   Expression) where

import Data.Char (isAlpha)
import Data.List

data Expression = ExpressionVariable String |
                  ExpressionInteger Integer |
                  ExpressionSum [Expression] |
                  ExpressionProduct [Expression] |
                  ExpressionPower (Expression,Expression)
                  deriving (Show, Eq)

-- This is a TOTAL ORDER
instance Ord Expression where
  -- Integers are sorted by value
  compare (ExpressionInteger x) (ExpressionInteger y) = compare x y
  -- Variables are sorted in alphabetical order
  compare (ExpressionVariable x) (ExpressionVariable y) =
    compare x y
  -- Sums are compared by comparing their contents, looking first
  -- at nonconstants and using constants as a tiebreaker
  compare (ExpressionSum x) (ExpressionSum y) = compareSum x y
  -- Same for products
  compare (ExpressionProduct x) (ExpressionProduct y) = compareProduct x y
  -- Powers are sorted first by base, then exponent
  compare (ExpressionPower x) (ExpressionPower y) = compare x y

  -- Integer < Variable < Sum
  compare (ExpressionInteger _) (ExpressionVariable _) = LT
  compare (ExpressionVariable _) (ExpressionInteger _) = GT
  compare (ExpressionVariable _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionVariable _) = GT
  -- Integer < Product < Sum
  compare (ExpressionInteger _) (ExpressionProduct _) = LT
  compare (ExpressionProduct _) (ExpressionInteger _) = GT
  compare (ExpressionProduct _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionProduct _) = GT
  -- Integer < Sum
  compare (ExpressionInteger _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionInteger _) = GT
  -- Variables are like singleton Products, so Variables and
  -- Products are intermingled in the sort order.
  compare (ExpressionProduct x) y@(ExpressionVariable _) = compareProduct x [y]
  compare x@(ExpressionVariable _) (ExpressionProduct y) = compareProduct [x] y

  -- Anything not a power is like a thing to the power 1,
  -- so Powers are intermingled with everything else in the
  -- sort order.
  compare x y@(ExpressionPower _) =
    compare (ExpressionPower (x,ExpressionInteger 1)) y
  compare x@(ExpressionPower _) y =
    compare x (ExpressionPower (y,ExpressionInteger 1))

-- FIXME: I hope there's a cleaner way to do this
compareSum :: [Expression] -> [Expression] -> Ordering
compareSum xs ys = compareSumOrProd (addConstant 0 xs) (addConstant 0 ys)

compareProduct :: [Expression] -> [Expression] -> Ordering
compareProduct xs ys = compareSumOrProd (addConstant 1 xs) (addConstant 1 ys)

addConstant :: Integer -> [Expression] -> [Expression]
addConstant _ xs@(ExpressionInteger _:_) = xs
addConstant n xs = ExpressionInteger n:xs

-- A sum or product is compared against another sum or product, resp.,
-- by first trying to compare the nonconstant terms, then the constant.
compareSumOrProd :: [Expression] -> [Expression] -> Ordering
compareSumOrProd (ExpressionInteger m:xs) (ExpressionInteger n:ys) =
  case compareExprList xs ys of
    LT -> LT
    GT -> GT
    EQ -> compare m n
compareExprList :: [Expression] -> [Expression] -> Ordering
compareExprList (x:xs) (y:ys) =
  case compare x y of
    LT -> LT
    GT -> GT
    EQ -> compareExprList xs ys
compareExprList [] [] = EQ
-- HERE'S THE BEEF
compareExprList (_:_) [] = LT
compareExprList [] (_:_) = GT

eMatch :: (Integer -> a) -> (String -> a) -> ([Expression] -> a) ->
          ([Expression] -> a) -> ((Expression,Expression) -> a) ->
          Expression -> a
eMatch f _ _ _ _ (ExpressionInteger n) = f n
eMatch _ f _ _ _ (ExpressionVariable s) = f s
eMatch _ _ f _ _ (ExpressionSum es) = f es
eMatch _ _ _ f _ (ExpressionProduct es) = f es
eMatch _ _ _ _ f (ExpressionPower ep) = f ep

{-
pullCoeffSum :: [Expression] -> (Integer,[Expression])
pullCoeffSum (ExpressionInteger n:es) = (n,es)
pullCoeffSum es = (0,es)
pullCoeffProd :: [Expression] -> (Integer,[Expression])
pullCoeffProd (ExpressionInteger n:es) = (n,es)
pullCoeffProd es = (1,es)
-}

useThisVariableOnlyForTestingTheExpressionConstructors =
  (ExpressionInteger, ExpressionVariable,
   ExpressionSum, ExpressionProduct, ExpressionPower)

-------------------- INTEGERS --------------------

-- This serves little purpose now, but may later if
-- we change it so signs are stored separately
eInt :: Integer -> Expression
eInt n = ExpressionInteger n

-------------------- VARIABLES --------------------

eVar :: String -> Expression
eVar "" = error "invalid variable name"
-- FIXME: should also check that subsequent chars are alphanumeric
eVar (v:vs)
  | isAlpha v = ExpressionVariable (v:vs)
  | otherwise = error "invalid variable name"

-------------------- SUMS --------------------

-- Return a sum in standard form, assuming that all summands are
-- already in standard form.
-- Invariants, in order of processing:
--   1. a sum contains no sums
--   2. a sum is sorted
--   3. like terms of a sum are combined together, and removed if zero
--   4. a sum contains at least 2 elements
eSum :: [Expression] -> Expression
eSum exprs = makeSum $
             combineSummands $
             sort $
             flattenSummands $
             exprs

flattenSummands :: [Expression] -> [Expression]
flattenSummands (ExpressionSum summands:es) = summands ++ flattenSummands es
flattenSummands (e:es) = e:flattenSummands es
flattenSummands [] = []

combineSummands :: [Expression] -> [Expression]
combineSummands es = map pushCoeff $ combineSummands' $ map popCoeff es
popCoeff :: Expression -> (Integer,Expression)
popCoeff (ExpressionInteger n) = (n,ExpressionInteger 1)
popCoeff (ExpressionProduct [ExpressionInteger n,e]) = (n,e)
popCoeff (ExpressionProduct (ExpressionInteger n:es)) =
  (n,ExpressionProduct es)
popCoeff x = (1,x)
combineSummands' :: [(Integer,Expression)] -> [(Integer,Expression)]
combineSummands' ((m,e):(n,f):gs)
  | e == f     = combineSummands' ((m+n,e):gs)
  | m == 0     = combineSummands' ((n,f):gs)
  | otherwise  = (m,e):combineSummands' ((n,f):gs)
combineSummands' xs = xs
pushCoeff :: (Integer,Expression) -> Expression
pushCoeff (1,e) = e
pushCoeff (n,ExpressionInteger 1) = ExpressionInteger n
pushCoeff (n,ExpressionProduct es) = ExpressionProduct (ExpressionInteger n:es)
pushCoeff (n,e) = ExpressionProduct [ExpressionInteger n,e]

makeSum :: [Expression] -> Expression
makeSum [] = eInt 0
makeSum [e] = e
makeSum es = ExpressionSum es

-------------------- PRODUCTS --------------------

-- Return a product in standard form, assuming that all factors are
-- already in standard form.
-- Invariants, in order of processing:
--   1. a product contains no products
--   2. a product is sorted
--   3. a product contains at most one constant, which is removed if 1
--   4. a product of zero with anything is zero
--   5. a product contains at least 2 elements
eProd :: [Expression] -> Expression
eProd exprs = makeProduct $
              detectZeroFactors $
              combineConstantFactors $
              sort $
              flattenFactors $
              exprs

flattenFactors :: [Expression] -> [Expression]
flattenFactors (ExpressionProduct terms:es) = terms ++ flattenFactors es
flattenFactors (e:es) = e:flattenFactors es
flattenFactors [] = []

combineConstantFactors :: [Expression] -> [Expression]
combineConstantFactors (ExpressionInteger m:ExpressionInteger n:es) =
  combineConstantFactors (ExpressionInteger (m*n):es)
combineConstantFactors (ExpressionInteger 1:es) = es
combineConstantFactors es = es

detectZeroFactors :: [Expression] -> [Expression]
detectZeroFactors (ExpressionInteger 0:_) = [ExpressionInteger 0]
detectZeroFactors es = es

makeProduct :: [Expression] -> Expression
makeProduct [] = eInt 1
makeProduct [e] = e
makeProduct es = ExpressionProduct es

-------------------- POWERS --------------------

ePower :: (Expression,Expression) -> Expression
ePower = ExpressionPower
