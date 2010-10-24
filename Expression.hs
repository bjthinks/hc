module Expression (eRat, eVar, eSum, eProd, eIntPow,
                   eMatch, eAsSum, eTransform,
                   useThisVariableOnlyForTestingTheExpressionConstructors,
                   Expression) where

import Data.Char (isAlpha)
import Data.List

data Expression = ExpressionVariable String |
                  ExpressionRational Rational |
                  ExpressionSum [Expression] |
                  ExpressionProduct [Expression] |
                  ExpressionIntPow Expression Integer
                  deriving (Show, Eq)

-- This is a TOTAL ORDER
instance Ord Expression where
  -- Rationals are sorted by value
  compare (ExpressionRational x) (ExpressionRational y) = compare x y
  -- Variables are sorted in alphabetical order
  compare (ExpressionVariable x) (ExpressionVariable y) =
    compare x y
  -- Sums are compared by comparing their contents, looking first
  -- at nonconstants and using constants as a tiebreaker
  compare (ExpressionSum x) (ExpressionSum y) = compareSum x y
  -- Same for products
  compare (ExpressionProduct x) (ExpressionProduct y) = compareProduct x y
  -- IntPows are compared base first, then by exponent in reverse order
  compare (ExpressionIntPow x a) (ExpressionIntPow y b) = compare (x,-a) (y,-b)
  -- Rational < Variable < Sum
  compare (ExpressionRational _) (ExpressionVariable _) = LT
  compare (ExpressionVariable _) (ExpressionRational _) = GT
  compare (ExpressionVariable _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionVariable _) = GT
  -- Rational < Product < Sum
  compare (ExpressionRational _) (ExpressionProduct _) = LT
  compare (ExpressionProduct _) (ExpressionRational _) = GT
  compare (ExpressionProduct _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionProduct _) = GT
  -- Rational < Sum
  compare (ExpressionRational _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionRational _) = GT
  -- Variables are like singleton Products, so Variables and
  -- Products are intermingled in the sort order.
  compare (ExpressionProduct x) y@(ExpressionVariable _) = compareProduct x [y]
  compare x@(ExpressionVariable _) (ExpressionProduct y) = compareProduct [x] y
  -- IntPows are compared against anything else by pretending the
  -- something else is raised to the first power.
  compare x y@(ExpressionIntPow _ _) = compare (ExpressionIntPow x 1) y
  compare x@(ExpressionIntPow _ _) y = compare x (ExpressionIntPow y 1)

-- FIXME: I hope there's a cleaner way to do this
compareSum :: [Expression] -> [Expression] -> Ordering
compareSum xs ys = compareSumOrProd (addConstant 0 xs) (addConstant 0 ys)

compareProduct :: [Expression] -> [Expression] -> Ordering
compareProduct xs ys = compareSumOrProd (addConstant 1 xs) (addConstant 1 ys)

addConstant :: Rational -> [Expression] -> [Expression]
addConstant _ xs@(ExpressionRational _:_) = xs
addConstant n xs = eRat n:xs

-- A sum or product is compared against another sum or product, resp.,
-- by first trying to compare the nonconstant terms, then the constant.
compareSumOrProd :: [Expression] -> [Expression] -> Ordering
compareSumOrProd (ExpressionRational m:xs) (ExpressionRational n:ys) =
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

eMatch :: (Rational -> a) -> (String -> a) -> ([Expression] -> a) ->
          ([Expression] -> a) -> (Expression -> Integer -> a) ->
          Expression -> a
eMatch f _ _ _ _ (ExpressionRational n) = f n
eMatch _ f _ _ _ (ExpressionVariable s) = f s
eMatch _ _ f _ _ (ExpressionSum es) = f es
eMatch _ _ _ f _ (ExpressionProduct es) = f es
eMatch _ _ _ _ f (ExpressionIntPow e n) = f e n

list :: a -> [a]
list x = x:[]

eAsSum :: Expression -> [Expression]
eAsSum =
  eMatch (list . eRat) (list . eVar) id (list . eProd) (\e n -> [eIntPow e n])

eTransform :: ([Expression] -> Expression) -> ([Expression] -> Expression) ->
              (Expression -> Integer -> Expression) -> Expression -> Expression
eTransform f g h = eMatch eRat eVar
                   (f . map (eTransform f g h))
                   (g . map (eTransform f g h))
                   (\e n -> h (eTransform f g h e) n)

useThisVariableOnlyForTestingTheExpressionConstructors ::
  (Rational -> Expression, String -> Expression,
   [Expression] -> Expression, [Expression] -> Expression,
   Expression -> Integer -> Expression)
useThisVariableOnlyForTestingTheExpressionConstructors =
  (ExpressionRational, ExpressionVariable,
   ExpressionSum, ExpressionProduct, ExpressionIntPow)

-------------------- RATIONALS --------------------

-- This serves little purpose now.
eRat :: Rational -> Expression
eRat n = ExpressionRational n

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
popCoeff :: Expression -> (Rational,Expression)
popCoeff (ExpressionRational n) = (n,eRat 1)
popCoeff (ExpressionProduct (ExpressionRational n:es)) = (n,eProd es)
popCoeff x = (1,x)
combineSummands' :: [(Rational,Expression)] -> [(Rational,Expression)]
combineSummands' ((m,e):(n,f):gs)
  | e == f     = combineSummands' ((m+n,e):gs)
  | m == 0     = combineSummands' ((n,f):gs)
  | otherwise  = (m,e):combineSummands' ((n,f):gs)
combineSummands' xs = xs
pushCoeff :: (Rational,Expression) -> Expression
pushCoeff (c,e) = eProd [eRat c,e]

makeSum :: [Expression] -> Expression
makeSum [] = eRat 0
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
              combineFactors $
              sort $
              flattenFactors $
              exprs

flattenFactors :: [Expression] -> [Expression]
flattenFactors (ExpressionProduct terms:es) = terms ++ flattenFactors es
flattenFactors (e:es) = e:flattenFactors es
flattenFactors [] = []

combineFactors :: [Expression] -> [Expression]
combineFactors es = map pushPower $ combineFactors' $ map popPower es
popPower :: Expression -> (Expression,Integer)
popPower (ExpressionIntPow e n) = (e,n)
popPower e = (e,1)
combineFactors' :: [(Expression,Integer)] -> [(Expression,Integer)]
combineFactors' ((e,m):(f,n):gs)
  | e == f = combineFactors' ((e,m+n):gs)
  | m == 0 = combineFactors' ((f,n):gs)
  | otherwise = (e,m):combineFactors' ((f,n):gs)
combineFactors' xs = xs
pushPower :: (Expression,Integer) -> Expression
pushPower = uncurry eIntPow

combineConstantFactors :: [Expression] -> [Expression]
combineConstantFactors (ExpressionRational m:ExpressionRational n:es) =
  combineConstantFactors (eRat (m*n):es)
combineConstantFactors (ExpressionRational 1:es) = es
combineConstantFactors es = es

detectZeroFactors :: [Expression] -> [Expression]
detectZeroFactors (ExpressionRational 0:_) = [eRat 0]
detectZeroFactors es = es

makeProduct :: [Expression] -> Expression
makeProduct [] = eRat 1
makeProduct [e] = e
makeProduct es = ExpressionProduct es

-------------------- INTEGER POWERS --------------------

eIntPow :: Expression -> Integer -> Expression
eIntPow e 1 = e
eIntPow e 0 = eRat 1
eIntPow (ExpressionProduct xs) n = eProd $ map (flip eIntPow n) xs
eIntPow (ExpressionIntPow x m) n = eIntPow x (m*n)
eIntPow (ExpressionRational x) n
  | n < 0 && x == 0 = error "division by zero"
  | n < 0           = eRat (recip (x^(-n)))
  | otherwise       = eRat (x^n)
eIntPow x n = ExpressionIntPow x n
