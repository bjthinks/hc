module Expression (eInt, eVar, eSum, eProd,
                   eMatch,
                   useThisVariableOnlyForTestingTheExpressionConstructors,
                   Expression) where

import Data.Char (isAlpha)
import Data.List

data Expression = ExpressionVariable String |
                  ExpressionInteger Integer |
                  ExpressionSum [Expression] |
                  ExpressionProduct [Expression]
                  deriving (Show, Eq)

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

  -- Integer < Variable
  compare (ExpressionInteger _) (ExpressionVariable _) = LT
  compare (ExpressionVariable _) (ExpressionInteger _) = GT
  -- Variable < Sum
  compare (ExpressionSum _) (ExpressionVariable _) = GT
  compare (ExpressionVariable _) (ExpressionSum _) = LT
  -- Integer < Sum
  compare (ExpressionInteger _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionInteger _) = GT
  -- Integer < Product
  compare (ExpressionInteger _) (ExpressionProduct _) = LT
  compare (ExpressionProduct _) (ExpressionInteger _) = GT
  -- Variables are like singleton Products, so Variables and
  -- Products are intermingled in the sort order.
  -- FIXME: this should be compareProduct, right? Can I write a
  -- failing unit test here?
  compare (ExpressionProduct x) y@(ExpressionVariable _) = compare x [y]
  compare x@(ExpressionVariable _) (ExpressionProduct y) = compare [x] y
  -- Product < Sum
  compare (ExpressionProduct _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionProduct _) = GT

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
          ([Expression] -> a) -> Expression -> a
eMatch f _ _ _ (ExpressionInteger n) = f n
eMatch _ f _ _ (ExpressionVariable s) = f s
eMatch _ _ f _ (ExpressionSum es) = f es
eMatch _ _ _ f (ExpressionProduct es) = f es

useThisVariableOnlyForTestingTheExpressionConstructors =
  (ExpressionInteger, ExpressionVariable,
   ExpressionSum, ExpressionProduct)

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

-- FIXME
combineSummands :: [Expression] -> [Expression]
combineSummands = id

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

-- OLD STUFF
{-


    multiplyConstants' (ExpressionInteger 1:es) = multiplyConstants' es
    multiplyConstants' (ExpressionInteger x:ExpressionInteger y:es) =
      multiplyConstants' (ExpressionInteger (x*y):es)
    multiplyConstants' (e:es) = e:multiplyConstants' es
    multiplyConstants' [] = []


addConstants :: Expression -> Expression
addConstants (ExpressionSum es) = ExpressionSum (addConstants' es') where
  es' = map addConstants es
  addConstants' (ExpressionInteger 0:es) = addConstants' es
  addConstants' (ExpressionInteger m:ExpressionInteger n:es) =
    addConstants' (ExpressionInteger (m+n):es)
  addConstants' (e:es) = e:addConstants' es
  addConstants' [] = []
addConstants (ExpressionProduct es) = ExpressionProduct (map addConstants es)
addConstants e@(_) = e

combineSummands :: Expression -> Expression
combineSummands (ExpressionSum es) =
  ExpressionSum $ removeTrivialProducts $ combineSummands' $
  makeTrivialProducts es
  where
    makeTrivialProducts :: [Expression] -> [Expression]
    makeTrivialProducts (ExpressionProduct ps:xs) =
      ExpressionProduct ps:makeTrivialProducts xs
    makeTrivialProducts (x:xs) =
      ExpressionProduct [x]:makeTrivialProducts xs
    makeTrivialProducts [] = []
    removeTrivialProducts :: [Expression] -> [Expression]
    removeTrivialProducts (ExpressionProduct [x]:xs) =
      x:removeTrivialProducts xs
    removeTrivialProducts (x:xs) = x:removeTrivialProducts xs
    removeTrivialProducts [] = []

    combineSummands' :: [Expression] -> [Expression]
    combineSummands' (ExpressionProduct x1:ExpressionProduct x2:xs) =
      case (stripConstants x1) == (stripConstants x2) of
        False -> ExpressionProduct x1:
                 combineSummands' (ExpressionProduct x2:xs)
        True -> combineSummands'
                (ExpressionProduct
                 (stripConstants x1 ++
                  [standardForm (ExpressionSum
                                 [ExpressionProduct (getConstants x1),
                                  ExpressionProduct (getConstants x2)])])
                 :xs)
    combineSummands' [x] = [x]
    combineSummands' [] = []
    stripConstants :: [Expression] -> [Expression]
    stripConstants (ExpressionInteger _:es) = es
    stripConstants (x:xs) = x:stripConstants xs
    stripConstants [] = []
    getConstants :: [Expression] -> [Expression]
    getConstants es = case getConstants' es of
      [] -> [ExpressionInteger 1]
      xs -> xs
    getConstants' :: [Expression] -> [Expression]
    getConstants' (ExpressionInteger n:es) =
      ExpressionInteger n:getConstants es
    getConstants' (_:es) = getConstants es
    getConstants' [] = []
combineSummands (ExpressionProduct es) =
  ExpressionProduct (map combineSummands es)
combineSummands e@(_) = e
-}
