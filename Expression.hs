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
--   3. like terms of a sum are combined together
--   4. a sum contains at least 2 elements
eSum :: [Expression] -> Expression
eSum exprs = makeSum $
             combineSummands $
             sortSummands $
             flattenSummands $
             exprs

flattenSummands :: [Expression] -> [Expression]
flattenSummands (ExpressionSum summands:es) = summands ++ flattenSummands es
flattenSummands (e:es) = e:flattenSummands es
flattenSummands [] = []

-- FIXME
sortSummands :: [Expression] -> [Expression]
sortSummands = id

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
--   3. a product contains at most one constant
--   4. a product of zero with anything is zero
--   5. a product contains at least 2 elements
eProd :: [Expression] -> Expression
eProd exprs = makeProduct $
              detectZeroFactors $
              combineConstantFactors $
              sortFactors $
              flattenFactors $
              exprs

flattenFactors :: [Expression] -> [Expression]
flattenFactors (ExpressionProduct terms:es) = terms ++ flattenFactors es
flattenFactors (e:es) = e:flattenFactors es
flattenFactors [] = []

-- FIXME
sortFactors :: [Expression] -> [Expression]
sortFactors = id

combineConstantFactors :: [Expression] -> [Expression]
combineConstantFactors (ExpressionInteger m:ExpressionInteger n:es) =
  combineConstantFactors (ExpressionInteger (m*n):es)
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
instance Ord Expression where
  -- Variables are sorted in alphabetical order
  compare (ExpressionVariable x) (ExpressionVariable y) =
    compare x y
  -- Variables come before integers
  compare (ExpressionVariable _) (ExpressionInteger _) = LT
  compare (ExpressionInteger _) (ExpressionVariable _) = GT
  -- Integers all go together, sorted by absolute value
  compare (ExpressionInteger x) (ExpressionInteger y) = compare (abs x) (abs y)
  -- In products, sums come after variables but before constants
  compare (ExpressionSum _) (ExpressionVariable _) = GT
  compare (ExpressionVariable _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionInteger _) = LT
  compare (ExpressionInteger _) (ExpressionSum _) = GT
  -- Sums are compared by comparing their contents
  compare (ExpressionSum x) (ExpressionSum y) = compareExprList x y
  -- Same for products
  compare (ExpressionProduct x) (ExpressionProduct y) = compareExprList x y
  -- In sums, integers and variables are compared with products by
  -- treating them as singleton products
  compare (ExpressionProduct x) y@(ExpressionVariable _) = compare x [y]
  compare (ExpressionProduct x) y@(ExpressionInteger _) = compare x [y]
  compare x@(ExpressionVariable _) (ExpressionProduct y) = compare [x] y
  compare x@(ExpressionInteger _) (ExpressionProduct y) = compare [x] y
  -- Products come before sums
  compare (ExpressionProduct _) (ExpressionSum _) = LT
  compare (ExpressionSum _) (ExpressionProduct _) = GT

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

-- Put an expression into "standard form".  This performs a series
-- of internal and basic algebraic simplifications, including:
-- nested products are flattened
-- products of zero are removed
-- terms of a product are sorted
--   (note that constants are internally placed last)
-- in a product, ones are removed, and (-1)*(-1) is removed
-- empty products are replaced with one, singleton products are unwrapped
-- nested sums are flattened
-- sums are sorted
-- sums of constants are evaluated, and zeroes are removed
-- like terms of sums are combined together
-- empty sums are replaced with zero, singleton sums are unwrapped
standardForm :: Expression -> Expression
standardForm e =
  let e' = removeTrivialSums $ combineSummands $ addConstants $
           sortSums $ flattenSums $
           removeTrivialProducts $ multiplyConstants $
           sortProducts $ destroyZeroProducts $ flattenProducts e in
  case e == e' of
    True -> e'
    False -> standardForm e'

flattenProducts :: Expression -> Expression
flattenProducts (ExpressionProduct es) = ExpressionProduct $
                                         flattenProducts' es' where
  es' = map flattenProducts es
  flattenProducts' :: [Expression] -> [Expression]
  flattenProducts' (ExpressionProduct nes:es) = flattenProducts' (nes ++ es)
  flattenProducts' (e:es) = e:flattenProducts' es
  flattenProducts' [] = []
flattenProducts (ExpressionSum es) = ExpressionSum (map flattenProducts es)
flattenProducts e@(_) = e

destroyZeroProducts :: Expression -> Expression
destroyZeroProducts (ExpressionProduct es)
  | hasZero es = ExpressionInteger 0
  | otherwise = ExpressionProduct (map destroyZeroProducts es)
    where
      hasZero :: [Expression] -> Bool
      hasZero (ExpressionInteger 0:_) = True
      hasZero (_:es) = hasZero es
      hasZero [] = False
destroyZeroProducts (ExpressionSum es) =
  ExpressionSum (map destroyZeroProducts es)
destroyZeroProducts e@(_) = e

sortProducts :: Expression -> Expression
sortProducts (ExpressionProduct es) =
  ExpressionProduct $ sort $ map sortProducts es
sortProducts (ExpressionSum es) = ExpressionSum $ map sortProducts es
sortProducts e@(_) = e

multiplyConstants :: Expression -> Expression
multiplyConstants (ExpressionProduct es) =
  ExpressionProduct (multiplyConstants' es') where
    es' = map multiplyConstants es
    multiplyConstants' (ExpressionInteger 1:es) = multiplyConstants' es
    multiplyConstants' (ExpressionInteger x:ExpressionInteger y:es) =
      multiplyConstants' (ExpressionInteger (x*y):es)
    multiplyConstants' (e:es) = e:multiplyConstants' es
    multiplyConstants' [] = []
multiplyConstants (ExpressionSum es) = ExpressionSum (map multiplyConstants es)
multiplyConstants e@(_) = e

removeTrivialProducts :: Expression -> Expression
removeTrivialProducts (ExpressionProduct []) = ExpressionInteger 1
removeTrivialProducts (ExpressionProduct [e]) = removeTrivialProducts e
removeTrivialProducts (ExpressionProduct es) =
  ExpressionProduct $ map removeTrivialProducts es
removeTrivialProducts (ExpressionSum es) =
  ExpressionSum (map removeTrivialProducts es)
removeTrivialProducts e@(_) = e

sortSums :: Expression -> Expression
sortSums s@(ExpressionSum []) = s
sortSums (ExpressionSum es) =
  ExpressionSum $ removeTrivialProducts $ sort $ makeTrivialProducts $
  map sortSums es
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
sortSums (ExpressionProduct es) = ExpressionProduct (map sortSums es)
sortSums e@(_) = e

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

removeTrivialSums :: Expression -> Expression
removeTrivialSums (ExpressionSum []) = ExpressionInteger 0
removeTrivialSums (ExpressionSum [e]) = removeTrivialSums e
removeTrivialSums (ExpressionSum es) = ExpressionSum $ map removeTrivialSums es
removeTrivialSums (ExpressionProduct es) =
  ExpressionProduct (map removeTrivialSums es)
removeTrivialSums e@(_) = e
-}
