module Expression (Expression(..), standardForm) where

import Data.List

data Expression = ExpressionVariable String |
                  ExpressionInteger Integer |
                  ExpressionSum [Expression] |
                  ExpressionProduct [Expression]
                  deriving (Show, Eq)

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
-- terms of a product are sorted
--   (note that constants are internally placed last)
-- in a product, ones are removed, and (-1)*(-1) is removed
-- empty products are replaced with one, singleton products are unwrapped
-- nested sums are flattened
-- sums are sorted
-- sums of constants are evaluated, and zeroes are removed
-- empty sums are replaced with zero, singleton sums are unwrapped
standardForm :: Expression -> Expression
standardForm e =
  let e' = removeTrivialSums $ addConstants $ sortSums $ flattenSums $
           removeTrivialProducts $ multiplyUnits $
           sortProducts $ flattenProducts e in
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

sortProducts :: Expression -> Expression
sortProducts (ExpressionProduct es) =
  ExpressionProduct $ sort $ map sortProducts es
sortProducts (ExpressionSum es) = ExpressionSum $ map sortProducts es
sortProducts e@(_) = e

removeTrivialProducts :: Expression -> Expression
removeTrivialProducts (ExpressionProduct []) = ExpressionInteger 1
removeTrivialProducts (ExpressionProduct [e]) = removeTrivialProducts e
removeTrivialProducts (ExpressionProduct es) =
  ExpressionProduct $ map removeTrivialProducts es
removeTrivialProducts (ExpressionSum es) =
  ExpressionSum (map removeTrivialProducts es)
removeTrivialProducts e@(_) = e

flattenSums :: Expression -> Expression
flattenSums (ExpressionSum es) = ExpressionSum $ flattenSums' es' where
  es' = map flattenSums es
  flattenSums' :: [Expression] -> [Expression]
  flattenSums' (ExpressionSum nes:es) = flattenSums' (nes ++ es)
  flattenSums' (e:es) = e:flattenSums' es
  flattenSums' [] = []
flattenSums (ExpressionProduct es) = ExpressionProduct (map flattenSums es)
flattenSums e@(_) = e

multiplyUnits :: Expression -> Expression
multiplyUnits (ExpressionProduct es) =
  ExpressionProduct (multiplyUnits' es') where
    es' = map multiplyUnits es
    multiplyUnits' (ExpressionInteger 1:es) = multiplyUnits' es
    multiplyUnits' (ExpressionInteger (-1):ExpressionInteger (-1):es) =
      multiplyUnits' es
    multiplyUnits' (e:es) = e:multiplyUnits' es
    multiplyUnits' [] = []
multiplyUnits (ExpressionSum es) = ExpressionSum (map multiplyUnits es)
multiplyUnits e@(_) = e

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

removeTrivialSums :: Expression -> Expression
removeTrivialSums (ExpressionSum []) = ExpressionInteger 0
removeTrivialSums (ExpressionSum [e]) = removeTrivialSums e
removeTrivialSums (ExpressionSum es) = ExpressionSum $ map removeTrivialSums es
removeTrivialSums (ExpressionProduct es) =
  ExpressionProduct (map removeTrivialSums es)
removeTrivialSums e@(_) = e
