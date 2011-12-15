module Newton where

import Data.List
import Data.List.Split

-- we represent polynomials as lists of pairs of (Double, Double)
-- the pair (a, n) corresponds to the term a*x^n
type Polynomial = [(Double, Double)]

parsePolynomial :: String -> Polynomial
parsePolynomial = map convertTerm . parse
    where parse str = parseAcc "" [] str
          parseAcc acc terms (c:cs) = case c of
                                        '+' -> parseAcc "" (acc:terms) cs
                                        '-' -> parseAcc "-" (acc:terms) cs
                                        _   -> parseAcc (acc ++ [c]) terms cs
          convertTerm = (\term -> ((read $ head $ splitOn "x^" term) :: Double, (read $ last $ splitOn "x^" term) :: Double))

evaluate :: Polynomial -> Double -> Double
evaluate poly x = foldl (\acc (coeff, exp) -> acc + coeff * (x ** exp)) 0 poly

polyDerivative :: Polynomial -> Polynomial
polyDerivative = map (\(coeff, exp) -> (coeff * exp, exp - 1))

{-
newton :: (Fractional t, Ord t) => (t -> t) -> (t -> t) -> t -> [t]
newton f f' x0 = iterate next x0
    where next xn = xn - ((f xn)/(f' xn))

findZero :: (Fractional t, Ord t) => (t -> t) -> (t -> t) -> t -> t -> t
findZero f f' x0 epsilon = case find (\x -> (abs $ f x) < epsilon) (newton f f' x0) of
                             Just root -> root
                             Nothing   -> undefined
-}
