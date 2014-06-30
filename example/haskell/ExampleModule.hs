module ExampleModule where

import Data.Char

foo :: Double -> Double -> Double
foo = (*)

bar :: Int -> Int
bar i = sum [1..i]

baz :: Int -> Bool
baz = (> 5)

arr_arg :: [Int] -> Int
arr_arg = sum

arr_ret :: Int -> [Int]
arr_ret i = [1..i]

arr_complex :: [[Int]] -> [[Int]]
arr_complex = map (map (* 2))

string_fun :: String -> String
string_fun str = str ++ reverse str

char_test :: Char -> Int
char_test = ord
