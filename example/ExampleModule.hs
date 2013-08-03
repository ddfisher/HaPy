module ExampleModule where

foo :: Double -> Double -> Double
foo = (*)

bar :: Int -> Int
bar i = sum [1..i]

baz :: Int -> Bool
baz = (> 5)
