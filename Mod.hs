module Mod where
import Data.HashTable
import Data.Int

one :: Integer
one = 999000

plus :: Int -> Int -> Int
plus = (+)

hash :: String -> Int32
hash = hashString

greeting :: String
greeting = "hi there, from Haskell!"
