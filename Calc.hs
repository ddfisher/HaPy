{-# LANGUAGE OverloadedStrings #-}

module Calc (evaluate) where
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as P8

import Control.Applicative hiding (many)
import Data.Word (Word8)
import Data.Char

import Data.Maybe
import Control.Monad

data Expr = Add
          | Subtract
          | Multiply
          | Divide
          | Number Double
          deriving Show

operators = [("+", Add)
            ,("-", Subtract)
            ,("*", Multiply)
            ,("/", Divide)
            ]

parseSpaces = skipMany P8.space

parseNumber :: Parser Expr
parseNumber = Number <$> P8.rational

parseOperator :: Parser Expr
parseOperator = (fromJust . (`lookup` operators)) <$> (choice $ map (string.fst) operators)

-- pushNumber :: Parser ([Expr] -> [Expr])
-- pushNumber = (:) <$> parseNumber

-- pushOperator :: Parser ([Expr] -> [Expr])
-- pushOperator = parseOperator <$> (head <$>
--     where push op (e1:e2:exprs) = op e1 e2 : exprs
-- pushOperator = push `liftM` parseOperator
--     where push op (e1:e2:exprs) = op e1 e2 : exprs
          -- push _ _ = fail "Stack too small"

parseExpr :: Parser Expr
parseExpr = parseSpaces *> choice [parseNumber, parseOperator] <* parseSpaces

parseExprs :: Parser [Expr]
parseExprs = many1 parseExpr

evalExprs :: [Expr] -> Maybe [Double]
evalExprs = foldl (flip changeStack) (Just [])
    where changeStack (Number n) (Just stack)        = Just (n:stack)
          changeStack Add        (Just (n1:n2:rest)) = Just (n2+n1:rest)
          changeStack Subtract   (Just (n1:n2:rest)) = Just (n2-n1:rest)
          changeStack Multiply   (Just (n1:n2:rest)) = Just (n2*n1:rest)
          changeStack Divide     (Just (n1:n2:rest)) = Just (n2/n1:rest)
          changeStack _          _                   = Nothing

evaluate :: String -> Double
evaluate str = case parseOnly parseExprs (B8.pack str) of
                    Right exprs -> fromMaybe (0/0) (getFirst $ evalExprs exprs)
                    Left _      -> (0/0)
            where getFirst (Just [x]) = Just x
                  getFirst _          = Nothing
