module Operators where

import Prelude hiding (EQ)

--BEGIN:UnBinOps
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Show, Eq)

data UnaryOp = Neg | Not
  deriving (Show, Eq)
--END:UnBinOps

