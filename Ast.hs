module Ast where

data Expr = Lit Lit
          | NamedBlock String [Expr]
          | AnonBlock [Expr]
          | Add
          | Sub
          | Mul
          | Div
          | Outchr
          | Call
          deriving (Eq, Show)

data Lit = Int Int
         | Char Char
         | Ident String
         deriving (Eq, Show)

