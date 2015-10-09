module Compile where

import Parse
import Codegen
import Asm
import Ast
import Text.ParserCombinators.Parsec (ParseError)

compile :: FilePath -> String -> Either ParseError String
compile f xs =
    case parseFile f xs of
      Right exps -> Right . pp . runCodegen $ generate exps
      Left err   -> Left err

