module Compile where

import Asm
import Ast
import Error
import Parse
import Codegen

compile :: FilePath -> String -> Either Error String
compile f xs =
    case parseFile f xs of
      Right exps -> pp <$> runCodegen (generate exps)
      Left err   -> Left (WhileParsing err)


