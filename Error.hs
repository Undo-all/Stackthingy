module Error where

import Text.ParserCombinators.Parsec (ParseError)

data Error = NotFound String
           | WhileParsing ParseError
           deriving (Eq, Show)

