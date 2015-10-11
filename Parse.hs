module Parse (parseFile) where

import Ast
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parseFile :: FilePath -> String -> Either ParseError [Expr]
parseFile f xs = parse parseExprs f xs

parseExprs :: Parser [Expr]
parseExprs = spaces *> many (parseExpr <* spaces)

parseExpr :: Parser Expr
parseExpr = Call <$ char '.'
        <|> Add <$ char '+'
        <|> Sub <$ char '-'
        <|> Mul <$ char '*'
        <|> Div <$ char '/'
        <|> Outchr <$ char '~'
        <|> try (parseNamedBlock)
        <|> parseAnonBlock
        <|> Lit <$> parseLit
        <?> "expression"

parseNamedBlock :: Parser Expr
parseNamedBlock = do
    name <- parseName
    string ":("
    contents <- parseExprs
    char ')'
    return $ NamedBlock name contents

parseName :: Parser String
parseName = do
    let legal = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
    first <- oneOf legal
    rest  <- many (oneOf (legal ++ ['0'..'9']))
    return $ first : rest

parseAnonBlock :: Parser Expr
parseAnonBlock = do
    char '('
    contents <- parseExprs
    char ')'
    return $ AnonBlock contents

parseLit :: Parser Lit
parseLit = Int <$> int
       <|> Ident <$> parseName
       <?> "literal"

