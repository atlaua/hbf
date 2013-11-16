module HBF.Parser
( parseBF
) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import HBF.Types

parseBF :: String -> Either ParseError Cmds
parseBF = parse bfCmds ""

bfCmds :: Parser Cmds
bfCmds = garbage *> many (bfCmd <* garbage)

bfCmd :: Parser Cmd
bfCmd = '<' --> MoveLeft
    <|> '>' --> MoveRight
    <|> '+' --> IncVal
    <|> '-' --> DecVal
    <|> '.' --> WriteVal
    <|> ',' --> ReadVal
    <|> loop

loop :: Parser Cmd
loop = char '[' *> fmap Loop bfCmds <* char ']'

garbage :: Parser ()
garbage = skipMany (noneOf "<>+-.,[]")

(-->) :: Char -> Cmd -> Parser Cmd
chr --> cmd = cmd <$ char chr
