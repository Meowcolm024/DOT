module Lexer
    ( identifier
    , braces
    , parens
    , reserved
    , reservedOp
    ) where

import           Data.Functor.Identity          ( Identity )
import           Text.Parsec.Language           ( haskellStyle )
import           Text.Parsec.String             ( Parser )
import qualified Text.Parsec.Token             as P

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellStyle
    { P.reservedNames   = ["let", "in", "Top", "Bot", "object"]
    , P.reservedOpNames = ["=", "^", "\\", ":", "..", "µ"]
    }

identifier :: Parser String
identifier = P.identifier lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer
