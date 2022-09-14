module Parser
    ( term
    , type_
    , regularParse
    ) where

import           Data.Functor                   ( ($>) )
import           Lexer
import           Syntax
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

definition :: Parser Definition
definition = chainl1 (try tydef <|> fldef) aggr
  where
    tydef = braces $ TypeDef <$> (typeMember <* reservedOp "=") <*> type_
    fldef = braces $ FieldDef <$> (termMember <* reservedOp "=") <*> term
    aggr  = reservedOp "^" $> AggrDef

typeMember :: Parser TypeMember
typeMember = TypeMember <$> identifier

termMember :: Parser TermMember
termMember = TermMember <$> identifier

variable :: Parser Variable
variable = Variable <$> identifier

type_ :: Parser Type
type_ =
    choice
            [ reserved "Top" $> Top
            , reserved "Bot" $> Bot
            , try tydelc
            , TypeProj <$> (variable <* reservedOp ".") <*> typeMember
            , func
            , fldelc
            , mu
            ]
        `chainl1` (reservedOp "^" $> Intersect)

  where
    tydelc = do
        (a, t, u) <- braces $ do
            a <- identifier
            reservedOp ":"
            t <- type_
            reservedOp ".."
            u <- type_
            pure (a, t, u)
        pure $ TypeDelc (TypeMember a) t u
    fldelc = do
        (a, t) <- braces $ do
            a <- identifier
            reservedOp ":"
            t <- type_
            pure (a, t)
        pure $ FieldDelc (TermMember a) t
    func = do
        reservedOp "\\"
        (x, s) <- parens $ do
            x <- variable
            reservedOp ":"
            s <- type_
            pure (x, s)
        ty <- type_
        pure $ Func x s ty
    mu = do
        reservedOp "@"
        (x, t) <- parens $ do
            x <- variable
            reservedOp ":"
            t <- type_
            pure (x, t)
        pure $ Mu x t

value :: Parser Value
value = object <|> lambda
  where
    object = do
        reservedOp "~"
        (x, t) <- parens $ do
            x <- variable
            reservedOp ":"
            t <- type_
            pure (x, t)
        ds <- definition
        pure $ Object x t ds
    lambda = do
        reservedOp "\\"
        (x, t) <- parens $ do
            x <- variable
            reservedOp ":"
            t <- type_
            pure (x, t)
        tm <- term
        pure $ Lambda x t tm

term :: Parser Term
term = app <|> (Val <$> value) <|> bnd
  where
    app = do
        v   <- variable
        opt <-
            optionMaybe
                (Right <$> (reservedOp "." *> termMember) <|> Left <$> variable)
        pure $ case opt of
            Nothing        -> Var v
            Just (Right a) -> Sel v a
            Just (Left  x) -> App v x
    bnd = do
        reserved "let"
        v <- variable
        reservedOp "="
        t <- term
        reserved "in"
        u <- term
        pure $ Let v t u

