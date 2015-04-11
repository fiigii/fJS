module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Control.Applicative ((<*>), (<$>), (*>), (<*), pure, (<$))
import Control.Monad

import Ast

jsparse :: String -> Either ParseError [Ast]
jsparse  = parse prog ""

-- The scanner.
lexer = P.makeTokenParser emptyDef {
  T.commentStart = "/*",
  T.commentEnd   = "*/",
  T.commentLine  = "//",
  T.nestedComments = True,
  T.identStart = letter <|> char '_' <|> char '$',
  T.identLetter     = alphaNum,
  T.reservedNames   = ["function", "var", "true", "false", "Bool", "Number",
                       "Ref", "ref", "Unit", "unit", "Top"],
  T.reservedOpNames = ["=", "?", ":", "->", "=>", "+", "-", "*", "/", "!",
                       "==", "!=", ">", "<", ">=", "<="],
  T.caseSensitive   = True
}

parens = P.parens lexer
braces = P.braces lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
semi = P.semi lexer
str_lit  = P.stringLiteral lexer
integer_lit = P.integer lexer

-- The parser
prog :: Parser [Ast]
prog = (expr) `endBy` semi

--- Asts
term :: Parser Ast
term  = parens expr <|> var <|> literal <|> refCreat <|>
        deRef <|> funcExpr

str :: Parser Ast
str = String <$> str_lit

num :: Parser Ast
num = Number . fromIntegral <$> integer_lit

literal :: Parser Ast
literal = bool <|> str <|> record <|> num <|> unit

unit :: Parser Ast
unit = reserved "unit" >> return Unit

bool :: Parser Ast
bool = (reserved "true" >> (return $ Bool True))
       <|> (reserved "false" >> (return $ Bool False))

record :: Parser Ast
record = (braces . commaSep1) recordTerm >>=
           (\rs -> return $ Record  rs)

recordTerm :: Parser (String, Ast)
recordTerm = do whiteSpace
                label <- identifier
                reservedOp "=>"
                init <- expr
                return $ (label, init)

refCreat :: Parser Ast
refCreat = reserved "ref" >> term >>= \tm -> return $ Ref tm

deRef :: Parser Ast
deRef = reservedOp "!" >> term >>= \tm -> return $ DeRef tm

var :: Parser Ast
var = do whiteSpace
         v <- identifier
         return $ Var v

--- experssions
expr :: Parser Ast
expr = assigExpr

assigExpr :: Parser Ast
assigExpr = do whiteSpace
               e <- parserBinExpr
               (do reservedOp "="
                   rightValue <- assigExpr
                   return $ Assign e rightValue) <|> return e 

makeInfixExpr str constr = Infix parser AssocLeft
  where parser = do whiteSpace
                    reservedOp str
                    return $ constr str 

exprTable =
  [ [makeInfixExpr ">" BinaryExpr,
     makeInfixExpr ">=" BinaryExpr,
     makeInfixExpr "<" BinaryExpr,
     makeInfixExpr "<=" BinaryExpr],
    [makeInfixExpr "==" BinaryExpr,
     makeInfixExpr "!=" BinaryExpr],
    [makeInfixExpr "+" BinaryExpr,
     makeInfixExpr "-" BinaryExpr],
    [makeInfixExpr "*" BinaryExpr,
     makeInfixExpr "/" BinaryExpr] ] 

parserBinExpr :: Parser Ast
parserBinExpr = buildExpressionParser exprTable preFixExpr

preFixExpr :: Parser Ast
preFixExpr =  do whiteSpace
                 e <- term
                 condExpr' e <|> maybeAddSuffix e
  where addSuffix e0 = (do e1 <- parens expr
                           maybeAddSuffix $ Appliction e0 e1) <|>
                       (do char '.'
                           label <- identifier
                           maybeAddSuffix $ Access e0 label)
        maybeAddSuffix e = addSuffix e
                           <|> return e
       
                                
condExpr' :: Ast -> Parser Ast
condExpr' e = do reservedOp "?"
                 t2 <- expr
                 reservedOp ":"
                 t3 <- expr
                 return $ IfExpr e t2 t3

funcExpr :: Parser Ast
funcExpr  = do whiteSpace
               reserved "function"
               v  <- parens $ arg
               body <- expr
               return $ Function v body

arg :: Parser (String, Ty)
arg = do whiteSpace
         formal <- identifier
         reservedOp ":"
         ty <- argTy
         return (formal, ty)

--- Types
argTy :: Parser Ty
argTy = do whiteSpace
           t <- parseType
           (do reservedOp "->"
               tt <- argTy
               return $ TyFun t tt) <|> return t

boolTy :: Parser Ty
boolTy = reserved "Bool" >> return TyBool

numTy :: Parser Ty
numTy = reserved "Number" >> return TyNum

nuitTy = reserved "Unit" >> return TyUnit

recordTy :: Parser Ty
recordTy = (braces . commaSep1) recordTyItem >>=
             (\rs -> return $ TyRecord  rs)

recordTyItem :: Parser (String, Ty)
recordTyItem =  do whiteSpace
                   label <- identifier
                   reservedOp ":"
                   ty <- parseType
                   return (label, ty)

refTy :: Parser Ty
refTy = reserved "Ref" >> parseType >>= \t -> return $ TyRef t

unitTy :: Parser Ty
unitTy = reserved "Unit" >> return TyUnit

topTy :: Parser Ty
topTy = reserved "Top" >> return TyTop

parseType :: Parser Ty
parseType = parens argTy <|> boolTy <|> numTy <|> recordTy <|> refTy <|>
         unitTy <|> topTy

--- statement
