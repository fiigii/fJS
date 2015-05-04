module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Control.Applicative ((<*>), (<$>), (*>), (<*), pure, (<$))
import Control.Monad

import Data.List (lookup, filter)

import Ast

jsparse :: String -> Either ParseError Ast
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
                       "Unit", "unit", "let", "if", "then", "else", "in", "head",
                       "tail"],
  T.reservedOpNames = ["=", "?", ":", "->", "=>", "+", "-", "*", "/", "!",
                       "==", "!=", ">", "<", ">=", "<="],
  T.caseSensitive   = True
}

parens = P.parens lexer
braces = P.braces lexer
brackets = P.brackets lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
semi = P.semi lexer
strLit  = P.stringLiteral lexer
integerLit = P.integer lexer

-- The parser
prog :: Parser Ast
prog = statement

--- statements
statement :: Parser Ast
statement = topLevel

topLevelDefs :: Parser [(String, Ast)]
topLevelDefs = many1 $ do whiteSpace
                          reserved "var"
                          v <- identifier
                          reservedOp "="
                          def <- expr
                          return (v, def)

topLevel :: Parser Ast
topLevel = do whiteSpace
              defs <- topLevelDefs
              let decls = filter (\(v, _) -> v /= "main") defs
              return $ case lookup "main" defs of
                        Just mainBody -> Letrec decls
                                                (Appliction mainBody
                                                            (String ""))
                        _ -> Letrec decls Unit

--- Terms
term :: Parser Ast
term  = parens expr <|> var <|> literal <|> funcExpr <|> ifExpr <|> letExpr
        <|> block <|> list <|> headExpr <|> tailExpr

str :: Parser Ast
str = String <$> strLit

num :: Parser Ast
num = Number . fromIntegral <$> integerLit

literal :: Parser Ast
literal = bool <|> str <|> num <|> unit

unit :: Parser Ast
unit = reserved "unit" >> return Unit

bool :: Parser Ast
bool = (reserved "true" >> return (Bool True))
       <|> (reserved "false" >> return (Bool False))

list :: Parser Ast
list = List <$> (brackets $ commaSep expr)

headExpr :: Parser Ast
headExpr = do whiteSpace
              reserved "head"
              l <- expr
              return $ Car l

tailExpr :: Parser Ast
tailExpr = do whiteSpace
              reserved "tail"
              l <- expr
              return $ Cdr l

var :: Parser Ast
var = do whiteSpace
         v <- identifier
         return $ Var v

--- experssions
expr :: Parser Ast
expr = parserBinExpr

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
  where addSuffix e0 = do es <- parens $ commaSep1 expr
                          maybeAddSuffix $ foldl (\arg acc -> Appliction arg acc)
                                                 (Appliction e0 $ head es)
                                                 (tail es)
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
               vs  <- parens $ commaSep1 identifier
               body <- expr
               return $ foldr (\v acc -> Function v acc)
                              (Function (last vs) body)
                              (init vs)

ifExpr :: Parser Ast
ifExpr = do whiteSpace
            reserved "if"
            t1 <- parens expr
            t2 <- expr
            reserved "else"
            t3 <- expr
            return $ IfExpr t1 t2 t3
            
letExpr :: Parser Ast
letExpr = do whiteSpace
             reserved "let"
             inits <- initItem `endBy` semi
             reserved "in"
             body <- expr
             return $ LetExpr inits body

block :: Parser Ast
block = braces $ do inits <- many localItem
                    body <- expr
                    return $ Letrec inits body

localItem :: Parser (String, Ast)
localItem = do whiteSpace
               reserved "var"
               var <- identifier
               reservedOp "="
               init <- expr
               return (var, init)

initItem :: Parser (String, Ast)
initItem = do whiteSpace
              var <- identifier
              reservedOp "="
              init <- expr
              return (var, init)
--- Types
              {-
parseType = funTy

funTy :: Parser Ty
funTy = do whiteSpace
           t <- tyTerm
           (do reservedOp "->"
               tt <- funTy
               return $ TyFun t tt) <|> return t

boolTy :: Parser Ty
boolTy = reserved "Bool" >> return TyBool

numTy :: Parser Ty
numTy = reserved "Number" >> return TyNum

nuitTy = reserved "Unit" >> return TyUnit

recordTy :: Parser Ty
recordTy =  TyRecord <$> (braces . commaSep1) recordTyItem

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

tyTerm :: Parser Ty
tyTerm = parens funTy <|> boolTy <|> numTy <|> recordTy <|> refTy <|>
         unitTy <|> topTy
-}

