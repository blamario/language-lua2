{-# Language RecordWildCards #-}
module Language.Lua.Grammar where

import Control.Applicative
import Data.Char (chr, isAlphaNum, isDigit, isHexDigit, isLetter, isSpace)
import Data.List.NonEmpty      (NonEmpty(..), toList)
import Data.Monoid ((<>))
import Numeric (readHex)
import Text.Grampa
import Language.Lua.Syntax
import Language.Lua.Parser.Internal (NodeInfo(..))

data LuaGrammar a f = LuaGrammar{
   chunk :: f (Block a),
   block :: f (Block a),
   stat :: f (Statement a),
   retstat :: f (ReturnStatement a),
   label :: f (Ident a),
   funcname :: f (FunctionName a),
   varlist :: f (VariableList1 a),
   var :: f (Variable a),
   namelist :: f (IdentList1 a),
   explist :: f (ExpressionList a),
   explist1 :: f (ExpressionList1 a),
   exp :: f (Expression a),
   prefixexp :: f (PrefixExpression a),
   functioncall :: f (FunctionCall a),
   args :: f (FunctionArgs a),
   functiondef :: f (FunctionBody a),
   funcbody :: f (FunctionBody a),
   parlist :: f (ParamList a),
   tableconstructor :: f (TableConstructor a),
   fieldlist :: f (FieldList a),
   field :: f (Field a),
   fieldsep :: f (),
   binop :: f (Binop a),
   unop :: f (Unop a),
   literalString :: f String,
   numeral :: f (Expression a),
   name :: f (Ident a),
   digits :: f String,
   hexDigits :: f String,
   initialHexDigits :: f String,
   exponent :: f String,
   hexExponent :: f String}

instance Functor1 (LuaGrammar a)

moptional :: (Functor1 g, MonoidNull t, Monoid x) => Parser g t x -> Parser g t x
moptional p = p <|> pure mempty

concatMany :: (Functor1 g, MonoidNull t, Monoid x) => Parser g t x -> Parser g t x
concatMany p = moptional (p <> concatMany p)

skip :: Parser g t x -> Parser g t ()
skip = (() <$)

spaces :: (Functor1 g, TextualMonoid t) => Parser g t ()
spaces = skipCharsWhile isSpace

char :: (Functor1 g, TextualMonoid t) => Char -> Parser g t Char
char = satisfyChar . (==)

count :: (Functor1 g, MonoidNull t) => Int -> Parser g t x -> Parser g t [x]
count n p | n > 0 = (:) <$> p <*> count (n-1) p         
          | otherwise = pure []

sepBy :: (Functor1 g, MonoidNull t) => Parser g t x -> Parser g t sep -> Parser g t [x]
sepBy p sep = toList <$> sepBy1 p sep <|> pure []

sepBy1 :: (Functor1 g, MonoidNull t) => Parser g t x -> Parser g t sep -> Parser g t (NonEmpty x)
sepBy1 p sep = (:|) <$> p <*> many (sep *> p)

node :: MonoidNull t => (NodeInfo -> x) -> Parser (LuaGrammar NodeInfo) t x
node f = pure (f undefined)

keyword :: (TextualMonoid t, Show t) => t -> Parser (LuaGrammar NodeInfo) t t
keyword k = spaces *> string k

toExpList :: ExpressionList1 a -> ExpressionList a
toExpList (ExpressionList1 a l) = ExpressionList a (toList l)

grammar :: GrammarBuilder (LuaGrammar NodeInfo) (LuaGrammar NodeInfo) String
grammar LuaGrammar{..} = LuaGrammar{
   chunk = block,
   block = node Block <*> many stat <*> optional retstat,
   stat = node EmptyStmt <* string ";" <|>
          node Assign <*> varlist <* string "=" <*> explist1 <|>
          node FunCall <*> functioncall <|>
          node Label <*> label <|>
          node Break <* keyword "break" <|>
          node Goto <* keyword "goto" <*> name <|>
          node Do <* keyword "do" <*> block <* keyword "end" <|>
          node While <* keyword "while" <*> exp <* keyword "do" <*> block <* keyword "end" <|>
          node Repeat <* keyword "repeat" <*> block <* keyword "until" <*> exp <|>
          node If <* keyword "if" <*> ((:|) <$> ((,) <$> exp <* keyword "then" <*> block) <*> many ((,) <$ keyword "elseif" <*> exp <* keyword "then" <*> block)) <*> optional (keyword "else" *> block) <* keyword "end" <|>
          node For <* keyword "for" <*> name <* string "=" <*> exp <* string "," <*> exp <*> optional (string "," *> exp) <* keyword "do" <*> block <* keyword "end" <|>
          node ForIn <* keyword "for" <*> namelist <* keyword "in" <*> explist1 <* keyword "do" <*> block <* keyword "end" <|>
          node FunAssign <* keyword "function" <*> funcname <*> funcbody <|>
          node LocalFunAssign <* keyword "local" <* keyword "function" <*> name <*> funcbody <|>
          node LocalAssign <* keyword "local" <*> namelist <*> (string "=" *> (toExpList <$> explist1) 
                                                                <|> node ExpressionList <*> pure []),

   retstat = node ReturnStatement <* keyword "return" <*> explist <* optional (string ";"),
   label = string "::" *> name <* string "::",
   funcname = node FunctionName <*> (node IdentList1 <*> sepBy1 name (string ".")) <*> optional (string ":" *> name),
   varlist = node VariableList1 <*> sepBy1 var (string ","),
   var = node VarIdent <*> name <|> 
         node VarField <*> prefixexp <* string "[" <*> exp <* string "]" <|> 
         node VarFieldName <*> prefixexp <* string "." <*> name,
   
   namelist = node IdentList1 <*> sepBy1 name (string ","),
   explist = node ExpressionList <*> sepBy exp (string ","),
   explist1 = node ExpressionList1 <*> sepBy1 exp (string ","),

   exp = node Nil <* keyword "nil" <|>
         node Bool <*> pure False <* keyword "false" <|>
         node Bool <*> pure True <* keyword "true" <|>
         numeral <|>
         node String <*> literalString <|>
         node Vararg <* string "..." <|>
         node FunDef <*> functiondef <|>
         node PrefixExp <*> prefixexp <|>
         node TableCtor <*> tableconstructor <|>
         flip <$> node Binop <*> exp <*> binop <*> exp <|>
         node Unop <*> unop <*> exp,

   prefixexp = 
      node PrefixVar <*> var <|>
      node PrefixFunCall <*> functioncall <|>
      node Parens <* string "(" <*> exp <* string ")",

   functioncall =  
      node FunctionCall <*> prefixexp <*> args <|>
      node MethodCall <*> prefixexp <* string ":" <*> name <*> args,

   args =  
      node Args <* string "(" <*> explist <* string ")" <|>
      node ArgsTable <*> tableconstructor <|>
      node ArgsString <*> literalString ,

   functiondef = keyword "function" *> funcbody,

   funcbody = node FunctionBody <* string "(" <*> optional parlist <* string ")" <*> block <* keyword "end",

   parlist = 
      node ParamList <*> namelist <*> (True <$ string "," <* string "..." <|> pure False) <|> 
      node ParamListVararg <* string "...",

   tableconstructor = node TableConstructor <* string "{" <*> fieldlist <* string "}",

   fieldlist = node FieldList <*> sepBy field fieldsep,
   field = 
      node FieldExp <* string "[" <*> exp <* string "]" <* string "=" <*> exp <|>
      node FieldIdent <*> name <* string "=" <*> exp <|>
      node Field <*> exp,

   fieldsep = skip (string "," <|> string ";"),

   binop =  
      node Plus       <* string "+"    <|>
      node Minus      <* string "-"    <|> 
      node Mult       <* string "*"    <|>
      node FloatDiv   <* string "/"    <|>
      node FloorDiv   <* string "//"   <|>
      node Exponent   <* string "^"    <|>
      node Modulo     <* string "%"    <|>
      node BitwiseAnd <* string "&"    <|>
      node BitwiseXor <* string "~"    <|>
      node BitwiseOr  <* string "|"    <|>
      node Rshift     <* string ">>"   <|>
      node Lshift     <* string "<<"   <|>
      node Concat     <* string ".."   <|>
      node Lt         <* string "<"    <|>
      node Leq        <* string "<="   <|>
      node Gt         <* string ">"    <|>
      node Geq        <* string ">="   <|>
      node Eq         <* string "=="   <|>
      node Neq        <* string "~="   <|>
      node And        <* keyword "and" <|>
      node Or         <* keyword "or",

   unop = 
      node Negate     <* string "-"    <|>
      node Not        <* keyword "not" <|>
      node Length     <* string "#"    <|>
      node BitwiseNot <* string "~",

   numeral = (node Integer <*> digits <|>
              node Float <*> (digits <> token "." <> digits <> moptional exponent) <|>
              node Float <*> (digits <> exponent) <|>
              node Integer <*> initialHexDigits <|>
              node Float <*> (initialHexDigits <> token "." <> hexDigits <> moptional hexExponent) <|>
              node Float <*> (initialHexDigits <> hexExponent))
             <* notFollowedBy (satisfyChar isAlphaNum),
   digits = some (satisfyChar isDigit),
   hexDigits = some (satisfyChar isHexDigit),
   initialHexDigits = token "0x" <> hexDigits,
   exponent = (token "e" <|> token "E") <> moptional (token "+" <|> token "-") <> digits,
   hexExponent = (token "p" <|> token "P") <> moptional (token "+" <|> token "-") <> digits,
   name = let isStartChar c = isLetter c || c == '_'
              isNameChar c = isStartChar c || isDigit c
          in node Ident <*> ((:) <$> satisfyChar isStartChar <*> many (satisfyChar isNameChar)) 
             <* notFollowedBy (satisfyChar isNameChar),
   literalString = let escapeSequence = 
                          token "\\" 
                          *> ("\\" <$ token "\\" <|>
                              "\a" <$ token "a" <|>
                              "\b" <$ token "b" <|>
                              "\f" <$ token "f" <|>
                              "\n" <$ token "n" <|>
                              "\r" <$ token "r" <|>
                              "\t" <$ token "t" <|>
                              "\v" <$ token "v" <|>
                              "\"" <$ token "\"" <|>
                              "\'" <$ token "\'" <|>
                              "\n" <$ token "\n" <|>
                              ((:[]) . chr) <$> (token "d" *> (read <$> ((:) <$> digit <*> ((:) <$> digit <*> ((:[]) <$> digit <|> pure [])))) <|>
                                       token "x" *> ((fst . head . readHex) <$> count 3 hexDigit)) <|>
                              "" <$ token "z" <* skipCharsWhile isSpace)
                       digit = satisfyChar isDigit
                       hexDigit = satisfyChar isHexDigit
                       literalWith quote = char quote *> concatMany (escapeSequence <|> takeCharsWhile (\c-> c /= '\'' && c /= quote)) <* char quote
                   in 
                      literalWith '"' <|> literalWith '\'' <|>
                      do token "["
                         equalSigns <- takeCharsWhile (== '=')
                         token "["
                         optional (token "\n")
                         many (notFollowedBy (do token "]"
                                                 string equalSigns
                                                 token "]")
                               *> satisfyChar (const True))}
