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

import Prelude hiding (exp, exponent)

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

instance Functor1 (LuaGrammar a) where
   fmap1 f g = LuaGrammar{
      chunk = f (chunk g),
      block = f (block g),
      stat = f (stat g),
      retstat = f (retstat g),
      label = f (label g),
      funcname = f (funcname g),
      varlist = f (varlist g),
      var = f (var g),
      namelist = f (namelist g),
      explist = f (explist g),
      explist1 = f (explist1 g),
      exp = f (exp g),
      prefixexp = f (prefixexp g),
      functioncall = f (functioncall g),
      args = f (args g),
      functiondef = f (functiondef g),
      funcbody = f (funcbody g),
      parlist = f (parlist g),
      tableconstructor = f (tableconstructor g),
      fieldlist = f (fieldlist g),
      field = f (field g),
      fieldsep = f (fieldsep g),
      binop = f (binop g),
      unop = f (unop g),
      literalString = f (literalString g),
      numeral = f (numeral g),
      name = f (name g),
      digits = f (digits g),
      hexDigits = f (hexDigits g),
      initialHexDigits = f (initialHexDigits g),
      exponent = f (exponent g),
      hexExponent = f (exponent g)}

instance Reassemblable (LuaGrammar a) where
   applyFieldwise f a b = LuaGrammar{
      chunk = chunk (f b{chunk= chunk a}),
      block = block (f b{block= block a}),
      stat = stat (f b{stat= stat a}),
      retstat = retstat (f b{retstat= retstat a}),
      label = label (f b{label= label a}),
      funcname = funcname (f b{funcname= funcname a}),
      varlist = varlist (f b{varlist= varlist a}),
      var = var (f b{var= var a}),
      namelist = namelist (f b{namelist= namelist a}),
      explist = explist (f b{explist= explist a}),
      explist1 = explist1 (f b{explist1= explist1 a}),
      exp = exp (f b{exp= exp a}),
      prefixexp = prefixexp (f b{prefixexp= prefixexp a}),
      functioncall = functioncall (f b{functioncall= functioncall a}),
      args = args (f b{args= args a}),
      functiondef = functiondef (f b{functiondef= functiondef a}),
      funcbody = funcbody (f b{funcbody= funcbody a}),
      parlist = parlist (f b{parlist= parlist a}),
      tableconstructor = tableconstructor (f b{tableconstructor= tableconstructor a}),
      fieldlist = fieldlist (f b{fieldlist= fieldlist a}),
      field = field (f b{field= field a}),
      fieldsep = fieldsep (f b{fieldsep= fieldsep a}),
      binop = binop (f b{binop= binop a}),
      unop = unop (f b{unop= unop a}),
      literalString = literalString (f b{literalString= literalString a}),
      numeral = numeral (f b{numeral= numeral a}),
      name = name (f b{name= name a}),
      digits = digits (f b{digits= digits a}),
      hexDigits = hexDigits (f b{hexDigits= hexDigits a}),
      initialHexDigits = initialHexDigits (f b{initialHexDigits= initialHexDigits a}),
      exponent = exponent (f b{exponent= exponent a}),
      hexExponent = exponent (f b{exponent= exponent a})}
   reassemble f g = LuaGrammar{
      chunk = f chunk (\x-> g{chunk= x}) g,
      block = f block (\x-> g{block= x}) g,
      stat = f stat (\x-> g{stat= x}) g,
      retstat = f retstat (\x-> g{retstat= x}) g,
      label = f label (\x-> g{label= x}) g,
      funcname = f funcname (\x-> g{funcname= x}) g,
      varlist = f varlist (\x-> g{varlist= x}) g,
      var = f var (\x-> g{var= x}) g,
      namelist = f namelist (\x-> g{namelist= x}) g,
      explist = f explist (\x-> g{explist= x}) g,
      explist1 = f explist1 (\x-> g{explist1= x}) g,
      exp = f exp (\x-> g{exp= x}) g,
      prefixexp = f prefixexp (\x-> g{prefixexp= x}) g,
      functioncall = f functioncall (\x-> g{functioncall= x}) g,
      args = f args (\x-> g{args= x}) g,
      functiondef = f functiondef (\x-> g{functiondef= x}) g,
      funcbody = f funcbody (\x-> g{funcbody= x}) g,
      parlist = f parlist (\x-> g{parlist= x}) g,
      tableconstructor = f tableconstructor (\x-> g{tableconstructor= x}) g,
      fieldlist = f fieldlist (\x-> g{fieldlist= x}) g,
      field = f field (\x-> g{field= x}) g,
      fieldsep = f fieldsep (\x-> g{fieldsep= x}) g,
      binop = f binop (\x-> g{binop= x}) g,
      unop = f unop (\x-> g{unop= x}) g,
      literalString = f literalString (\x-> g{literalString= x}) g,
      numeral = f numeral (\x-> g{numeral= x}) g,
      name = f name (\x-> g{name= x}) g,
      digits = f digits (\x-> g{digits= x}) g,
      hexDigits = f hexDigits (\x-> g{hexDigits= x}) g,
      initialHexDigits = f initialHexDigits (\x-> g{initialHexDigits= x}) g,
      exponent = f exponent (\x-> g{exponent= x}) g,
      hexExponent = f exponent (\x-> g{exponent= x}) g}
   

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

upto :: (Functor1 g, MonoidNull t) => Int -> Parser g t x -> Parser g t [x]
upto n p | n > 0 = moptional ((:) <$> p <*> upto (n-1) p)
         | otherwise = pure []

sepBy :: (Functor1 g, MonoidNull t) => Parser g t x -> Parser g t sep -> Parser g t [x]
sepBy p sep = toList <$> sepBy1 p sep <|> pure []

sepBy1 :: (Functor1 g, MonoidNull t) => Parser g t x -> Parser g t sep -> Parser g t (NonEmpty x)
sepBy1 p sep = (:|) <$> p <*> many (sep *> p)

node :: MonoidNull t => (NodeInfo -> x) -> Parser (LuaGrammar NodeInfo) t x
node f = pure (f mempty)

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
                              ((:[]) . chr) <$> (token "d" *> (read <$> upto 3 digit) <|>
                                                 token "x" *> ((fst . head . readHex) <$> count 2 hexDigit)) <|>
                              "" <$ token "z" <* skipCharsWhile isSpace)
                       digit = satisfyChar isDigit
                       hexDigit = satisfyChar isHexDigit
                       literalWith quote = char quote 
                                           *> concatMany (escapeSequence <|>
                                                          takeCharsWhile1 (\c-> c /= '\'' && c /= quote)) 
                                           <* char quote
                   in literalWith '"' <|> literalWith '\'' <|>
                      do token "["
                         equalSigns <- takeCharsWhile (== '=')
                         token "["
                         optional (token "\n")
                         let terminator = do token "]"
                                             string equalSigns
                                             token "]"
                         s <- many (notFollowedBy terminator
                                    *> satisfyChar (const True))
                         terminator
                         return s
   }
