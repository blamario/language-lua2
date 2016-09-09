{-# Language RecordWildCards #-}
module Language.Lua.Grammar where

import Control.Applicative
import Control.Monad (guard)
import Data.Char (chr, isAlphaNum, isDigit, isHexDigit, isLetter, isSpace)
import Data.Functor.Classes (Show1, showsPrec1)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Monoid ((<>))
import Numeric (readHex)
import Text.Grampa
import Language.Lua.Syntax
import Language.Lua.Parser.Internal (NodeInfo(..))

import Debug.Trace (trace)

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
   exp, andexp, compexp, bitorexp, bitxorexp, bitandexp, shiftexp,
   concatexp, sumexp, productexp, factorexp, expexp, primaryexp :: f (Expression a),
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
   longBracket :: f String,
   comment :: f String,
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
      andexp = f (andexp g),
      compexp = f (compexp g),
      bitorexp = f (bitorexp g),
      bitxorexp = f (bitxorexp g),
      bitandexp = f (bitandexp g),
      shiftexp = f (shiftexp g),
      concatexp = f (concatexp g),
      sumexp = f (sumexp g),
      productexp = f (productexp g),
      factorexp = f (factorexp g),
      expexp = f (expexp g),
      primaryexp = f (primaryexp g),
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
      longBracket = f (longBracket g),
      comment = f (comment g),
      numeral = f (numeral g),
      name = f (name g),
      digits = f (digits g),
      hexDigits = f (hexDigits g),
      initialHexDigits = f (initialHexDigits g),
      exponent = f (exponent g),
      hexExponent = f (exponent g)}

instance Apply1 (LuaGrammar a) where
   ap1 f g = LuaGrammar{
      chunk = chunk f `apply1` chunk g,
      block = block f `apply1` block g,
      stat = stat f `apply1` stat g,
      retstat = retstat f `apply1` retstat g,
      label = label f `apply1` label g,
      funcname = funcname f `apply1` funcname g,
      varlist = varlist f `apply1` varlist g,
      var = var f `apply1` var g,
      namelist = namelist f `apply1` namelist g,
      explist = explist f `apply1` explist g,
      explist1 = explist1 f `apply1` explist1 g,
      exp = exp f `apply1` exp g,
      andexp = andexp f `apply1` andexp g,
      compexp = compexp f `apply1` compexp g,
      bitorexp = bitorexp f `apply1` bitorexp g,
      bitxorexp = bitxorexp f `apply1` bitxorexp g,
      bitandexp = bitandexp f `apply1` bitandexp g,
      shiftexp = shiftexp f `apply1` shiftexp g,
      concatexp = concatexp f `apply1` concatexp g,
      sumexp = sumexp f `apply1` sumexp g,
      productexp = productexp f `apply1` productexp g,
      factorexp = factorexp f `apply1` factorexp g,
      expexp = expexp f `apply1` expexp g,
      primaryexp = primaryexp f `apply1` primaryexp g,
      prefixexp = prefixexp f `apply1` prefixexp g,
      functioncall = functioncall f `apply1` functioncall g,
      args = args f `apply1` args g,
      functiondef = functiondef f `apply1` functiondef g,
      funcbody = funcbody f `apply1` funcbody g,
      parlist = parlist f `apply1` parlist g,
      tableconstructor = tableconstructor f `apply1` tableconstructor g,
      fieldlist = fieldlist f `apply1` fieldlist g,
      field = field f `apply1` field g,
      fieldsep = fieldsep f `apply1` fieldsep g,
      binop = binop f `apply1` binop g,
      unop = unop f `apply1` unop g,
      literalString = literalString f `apply1` literalString g,
      longBracket = longBracket f `apply1` longBracket g,
      comment = comment f `apply1` comment g,
      numeral = numeral f `apply1` numeral g,
      name = name f `apply1` name g,
      digits = digits f `apply1` digits g,
      hexDigits = hexDigits f `apply1` hexDigits g,
      initialHexDigits = initialHexDigits f `apply1` initialHexDigits g,
      exponent = exponent f `apply1` exponent g,
      hexExponent = exponent f `apply1` exponent g}

instance Alternative1 (LuaGrammar a) where
   empty1 = LuaGrammar{
      chunk = empty,
      block = empty,
      stat = empty,
      retstat = empty,
      label = empty,
      funcname = empty,
      varlist = empty,
      var = empty,
      namelist = empty,
      explist = empty,
      explist1 = empty,
      exp = empty,
      andexp = empty,
      compexp = empty,
      bitorexp = empty,
      bitxorexp = empty,
      bitandexp = empty,
      shiftexp = empty,
      concatexp = empty,
      sumexp = empty,
      productexp = empty,
      factorexp = empty,
      expexp = empty,
      primaryexp = empty,
      prefixexp = empty,
      functioncall = empty,
      args = empty,
      functiondef = empty,
      funcbody = empty,
      parlist = empty,
      tableconstructor = empty,
      fieldlist = empty,
      field = empty,
      fieldsep = empty,
      binop = empty,
      unop = empty,
      literalString = empty,
      longBracket = empty,
      comment = empty,
      numeral = empty,
      name = empty,
      digits = empty,
      hexDigits = empty,
      initialHexDigits = empty,
      exponent = empty,
      hexExponent = empty}
   choose1 f g = LuaGrammar{
      chunk = chunk f <|> chunk g,
      block = block f <|> block g,
      stat = stat f <|> stat g,
      retstat = retstat f <|> retstat g,
      label = label f <|> label g,
      funcname = funcname f <|> funcname g,
      varlist = varlist f <|> varlist g,
      var = var f <|> var g,
      namelist = namelist f <|> namelist g,
      explist = explist f <|> explist g,
      explist1 = explist1 f <|> explist1 g,
      exp = exp f <|> exp g,
      andexp = andexp f <|> andexp g,
      compexp = compexp f <|> compexp g,
      bitorexp = bitorexp f <|> bitorexp g,
      bitxorexp = bitxorexp f <|> bitxorexp g,
      bitandexp = bitandexp f <|> bitandexp g,
      shiftexp = shiftexp f <|> shiftexp g,
      concatexp = concatexp f <|> concatexp g,
      sumexp = sumexp f <|> sumexp g,
      productexp = productexp f <|> productexp g,
      factorexp = factorexp f <|> factorexp g,
      expexp = expexp f <|> expexp g,
      primaryexp = primaryexp f <|> primaryexp g,
      prefixexp = prefixexp f <|> prefixexp g,
      functioncall = functioncall f <|> functioncall g,
      args = args f <|> args g,
      functiondef = functiondef f <|> functiondef g,
      funcbody = funcbody f <|> funcbody g,
      parlist = parlist f <|> parlist g,
      tableconstructor = tableconstructor f <|> tableconstructor g,
      fieldlist = fieldlist f <|> fieldlist g,
      field = field f <|> field g,
      fieldsep = fieldsep f <|> fieldsep g,
      binop = binop f <|> binop g,
      unop = unop f <|> unop g,
      literalString = literalString f <|> literalString g,
      longBracket = longBracket f <|> longBracket g,
      comment = comment f <|> comment g,
      numeral = numeral f <|> numeral g,
      name = name f <|> name g,
      digits = digits f <|> digits g,
      hexDigits = hexDigits f <|> hexDigits g,
      initialHexDigits = initialHexDigits f <|> initialHexDigits g,
      exponent = exponent f <|> exponent g,
      hexExponent = exponent f <|> exponent g}

instance Foldable1 (LuaGrammar a) where
   foldMap1 f g =
      f (chunk g) <>
      f (block g) <>
      f (stat g) <>
      f (retstat g) <>
      f (label g) <>
      f (funcname g) <>
      f (varlist g) <>
      f (var g) <>
      f (namelist g) <>
      f (explist g) <>
      f (explist1 g) <>
      f (exp g) <>
      f (andexp g) <>
      f (compexp g) <>
      f (bitorexp g) <>
      f (bitxorexp g) <>
      f (bitandexp g) <>
      f (shiftexp g) <>
      f (concatexp g) <>
      f (sumexp g) <>
      f (productexp g) <>
      f (factorexp g) <>
      f (expexp g) <>
      f (primaryexp g) <>
      f (prefixexp g) <>
      f (functioncall g) <>
      f (args g) <>
      f (functiondef g) <>
      f (funcbody g) <>
      f (parlist g) <>
      f (tableconstructor g) <>
      f (fieldlist g) <>
      f (field g) <>
      f (fieldsep g) <>
      f (binop g) <>
      f (unop g) <>
      f (literalString g) <>
      f (longBracket g) <>
      f (comment g) <>
      f (numeral g) <>
      f (name g) <>
      f (digits g) <>
      f (hexDigits g) <>
      f (initialHexDigits g) <>
      f (exponent g) <>
      f (hexExponent g)

instance Traversable1 (LuaGrammar a) where
   traverse1 f g =
      LuaGrammar <$>
      f (chunk g) <*>
      f (block g) <*>
      f (stat g) <*>
      f (retstat g) <*>
      f (label g) <*>
      f (funcname g) <*>
      f (varlist g) <*>
      f (var g) <*>
      f (namelist g) <*>
      f (explist g) <*>
      f (explist1 g) <*>
      f (exp g) <*>
      f (andexp g) <*>
      f (compexp g) <*>
      f (bitorexp g) <*>
      f (bitxorexp g) <*>
      f (bitandexp g) <*>
      f (shiftexp g) <*>
      f (concatexp g) <*>
      f (sumexp g) <*>
      f (productexp g) <*>
      f (factorexp g) <*>
      f (expexp g) <*>
      f (primaryexp g) <*>
      f (prefixexp g) <*>
      f (functioncall g) <*>
      f (args g) <*>
      f (functiondef g) <*>
      f (funcbody g) <*>
      f (parlist g) <*>
      f (tableconstructor g) <*>
      f (fieldlist g) <*>
      f (field g) <*>
      f (fieldsep g) <*>
      f (binop g) <*>
      f (unop g) <*>
      f (literalString g) <*>
      f (longBracket g) <*>
      f (comment g) <*>
      f (numeral g) <*>
      f (name g) <*>
      f (digits g) <*>
      f (hexDigits g) <*>
      f (initialHexDigits g) <*>
      f (exponent g) <*>
      f (hexExponent g)

instance Reassemblable (LuaGrammar a) where
   reassemble f g = LuaGrammar{
      chunk = f chunk g,
      block = f block g,
      stat = f stat g,
      retstat = f retstat g,
      label = f label g,
      funcname = f funcname g,
      varlist = f varlist g,
      var = f var g,
      namelist = f namelist g,
      explist = f explist g,
      explist1 = f explist1 g,
      exp = f exp g,
      andexp = f andexp g,
      compexp = f compexp g,
      bitorexp = f bitorexp g,
      bitxorexp = f bitxorexp g,
      bitandexp = f bitandexp g,
      shiftexp = f shiftexp g,
      concatexp = f concatexp g,
      sumexp = f sumexp g,
      productexp = f productexp g,
      factorexp = f factorexp g,
      expexp = f expexp g,
      primaryexp = f primaryexp g,
      prefixexp = f prefixexp g,
      functioncall = f functioncall g,
      args = f args g,
      functiondef = f functiondef g,
      funcbody = f funcbody g,
      parlist = f parlist g,
      tableconstructor = f tableconstructor g,
      fieldlist = f fieldlist g,
      field = f field g,
      fieldsep = f fieldsep g,
      binop = f binop g,
      unop = f unop g,
      literalString = f literalString g,
      longBracket = f longBracket g,
      comment= f comment g,
      numeral = f numeral g,
      name = f name g,
      digits = f digits g,
      hexDigits = f hexDigits g,
      initialHexDigits = f initialHexDigits g,
      exponent = f exponent g,
      hexExponent = f exponent g}

instance (Show1 f, Show a) => Show (LuaGrammar a f) where
   showsPrec prec g rest = "LuaGrammar{" ++
      "  chunk = " ++ showsPrec1 prec (chunk g) "\n" ++
      "  block = " ++ showsPrec1 prec (block g) "\n" ++
      "  stat = " ++ showsPrec1 prec (stat g) "\n" ++
      "  retstat = " ++ showsPrec1 prec (retstat g) "\n" ++
      "  label = " ++ showsPrec1 prec (label g) "\n" ++
      "  funcname = " ++ showsPrec1 prec (funcname g) "\n" ++
      "  varlist = " ++ showsPrec1 prec (varlist g) "\n" ++
      "  var = " ++ showsPrec1 prec (var g) "\n" ++
      "  namelist = " ++ showsPrec1 prec (namelist g) "\n" ++
      "  explist = " ++ showsPrec1 prec (explist g) "\n" ++
      "  explist1 = " ++ showsPrec1 prec (explist1 g) "\n" ++
      "  exp = " ++ showsPrec1 prec (exp g) "\n" ++
      "  andexp = " ++ showsPrec1 prec (andexp g) "\n" ++
      "  compexp = " ++ showsPrec1 prec (compexp g) "\n" ++
      "  bitorexp = " ++ showsPrec1 prec (bitorexp g) "\n" ++
      "  bitxorexp = " ++ showsPrec1 prec (bitxorexp g) "\n" ++
      "  bitandexp = " ++ showsPrec1 prec (bitandexp g) "\n" ++
      "  shiftexp = " ++ showsPrec1 prec (shiftexp g) "\n" ++
      "  concatexp = " ++ showsPrec1 prec (concatexp g) "\n" ++
      "  sumexp = " ++ showsPrec1 prec (sumexp g) "\n" ++
      "  productexp = " ++ showsPrec1 prec (productexp g) "\n" ++
      "  factorexp = " ++ showsPrec1 prec (factorexp g) "\n" ++
      "  expexp = " ++ showsPrec1 prec (expexp g) "\n" ++
      "  primaryexp = " ++ showsPrec1 prec (primaryexp g) "\n" ++
      "  prefixexp = " ++ showsPrec1 prec (prefixexp g) "\n" ++
      "  functioncall = " ++ showsPrec1 prec (functioncall g) "\n" ++
      "  args = " ++ showsPrec1 prec (args g) "\n" ++
      "  functiondef = " ++ showsPrec1 prec (functiondef g) "\n" ++
      "  funcbody = " ++ showsPrec1 prec (funcbody g) "\n" ++
      "  parlist = " ++ showsPrec1 prec (parlist g) "\n" ++
      "  tableconstructor = " ++ showsPrec1 prec (tableconstructor g) "\n" ++
      "  fieldlist = " ++ showsPrec1 prec (fieldlist g) "\n" ++
      "  field = " ++ showsPrec1 prec (field g) "\n" ++
      "  fieldsep = " ++ showsPrec1 prec (fieldsep g) "\n" ++
      "  binop = " ++ showsPrec1 prec (binop g) "\n" ++
      "  unop = " ++ showsPrec1 prec (unop g) "\n" ++
      "  literalString = " ++ showsPrec1 prec (literalString g) "\n" ++
      "  longBracket = " ++ showsPrec1 prec (longBracket g) "\n" ++
      "  comment = " ++ showsPrec1 prec (comment g) "\n" ++
      "  numeral = " ++ showsPrec1 prec (numeral g) "\n" ++
      "  name = " ++ showsPrec1 prec (name g) "\n" ++
      "  digits = " ++ showsPrec1 prec (digits g) "\n" ++
      "  hexDigits = " ++ showsPrec1 prec (hexDigits g) "\n" ++
      "  initialHexDigits = " ++ showsPrec1 prec (initialHexDigits g) "\n" ++
      "  exponent = " ++ showsPrec1 prec (exponent g) "\n" ++
      "  hexExponent = " ++ showsPrec1 prec (hexExponent g) ("}" ++ rest)

moptional :: (MonoidNull t, Monoid x) => Parser g t x -> Parser g t x
moptional p = p <|> pure mempty

concatMany :: (Functor1 g, MonoidNull t, Monoid x) => Parser g t x -> Parser g t x
concatMany p = moptional (p <> concatMany p)

skip :: Parser g t x -> Parser g t ()
skip = (() <$)

skipMany :: (Functor1 g, MonoidNull t) => Parser g t x -> Parser g t ()
skipMany p = moptional (p *> skipMany p)

spaces :: (Functor1 g, TextualMonoid t) => Parser g t ()
spaces = skipCharsWhile isSpace

ignorable :: Parser (LuaGrammar NodeInfo) String ()
ignorable = spaces *> skipMany (comment luaGrammar *> spaces)

char :: (Functor1 g, TextualMonoid t) => Char -> Parser g t Char
char = satisfyChar . (==)

count :: (Functor1 g, MonoidNull t) => Int -> Parser g t x -> Parser g t [x]
count n p | n > 0 = (:) <$> p <*> count (n-1) p         
          | otherwise = pure []

upto :: (Functor1 g, MonoidNull t) => Int -> Parser g t x -> Parser g t [x]
upto n p | n > 0 = moptional ((:) <$> p <*> upto (n-1) p)
         | otherwise = pure []

sepBy :: Monoid t => Parser g t x -> Parser g t sep -> Parser g t [x]
sepBy p sep = toList <$> sepBy1 p sep <|> pure []

sepBy1 :: Monoid t => Parser g t x -> Parser g t sep -> Parser g t (NonEmpty x)
sepBy1 p sep = (:|) <$> p <*> many (sep *> p)

node :: MonoidNull t => (NodeInfo -> x) -> Parser (LuaGrammar NodeInfo) t x
node f = pure (f mempty)

keyword :: String -> Parser (LuaGrammar NodeInfo) String String
keyword k = ignorable *> string k <* notFollowedBy (satisfyChar isAlphaNum)

symbol :: String -> Parser (LuaGrammar NodeInfo) String String
symbol s = ignorable *> string s

toExpList :: ExpressionList1 a -> ExpressionList a
toExpList (ExpressionList1 a l) = ExpressionList a (toList l)

-- Section 3.1
reservedKeywords  :: [String]
reservedKeywords = ["and", "break", "do", "else", "elseif", "end",
                    "false", "for", "function", "goto", "if", "in",
                    "local", "nil", "not", "or", "repeat", "return",
                    "then", "true", "until", "while"]

luaGrammar :: Grammar (LuaGrammar NodeInfo) String
luaGrammar = fixGrammar grammar

grammar :: GrammarBuilder (LuaGrammar NodeInfo) (LuaGrammar NodeInfo) String
grammar LuaGrammar{..} = LuaGrammar{
   chunk = block <* ignorable <* endOfInput,
   block = node Block <*> many stat <*> optional retstat,
   stat = node EmptyStmt <* symbol ";" <|>
          node Assign <*> varlist <* symbol "=" <*> explist1 <|>
          node FunCall <*> functioncall <|>
          node Label <*> label <|>
          node Break <* keyword "break" <|>
          node Goto <* keyword "goto" <*> name <|>
          node Do <* keyword "do" <*> block <* keyword "end" <|>
          node While <* keyword "while" <*> exp <* keyword "do" <*> block <* keyword "end" <|>
          node Repeat <* keyword "repeat" <*> block <* keyword "until" <*> exp <|>
          node If <* keyword "if" <*> ((:|) <$> ((,) <$> exp <* keyword "then" <*> block) 
                                       <*> many ((,) <$ keyword "elseif" <*> exp <* keyword "then" <*> block)) 
                  <*> optional (keyword "else" *> block) <* keyword "end" <|>
          node For <* keyword "for" <*> name <* symbol "="
                   <*> exp <* symbol "," <*> exp <*> optional (symbol "," *> exp)
                   <* keyword "do" <*> block <* keyword "end" <|>
          node ForIn <* keyword "for" <*> namelist <* keyword "in" <*> explist1 
                     <* keyword "do" <*> block <* keyword "end" <|>
          node FunAssign <* keyword "function" <*> funcname <*> funcbody <|>
          node LocalFunAssign <* keyword "local" <* keyword "function" <*> name <*> funcbody <|>
          node LocalAssign <* keyword "local" <*> namelist <*> (symbol "=" *> (toExpList <$> explist1)
                                                                <|> node ExpressionList <*> pure []),

   retstat = node ReturnStatement <* keyword "return" <*> explist <* optional (symbol ";"),
   label = symbol "::" *> name <* symbol "::",
   funcname = node FunctionName <*> (node IdentList1 <*> sepBy1 name (symbol ".")) <*> optional (symbol ":" *> name),
   varlist = node VariableList1 <*> sepBy1 var (symbol ","),
   var = node VarIdent <*> name <|>
         node VarField <*> prefixexp <* symbol "[" <*> exp <* symbol "]" <|> 
         node VarFieldName <*> prefixexp <* symbol "." <*> name,
   
   namelist = node IdentList1 <*> sepBy1 name (symbol ","),
   explist = node ExpressionList <*> sepBy exp (symbol ","),
   explist1 = node ExpressionList1 <*> sepBy1 exp (symbol ","),

   exp = andexp <|> 
         flip <$> node Binop <*> exp <*> (node Or <* keyword "or") <*> andexp,
   andexp = compexp <|> 
            flip <$> node Binop <*> andexp <*> (node And <* keyword "and") <*> compexp,
   compexp = bitorexp <|> 
             flip <$> node Binop <*> compexp <*> (node Lt <* symbol "<") <*> bitorexp <|>
             flip <$> node Binop <*> compexp <*> (node Gt <* symbol ">") <*> bitorexp <|>
             flip <$> node Binop <*> compexp <*> (node Leq <* symbol "<=") <*> bitorexp <|>
             flip <$> node Binop <*> compexp <*> (node Geq <* symbol ">=") <*> bitorexp <|>
             flip <$> node Binop <*> compexp <*> (node Neq <* symbol "~=") <*> bitorexp <|>
             flip <$> node Binop <*> compexp <*> (node Eq <* symbol "==") <*> bitorexp,
   bitorexp = bitxorexp <|>
              flip <$> node Binop <*> bitorexp <*> (node BitwiseOr <* symbol "|") <*> bitxorexp,
   bitxorexp = bitandexp <|>
               flip <$> node Binop <*> bitxorexp <*> (node BitwiseXor <* symbol "~") <*> bitandexp,
   bitandexp = shiftexp <|>
               flip <$> node Binop <*> bitandexp <*> (node BitwiseAnd <* symbol "&") <*> shiftexp,
   shiftexp = concatexp <|>
              flip <$> node Binop <*> shiftexp <*> (node Lshift <* symbol "<<") <*> concatexp <|>
              flip <$> node Binop <*> shiftexp <*> (node Rshift <* symbol ">>") <*> concatexp,
   concatexp = sumexp <|>
               flip <$> node Binop <*> concatexp <*> (node Concat <* symbol "..") <*> sumexp,
   sumexp = productexp <|>
            flip <$> node Binop <*> sumexp <*> (node Plus <* symbol "+") <*> productexp <|>
            flip <$> node Binop <*> sumexp <*> (node Minus <* symbol "-") <*> productexp,
   productexp = factorexp <|>
                flip <$> node Binop <*> productexp <*> (node Mult <* symbol "*") <*> factorexp <|>
                flip <$> node Binop <*> productexp <*> (node FloatDiv <* symbol "/") <*> factorexp <|>
                flip <$> node Binop <*> productexp <*> (node FloorDiv <* symbol "//") <*> factorexp <|>
                flip <$> node Binop <*> productexp <*> (node Modulo <* symbol "%") <*> factorexp,
   factorexp = expexp <|>
               node Unop <*> unop <*> expexp,
   expexp = primaryexp <|>
            flip <$> node Binop <*> primaryexp <*> (node Exponent <* symbol "^") <*> expexp,
   primaryexp =
      node Nil <* keyword "nil" <|>
      node Bool <*> pure False <* keyword "false" <|>
      node Bool <*> pure True <* keyword "true" <|>
      numeral <|>
      node String <*> literalString <|>
      node Vararg <* symbol "..." <|>
      node FunDef <*> functiondef <|>
      node PrefixExp <*> prefixexp <|>
      node TableCtor <*> tableconstructor,

   prefixexp =
      node PrefixVar <*> var <|>
      node PrefixFunCall <*> functioncall <|>
      node Parens <* symbol "(" <*> exp <* symbol ")",

   functioncall =
      node FunctionCall <*> prefixexp <*> args <|>
      node MethodCall <*> prefixexp <* symbol ":" <*> name <*> args,

   args =
      node Args <* symbol "(" <*> explist <* symbol ")" <|>
      node ArgsTable <*> tableconstructor <|>
      node ArgsString <*> literalString ,

   functiondef = keyword "function" *> funcbody,

   funcbody = node FunctionBody <* symbol "(" <*> optional parlist <* symbol ")" <*> block <* keyword "end",

   parlist =
      node ParamList <*> namelist <*> (True <$ symbol "," <* symbol "..." <|> pure False) <|> 
      node ParamListVararg <* symbol "...",

   tableconstructor = node TableConstructor <* symbol "{" <*> fieldlist <* symbol "}",

   fieldlist = node FieldList <*> sepBy field fieldsep,
   field =
      node FieldExp <* symbol "[" <*> exp <* symbol "]" <* symbol "=" <*> exp <|>
      node FieldIdent <*> name <* symbol "=" <*> exp <|>
      node Field <*> exp,

   fieldsep = skip (symbol "," <|> symbol ";"),

   binop =
      node Plus       <* symbol "+"    <|>
      node Minus      <* symbol "-"    <|> 
      node Mult       <* symbol "*"    <|>
      node FloatDiv   <* symbol "/"    <|>
      node FloorDiv   <* symbol "//"   <|>
      node Exponent   <* symbol "^"    <|>
      node Modulo     <* symbol "%"    <|>
      node BitwiseAnd <* symbol "&"    <|>
      node BitwiseXor <* symbol "~"    <|>
      node BitwiseOr  <* symbol "|"    <|>
      node Rshift     <* symbol ">>"   <|>
      node Lshift     <* symbol "<<"   <|>
      node Concat     <* symbol ".."   <|>
      node Lt         <* symbol "<"    <|>
      node Leq        <* symbol "<="   <|>
      node Gt         <* symbol ">"    <|>
      node Geq        <* symbol ">="   <|>
      node Eq         <* symbol "=="   <|>
      node Neq        <* symbol "~="   <|>
      node And        <* keyword "and" <|>
      node Or         <* keyword "or",

   unop =
      node Negate     <* symbol "-"    <|>
      node Not        <* keyword "not" <|>
      node Length     <* symbol "#"    <|>
      node BitwiseNot <* symbol "~",

   numeral = ignorable *> 
             (node Integer <*> digits <|>
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
   name = do ignorable
             let isStartChar c = isLetter c || c == '_'
                 isNameChar c = isStartChar c || isDigit c
             identifier <- ((:) <$> satisfyChar isStartChar <*> many (satisfyChar isNameChar))
                           <* notFollowedBy (satisfyChar isNameChar)
             guard (notElem identifier reservedKeywords)
             node Ident <*> pure identifier,
   literalString = ignorable *>
                   let escapeSequence = 
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
                                                          takeCharsWhile1 (\c-> c /= '\\' && c /= quote)) 
                                           <* char quote
                   in literalWith '"' <|> 
                      literalWith '\'' <|> 
                      string "!" *> longBracket,
   longBracket = do equalSigns <- token "[" *> takeCharsWhile (== '=') <* token "[" <* optional (token "\n")
                    let terminator = token "]" *> string equalSigns *> token "]"
                    many (notFollowedBy terminator
                          *> satisfyChar (const True)) <* terminator,
   comment = string "--" *> (takeCharsWhile (/= '\n') <* (skip (char '\n') <|> endOfInput) <|> longBracket)
   }

traceRest :: Functor1 g => String -> Parser g String ()
--traceRest msg = pure ()
traceRest msg = lookAhead (takeCharsWhile (const True)) >>= \s-> trace ("[[" ++ msg ++ ":" ++ s ++ "]]") (pure ())
