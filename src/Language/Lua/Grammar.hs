{-# Language OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
module Language.Lua.Grammar where

import Control.Applicative
import Control.Monad (guard, void)
import Data.Char (chr, isDigit, isLetter)
import Data.Functor.Classes (Show1, showsPrec1)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Monoid ((<>))
import Data.Monoid.Textual (TextualMonoid, toString)
import Numeric (readHex)

import qualified Rank2
import qualified Rank2.TH
import Text.Grampa
import Text.Grampa.ContextFree.LeftRecursive (Parser)

import Text.Parser.Char (alphaNum, anyChar, char, digit, hexDigit)
import qualified Text.Parser.Char as P
import Text.Parser.Combinators (choice, count, sepBy, skipMany, try)
import Text.Parser.Expression (Assoc(..), Operator(..))

import Language.Lua.Syntax
import Language.Lua.Parser.Internal (NodeInfo(..))

import Prelude hiding (exp, exponent)

data LuaGrammar a f = LuaGrammar{
   chunk :: f (Block a),
   block :: f (Block a),
   stats :: f [Statement a],
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
   primaryexp :: f (Expression a),
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

$(Rank2.TH.deriveAll ''LuaGrammar)

instance (Show1 f, Show a) => Show (LuaGrammar a f) where
   showsPrec prec g rest = "LuaGrammar{" ++
      "  chunk = " ++ showsPrec1 prec (chunk g) "\n" ++
      "  block = " ++ showsPrec1 prec (block g) "\n" ++
      "  stats = " ++ showsPrec1 prec (stats g) "\n" ++
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

moptional :: (Monoid x, Alternative p) => p x -> p x
moptional p = p <|> pure mempty

ignorable :: (TextualMonoid t, Parsing (p (LuaGrammar NodeInfo) t), GrammarParsing p, MonoidParsing (p (LuaGrammar NodeInfo))) => 
             p (LuaGrammar NodeInfo) t ()
ignorable = whiteSpace *> skipMany (nonTerminal comment *> whiteSpace)

sepBy1 :: Alternative p => p x -> p sep -> p (NonEmpty x)
sepBy1 p sep = (:|) <$> p <*> many (sep *> p)

upto :: (TextualMonoid s, MonoidParsing p) => Int -> (Char -> Bool) -> p s s
upto n0 predicate = scanChars n0 (\n c-> if n > 0 && predicate c then Just (pred n) else Nothing)

-- | Tweaked version of 'Text.Parser.Expression.buildExpressionParser' that allows chaining prefix operators of arbitrary
-- precedence
buildExpressionParser :: forall m g s a. (GrammarParsing m, Parsing (m g s)) =>
                         [[Operator (m g s) a]] -> m g s a -> m g s a
buildExpressionParser operators simpleExpr = foldl makeParser prefixExpr operators
   where
      prefixExpr = foldl makePrefixParser simpleExpr operators
      makePrefixParser term ops =
         let (_, _, _, prefix, postfix) = foldr splitOp ([],[],[],[],[]) ops
             prefixOp   = choice prefix  <?> ""
             postfixOp  = choice postfix <?> ""
             termP         = (prefixFactor <|> term) <**> postfixFactor
             prefixFactor  = foldr (.) id <$> some prefixOp <*> makeParser term ops
             postfixFactor = foldr (flip (.)) id <$> many postfixOp
         in termP <?> "operator"
      makeParser term ops
        = let (rassoc,lassoc,nassoc,_prefix,_postfix) = foldr splitOp ([],[],[],[],[]) ops
              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc

              ambiguous assoc op = try (op *> empty <?> ("ambiguous use of a " ++ assoc ++ "-associative operator"))

              ambiguousLeft, ambiguousNon, ambiguousRight :: m g s (x -> x)
              ambiguousRight    = ambiguous "right" rassocOp
              ambiguousLeft     = ambiguous "left" lassocOp
              ambiguousNon      = ambiguous "non" nassocOp

              termP = term

              rassocP  = (flip <$> rassocOp <*> (termP <**> recursive rassocP1)
                          <|> ambiguousLeft
                          <|> ambiguousNon)

              rassocP1 = rassocP <|> pure id

              lassocP  = ((flip <$> lassocOp <*> termP) <**> ((.) <$> recursive lassocP1)
                          <|> ambiguousRight
                          <|> ambiguousNon)

              lassocP1 = lassocP <|> pure id

              nassocP = (flip <$> nassocOp <*> termP)
                        <**> (ambiguousRight
                              <|> ambiguousLeft
                              <|> ambiguousNon
                              <|> pure id)
           in term <**> (rassocP <|> lassocP <|> nassocP <|> pure id) <?> "operator"


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)

node :: Applicative p => (NodeInfo -> x) -> p x
node f = pure (f mempty)

keyword :: (Show t, TextualMonoid t, CharParsing (p (LuaGrammar NodeInfo) t), 
            GrammarParsing p, MonoidParsing (p (LuaGrammar NodeInfo))) => t -> p (LuaGrammar NodeInfo) t t
keyword k = ignorable *> string k <* notFollowedBy alphaNum

symbol :: (Show t, TextualMonoid t,
           Parsing (p (LuaGrammar NodeInfo) t), GrammarParsing p, MonoidParsing (p (LuaGrammar NodeInfo))) => 
          t -> p (LuaGrammar NodeInfo) t t
symbol s = ignorable *> string s

toExpList :: ExpressionList1 a -> ExpressionList a
toExpList (ExpressionList1 a l) = ExpressionList a (toList l)

-- Section 3.1
reservedKeywords  :: [String]
reservedKeywords = ["and", "break", "do", "else", "elseif", "end",
                    "false", "for", "function", "goto", "if", "in",
                    "local", "nil", "not", "or", "repeat", "return",
                    "then", "true", "until", "while"]

luaGrammar :: (Eq t, Show t, TextualMonoid t) => Grammar (LuaGrammar NodeInfo) Parser t
luaGrammar = fixGrammar grammar

grammar :: (Eq t, Show t, TextualMonoid t) =>
           GrammarBuilder (LuaGrammar NodeInfo) (LuaGrammar NodeInfo) Parser t
grammar LuaGrammar{..} = LuaGrammar{
   chunk = optional (token "#" *> takeCharsWhile (/= '\n') *> (void (token "\n") <|> endOfInput))
           *> block <* ignorable <* endOfInput,
   block = node Block <*> stats <*> optional retstat,
   stats = (:) <$> stat <*> stats <|> pure [],
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

   -- Operator precedence from 3.4.8
   exp = let binary op = Infix (node Binop <*> op)
             operators = [
                [binary (node Exponent <* symbol "^") AssocRight],
                [Prefix (node Unop <*> unop)],
                [binary (node Mult <* symbol "*") AssocLeft,
                 binary (node FloatDiv <* symbol "/") AssocLeft,
                 binary (node FloorDiv <* symbol "//") AssocLeft,
                 binary (node Modulo <* symbol "%") AssocLeft],
                [binary (node Plus <* symbol "+") AssocLeft,
                 binary (node Minus <* symbol "-" <* notFollowedBy (char '-')) AssocLeft],
                [binary (node Concat <* symbol "..") AssocRight],
                [binary (node Lshift <* symbol "<<") AssocLeft,
                 binary (node Rshift <* symbol ">>") AssocLeft],
                [binary (node BitwiseAnd <* symbol "&") AssocLeft],
                [binary (node BitwiseXor <* symbol "~") AssocLeft],
                [binary (node BitwiseOr <* symbol "|") AssocLeft],
                [binary (node Lt <* symbol "<") AssocLeft,
                 binary (node Gt <* symbol ">") AssocLeft,
                 binary (node Leq <* symbol "<=") AssocLeft,
                 binary (node Geq <* symbol ">=") AssocLeft,
                 binary (node Neq <* symbol "~=") AssocLeft,
                 binary (node Eq <* symbol "==") AssocLeft],
                [binary (node And <* keyword "and") AssocLeft],
                [binary (node Or <* keyword "or") AssocLeft]]
         in buildExpressionParser operators primaryexp,
   primaryexp =
      node Nil <* keyword "nil" <|>
      node Bool <*> pure False <* keyword "false" <|>
      node Bool <*> pure True <* keyword "true" <|>
      numeral <|>
      node String <*> literalString <|>
      node Vararg <* symbol "..." <|>
      node FunDef <*> functiondef <|>
      node PrefixExp <*> prefixexp <* notFollowedBy (symbol "(") <|> -- fix the ambiguity from 3.3.1
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

   fieldlist = node FieldList <*> ((toList <$> sepBy1 field fieldsep) <* optional fieldsep <|> pure []),
   field =
      node FieldExp <* symbol "[" <*> exp <* symbol "]" <* symbol "=" <*> exp <|>
      node FieldIdent <*> name <* symbol "=" <*> exp <|>
      node Field <*> exp,

   fieldsep = void (symbol "," <|> symbol ";"),

   binop =
      node Plus       <* symbol "+"    <|>
      node Minus      <* symbol "-" <* notFollowedBy (char '-') <|> 
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
      node Negate     <* symbol "-" <* notFollowedBy (char '-') <|>  -- eliminate ambiguity
      node Not        <* keyword "not" <|>
      node Length     <* symbol "#"    <|>
      node BitwiseNot <* symbol "~",

   numeral = ignorable *>
             (node Integer <*> digits <|>
              node Float <*> (digits <> P.string "." <> moptional digits <> moptional exponent) <|>
              node Float <*> (P.string "." <> digits <> moptional exponent) <|>
              node Float <*> (digits <> exponent) <|>
              node Integer <*> initialHexDigits <|>
              node Float <*> (initialHexDigits <> P.string "." <> moptional hexDigits <> moptional hexExponent) <|>
              node Float <*> ((P.string "0x." <|> P.string "0X.") <> hexDigits <> moptional hexExponent) <|>
              node Float <*> (initialHexDigits <> hexExponent))
             <* notFollowedBy alphaNum,
   digits = some digit,
   hexDigits = some hexDigit,
   initialHexDigits = (P.string "0x" <|> P.string "0X") <> hexDigits,
   exponent = (P.string "e" <|> P.string "E") <> moptional (P.string "+" <|> P.string "-") <> digits,
   hexExponent = (P.string "p" <|> P.string "P") <> moptional (P.string "+" <|> P.string "-") <> digits,
   name = do ignorable
             let isStartChar c = isLetter c || c == '_'
                 isNameChar c = isStartChar c || isDigit c
             identifier <- ((:) <$> satisfyChar isStartChar <*> (toString (const "") <$> takeCharsWhile isNameChar))
             guard (notElem identifier reservedKeywords)
             node Ident <*> pure identifier
          <?> "name",
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
                              ((:[]) . chr) <$> (read <$> ((:) <$> digit <*> (toString (const "") <$> upto 2 isDigit))
                                                 <|>
                                                 token "x" *> ((fst . head . readHex) <$> count 2 hexDigit) <|>
                                                 string "u{" *> ((fst . head . readHex) <$> some hexDigit) <* token "}")
                                <|>
                              "" <$ token "z" <* whiteSpace)
                       literalWith quote = char quote
                                           *> concatMany (escapeSequence <|>
                                                          toString (const "") <$> takeCharsWhile1 (\c-> c /= '\\' && c /= quote)) 
                                           <* char quote
                   in literalWith '"' <|> 
                      literalWith '\'' <|> 
                      longBracket,
   longBracket = do void (token "[")
                    equalSigns <- takeCharsWhile (== '=')
                    void (token "[")
                    void (token "\n") <|> notFollowedBy (token "\n")
                    let terminator = token "]" *> string equalSigns *> token "]"
                    many (notFollowedBy terminator *> anyChar) <* terminator,
   comment = string "--" *> (toString (const "") <$> takeCharsWhile (/= '\n') <* (void (char '\n') <|> endOfInput) <|>
                             longBracket)
   }
