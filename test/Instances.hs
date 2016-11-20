{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Language.Lua.Token
import Language.Lua.Syntax
import Language.Lua.Parser (NodeInfo)

import           Control.Applicative
import           Control.DeepSeq
import           Data.Char                  (isAsciiLower, isAsciiUpper, isDigit)
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NE
import           Data.Loc                   (L(..), Loc(..), Pos(..))
import           GHC.Generics               (Generic)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

--------------------------------------------------------------------------------
-- Arbitrary

#if !MIN_VERSION_base(4,8,0)
import Data.Typeable (Typeable)
type C a = (Arbitrary a, Typeable a)
#else
type C a = Arbitrary a
#endif

instance C a => Arbitrary (Ident a) where
    arbitrary = Ident <$> arbitrary <*> genIdent
      where
        genIdent :: Gen String
        genIdent = liftA2 (:) first rest `suchThat` \s -> not (HS.member s keywords)
          where
            first :: Gen Char
            first = frequency [(5, pure '_'), (95, arbitrary `suchThat` isAsciiLetter)]

            rest :: Gen String
            rest = listOf $
                frequency [ (10, pure '_')
                          , (45, arbitrary `suchThat` isAsciiLetter)
                          , (45, arbitrary `suchThat` isDigit)
                          ]

            -- Meh, forget unicode for now.
            isAsciiLetter :: Char -> Bool
            isAsciiLetter c = isAsciiLower c || isAsciiUpper c

        keywords :: HashSet String
        keywords = HS.fromList
            [ "and", "break", "do", "else", "elseif", "end", "false"
            , "for", "function", "goto", "if", "in", "local", "nil"
            , "not", "or", "repeat", "return", "then", "true", "until", "while"
            ]

instance C a => Arbitrary (IdentList a) where
    arbitrary = IdentList <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance C a => Arbitrary (IdentList1 a) where
    arbitrary = IdentList1 <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance C a => Arbitrary (Block a) where
    arbitrary = Block <$> arbitrary <*> listOf1 arbitrary <*> arbitrary
    shrink = genericShrink

instance C a => Arbitrary (Statement a) where
    arbitrary = oneof
        [ EmptyStmt      <$> arbitrary
        , Assign         <$> arbitrary <*> arbitrary <*> arbitrary
        , FunCall        <$> arbitrary <*> arbitrary
        , Label          <$> arbitrary <*> arbitrary
        , Break          <$> arbitrary
        , Goto           <$> arbitrary <*> arbitrary
        , Do             <$> arbitrary <*> arbitrary
        , While          <$> arbitrary <*> arbitrary <*> arbitrary
        , Repeat         <$> arbitrary <*> arbitrary <*> arbitrary
        , If             <$> arbitrary <*> arbitrary <*> arbitrary
        , For            <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , ForIn          <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , FunAssign      <$> arbitrary <*> arbitrary <*> arbitrary
        , LocalFunAssign <$> arbitrary <*> arbitrary <*> arbitrary
        , LocalAssign    <$> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance C a => Arbitrary (ReturnStatement a) where
    arbitrary = ReturnStatement <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance C a => Arbitrary (FunctionName a) where
    arbitrary = FunctionName <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

instance C a => Arbitrary (Variable a) where
    arbitrary = oneof
        [ VarIdent     <$> arbitrary <*> arbitrary
        , VarField     <$> arbitrary <*> arbitrary <*> arbitrary
        , VarFieldName <$> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance C a => Arbitrary (VariableList1 a) where
    arbitrary = VariableList1 <$> arbitrary <*> arbitrary

instance C a => Arbitrary (Expression a) where
    arbitrary = oneof
        [ Nil       <$> arbitrary
        , Bool      <$> arbitrary <*> arbitrary
        , Integer   <$> arbitrary <*> (show <$> (arbitrary :: Gen Int)) -- TODO: Make these better
        , Float     <$> arbitrary <*> (show <$> (arbitrary :: Gen Float))
        , String    <$> arbitrary <*> arbitrary
        , Vararg    <$> arbitrary
        , FunDef    <$> arbitrary <*> arbitrary
        , PrefixExp <$> arbitrary <*> arbitrary
        , TableCtor <$> arbitrary <*> arbitrary
        , Binop     <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , Unop      <$> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance C a => Arbitrary (ExpressionList a) where
    arbitrary = ExpressionList <$> arbitrary <*> arbitrary

instance C a => Arbitrary (ExpressionList1 a) where
    arbitrary = ExpressionList1 <$> arbitrary <*> arbitrary

instance C a => Arbitrary (PrefixExpression a) where
    arbitrary = oneof
        [ PrefixVar     <$> arbitrary <*> arbitrary
        , PrefixFunCall <$> arbitrary <*> arbitrary
        , Parens        <$> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance C a => Arbitrary (FunctionCall a) where
    arbitrary = oneof
        [ FunctionCall <$> arbitrary <*> arbitrary <*> arbitrary
        , MethodCall   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance C a => Arbitrary (FunctionArgs a) where
    arbitrary = oneof
        [ Args       <$> arbitrary <*> arbitrary
        , ArgsTable  <$> arbitrary <*> arbitrary
        , ArgsString <$> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance C a => Arbitrary (FunctionBody a) where
    arbitrary = FunctionBody <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

instance C a => Arbitrary (ParamList a) where
    arbitrary = oneof
        [ ParamList <$> arbitrary <*> arbitrary <*> arbitrary
        , ParamListVararg <$> arbitrary
        ]

instance C a => Arbitrary (TableConstructor a) where
    arbitrary = TableConstructor <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance C a => Arbitrary (Field a) where
    arbitrary = oneof
        [ FieldExp   <$> arbitrary <*> arbitrary <*> arbitrary
        , FieldIdent <$> arbitrary <*> arbitrary <*> arbitrary
        , Field      <$> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance C a => Arbitrary (FieldList a) where
    arbitrary = FieldList <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance C a => Arbitrary (Binop a) where
    arbitrary = oneof
        [ Plus       <$> arbitrary
        , Minus      <$> arbitrary
        , Mult       <$> arbitrary
        , FloatDiv   <$> arbitrary
        , FloorDiv   <$> arbitrary
        , Exponent   <$> arbitrary
        , Modulo     <$> arbitrary
        , BitwiseAnd <$> arbitrary
        , BitwiseXor <$> arbitrary
        , BitwiseOr  <$> arbitrary
        , Rshift     <$> arbitrary
        , Lshift     <$> arbitrary
        , Concat     <$> arbitrary
        , Lt         <$> arbitrary
        , Leq        <$> arbitrary
        , Gt         <$> arbitrary
        , Geq        <$> arbitrary
        , Eq         <$> arbitrary
        , Neq        <$> arbitrary
        , And        <$> arbitrary
        , Or         <$> arbitrary
        ]

    shrink = genericShrink

instance C a => Arbitrary (Unop a) where
    arbitrary = oneof
        [ Negate     <$> arbitrary
        , Not        <$> arbitrary
        , Length     <$> arbitrary
        , BitwiseNot <$> arbitrary
        ]

    shrink = genericShrink

--------------------------------------------------------------------------------
-- NFData

instance NFData NodeInfo
instance NFData Token

instance NFData a => NFData (Ident a)
instance NFData a => NFData (IdentList a)
instance NFData a => NFData (IdentList1 a)
instance NFData a => NFData (Block a)
instance NFData a => NFData (Statement a)
instance NFData a => NFData (ReturnStatement a)
instance NFData a => NFData (FunctionName a)
instance NFData a => NFData (Variable a)
instance NFData a => NFData (VariableList1 a)
instance NFData a => NFData (Expression a)
instance NFData a => NFData (ExpressionList a)
instance NFData a => NFData (ExpressionList1 a)
instance NFData a => NFData (PrefixExpression a)
instance NFData a => NFData (FunctionCall a)
instance NFData a => NFData (FunctionArgs a)
instance NFData a => NFData (FunctionBody a)
instance NFData a => NFData (ParamList a)
instance NFData a => NFData (TableConstructor a)
instance NFData a => NFData (Field a)
instance NFData a => NFData (FieldList a)
instance NFData a => NFData (Binop a)
instance NFData a => NFData (Unop a)

-- Orphans

deriving instance Generic a => Generic (L a)
instance (Generic a, NFData a) => NFData (L a)

deriving instance Generic Loc
instance NFData Loc

deriving instance Generic Pos
instance NFData Pos
