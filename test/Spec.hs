{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor.Compose (Compose(..))
import Instances ()
import Language.Lua.Lexer
import Language.Lua.Parser
import Language.Lua.Parser.Internal
import Language.Lua.Pretty
import Language.Lua.Syntax
import Language.Lua.Token

import Language.Lua.Grammar
import Text.Grampa (parseAll, ParseFailure(..))

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$))
#endif
import           Control.DeepSeq       (rnf)
import           Control.Exception     (evaluate)
import           Data.List             (isSuffixOf)
import qualified Data.List.NonEmpty    as NE
import           Data.Loc
import           Data.Monoid
import qualified Data.Sequence         as Seq
import           System.Directory (listDirectory)
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ lexerTests
  , parserTests
  , grammarTests
  -- , prettyTests
  ]

lexerTests :: TestTree
lexerTests = testGroup "lexer tests"
  [ stringLiteralTests
  , intLiteralTests
  , floatLiteralTests
  , commentTests
  ]
 where
  stringLiteralTests :: TestTree
  stringLiteralTests = testCase "string literals" $ do
    let tk = [TkStringLit "alo\n123\""]
    l "\'alo\\\n123\"\'"        @?= tk
    l "\"alo\\\n123\\\"\""      @?= tk
    l "\'\\97lo\\10\\04923\"\'" @?= tk
    l "[[alo\n123\"]]"          @?= tk
    l "[==[\nalo\n123\"]==]"    @?= tk

  intLiteralTests :: TestTree
  intLiteralTests = testCase "int literals" $ do
    l "3"        @?= [TkIntLit "3"]
    l "345"      @?= [TkIntLit "345"]
    l "0xff"     @?= [TkIntLit "0xff"]
    l "0XBEBada" @?= [TkIntLit "0XBEBada"]

  floatLiteralTests :: TestTree
  floatLiteralTests = testCase "float literals" $ do
    l "3."        @?= [TkFloatLit "3."]
    l "3.1416"    @?= [TkFloatLit "3.1416"]
    l "3.14e-2"   @?= [TkFloatLit "3.14e-2"]
    l "3.14E+2"   @?= [TkFloatLit "3.14E+2"]
    l "3.14E2"    @?= [TkFloatLit "3.14E2"]
    l "0x0.1E"    @?= [TkFloatLit "0x0.1E"]
    l "0xA23p-4"  @?= [TkFloatLit "0xA23p-4"]
    l "0X1.92P+1" @?= [TkFloatLit "0X1.92P+1"]
    l "0X1.92p1"  @?= [TkFloatLit "0X1.92p1"]

  commentTests :: TestTree
  commentTests = testCase "comments" $ do
    l "--"                     @?= []
    l "--just a comment"       @?= []
    l "-- just a comment"      @?= []
    l "3 -- hi"                @?= [TkIntLit "3"]
    l "3 --[ hi"               @?= [TkIntLit "3"]
    l "3 --[\nhi"              @?= [TkIntLit "3", TkIdent "hi"]
    l "3 --[[hello\nworld]] 4" @?= [TkIntLit "3", TkIntLit "4"]

  l :: String -> [Token]
  l = map unLoc . streamToList . runLexer luaLexer ""

parserTests :: TestTree
parserTests = testGroup "parser tests"
  [ testCase "sepBy 0" $ do
      let ([is], _) = fullParses (parser identListG) ([] :: [L Token])
      is @?= IdentList (NodeInfo NoLoc Seq.empty) []
  , testCase "sepBy 1" $ do
      let ([is], _) = fullParses (parser identListG) (l "hi")
          loc1 = Loc (Pos "" 1 1 0) (Pos "" 1 2 1)
      is @?= IdentList (NodeInfo loc1 (Seq.fromList [L loc1 (TkIdent "hi")]))
                       [Ident (NodeInfo loc1 (Seq.fromList [L loc1 (TkIdent "hi")])) "hi"]
  , testCase "sepBy 2" $ do
      let ([is], _) = fullParses (parser identListG) (l "hi,ho")
          loc1 = Loc (Pos "" 1 1 0) (Pos "" 1 2 1) -- "hi"
          loc2 = Loc (Pos "" 1 3 2) (Pos "" 1 3 2) -- ","
          loc3 = Loc (Pos "" 1 4 3) (Pos "" 1 5 4) -- "ho"
      is @?= IdentList (NodeInfo (loc1 <> loc3) (Seq.fromList [ L loc1 (TkIdent "hi")
                                                              , L loc2 TkComma
                                                              , L loc3 (TkIdent "ho")
                                                              ]))
                       [ Ident (NodeInfo loc1 (Seq.fromList [L loc1 (TkIdent "hi")])) "hi"
                       , Ident (NodeInfo loc3 (Seq.fromList [L loc3 (TkIdent "ho")])) "ho"
                       ]
  , testCase "sepBy 3" $ do
      let ([is], _) = fullParses (parser identListG) (l "hi,ho,ha")
          loc1 = Loc (Pos "" 1 1 0) (Pos "" 1 2 1) -- "hi"
          loc2 = Loc (Pos "" 1 3 2) (Pos "" 1 3 2) -- ","
          loc3 = Loc (Pos "" 1 4 3) (Pos "" 1 5 4) -- "ho"
          loc4 = Loc (Pos "" 1 6 5) (Pos "" 1 6 5) -- ","
          loc5 = Loc (Pos "" 1 7 6) (Pos "" 1 8 7) -- "ha"
      is @?= IdentList (NodeInfo (loc1 <> loc5) (Seq.fromList [ L loc1 (TkIdent "hi")
                                                              , L loc2 TkComma
                                                              , L loc3 (TkIdent "ho")
                                                              , L loc4 TkComma
                                                              , L loc5 (TkIdent "ha")
                                                              ]))
                       [ Ident (NodeInfo loc1 (Seq.fromList [L loc1 (TkIdent "hi")])) "hi"
                       , Ident (NodeInfo loc3 (Seq.fromList [L loc3 (TkIdent "ho")])) "ho"
                       , Ident (NodeInfo loc5 (Seq.fromList [L loc5 (TkIdent "ha")])) "ha"
                       ]
  , testCase "sepBy1 1" $ do
      let ([is], _) = fullParses (parser identList1G) (l "hi")
          loc1 = Loc (Pos "" 1 1 0) (Pos "" 1 2 1)
      is @?= IdentList1 (NodeInfo loc1 (Seq.fromList [L loc1 (TkIdent "hi")]))
                        [Ident (NodeInfo loc1 (Seq.fromList [L loc1 (TkIdent "hi")])) "hi"]
  , testCase "sepBy1 2" $ do
      let ([is], _) = fullParses (parser identList1G) (l "hi,ho")
          loc1 = Loc (Pos "" 1 1 0) (Pos "" 1 2 1) -- "hi"
          loc2 = Loc (Pos "" 1 3 2) (Pos "" 1 3 2) -- ","
          loc3 = Loc (Pos "" 1 4 3) (Pos "" 1 5 4) -- "ho"
      is @?= IdentList1 (NodeInfo (loc1 <> loc3) (Seq.fromList [ L loc1 (TkIdent "hi")
                                                               , L loc2 TkComma
                                                               , L loc3 (TkIdent "ho")
                                                               ]))
                        [ Ident (NodeInfo loc1 (Seq.fromList [L loc1 (TkIdent "hi")])) "hi"
                        , Ident (NodeInfo loc3 (Seq.fromList [L loc3 (TkIdent "ho")])) "ho"
                        ]
  , testCase "sepBy1 3" $ do
      let ([is], _) = fullParses (parser identList1G) (l "hi,ho,ha")
          loc1 = Loc (Pos "" 1 1 0) (Pos "" 1 2 1) -- "hi"
          loc2 = Loc (Pos "" 1 3 2) (Pos "" 1 3 2) -- ","
          loc3 = Loc (Pos "" 1 4 3) (Pos "" 1 5 4) -- "ho"
          loc4 = Loc (Pos "" 1 6 5) (Pos "" 1 6 5) -- ","
          loc5 = Loc (Pos "" 1 7 6) (Pos "" 1 8 7) -- "ha"
      is @?= IdentList1 (NodeInfo (loc1 <> loc5) (Seq.fromList [ L loc1 (TkIdent "hi")
                                                               , L loc2 TkComma
                                                               , L loc3 (TkIdent "ho")
                                                               , L loc4 TkComma
                                                               , L loc5 (TkIdent "ha")
                                                               ]))
                        [ Ident (NodeInfo loc1 (Seq.fromList [L loc1 (TkIdent "hi")])) "hi"
                        , Ident (NodeInfo loc3 (Seq.fromList [L loc3 (TkIdent "ho")])) "ho"
                        , Ident (NodeInfo loc5 (Seq.fromList [L loc5 (TkIdent "ha")])) "ha"
                        ]
  , testCase "field list 1" $ do
      let ([fs], _) = fullParses (parser luaFieldList) (l "[\"a\"]=b")
          loc1 = Loc (Pos "" 1 1 0) (Pos "" 1 1 0) -- "["
          loc2 = Loc (Pos "" 1 2 1) (Pos "" 1 4 3) -- '"a"'
          loc3 = Loc (Pos "" 1 5 4) (Pos "" 1 5 4) -- "]"
          loc4 = Loc (Pos "" 1 6 5) (Pos "" 1 6 5) -- "="
          loc5 = Loc (Pos "" 1 7 6) (Pos "" 1 7 6) -- "b"
          info = NodeInfo (loc1 <> loc5) (Seq.fromList [ L loc1 TkLBracket
                                                       , L loc2 (TkStringLit "a")
                                                       , L loc3 TkRBracket
                                                       , L loc4 TkAssign
                                                       , L loc5 (TkIdent "b")
                                                       ])
          a_info = NodeInfo loc2 (Seq.fromList [L loc2 (TkStringLit "a")])
          b_info = NodeInfo loc5 (Seq.fromList [L loc5 (TkIdent "b")])

      fs @?= FieldList info [ FieldExp info
                                       (String a_info "a")
                                       (PrefixExp b_info
                                        (PrefixVar b_info
                                         (VarIdent b_info
                                          (Ident b_info "b")))) ]

  , testCase "param list" $ do
      let ([p], _) = fullParses (parser luaParamList) (l "...")
          loc = Loc (Pos "" 1 1 0) (Pos "" 1 3 2)
      p @?= ParamListVararg (NodeInfo loc (Seq.singleton (L loc TkVararg)))

  , testCase "param list 2" $ do
      let ([p], _) = fullParses (parser luaParamList) (l "foo, bar")
          loc1 = Loc (Pos "" 1 1 0) (Pos "" 1 3 2)
          loc2 = Loc (Pos "" 1 4 3) (Pos "" 1 4 3)
          loc3 = Loc (Pos "" 1 6 5) (Pos "" 1 8 7)
          tk1  = L loc1 (TkIdent "foo")
          tk2  = L loc2 TkComma
          tk3  = L loc3 (TkIdent "bar")
          info = NodeInfo (loc1 <> loc3) (Seq.fromList [tk1, tk2, tk3])
      p @?= ParamList info (IdentList1 info [ Ident (NodeInfo loc1 (Seq.singleton tk1)) "foo"
                                            , Ident (NodeInfo loc3 (Seq.singleton tk3)) "bar"
                                            ])
                           False

    , testCase "parse 1.lua" (parseFile "test/samples/1.lua")
    , testCase "parse 2.lua" (parseFile "test/samples/2.lua")

    -- TODO: Make smarter shrinks and re-enable
    -- , QC.testProperty "Pretty-printer round-trip" (\luaAst -> luaFromString (luaToString luaAst) == Just luaAst)
    ]
 where
  l :: String -> [L Token]
  l = streamToList . runLexer luaLexer ""
  parseFile :: String -> IO ()
  parseFile file = readFile file >>= evaluate . rnf . parseLua file

grammarTests :: TestTree
grammarTests = testGroup "grammar tests"
  [ testCase "sepBy 0" $ do
      namelist (parseAll luaGrammar "") @?= Compose (Left (ParseFailure 0 ["satisfyChar"]))
  , testCase "sepBy 1" $ do
      let loc1 = NoLoc
      namelist (parseAll luaGrammar ("hi" :: String))
         @?= Compose (Right [IdentList1 (NodeInfo loc1 mempty) [Ident (NodeInfo loc1 mempty) "hi"]])
  , testCase "sepBy 2" $ do
      let Compose (Right [is]) = namelist (parseAll luaGrammar "hi,ho")
          loc1 = NoLoc -- "hi"
          loc2 = NoLoc -- ","
          loc3 = NoLoc -- "ho"
      is @?= IdentList1 (NodeInfo loc1 mempty)
                        [ Ident (NodeInfo loc1 mempty) "hi"
                        , Ident (NodeInfo loc3 mempty) "ho"
                        ]
  , testCase "sepBy 3" $ do
      let Compose (Right [is]) = namelist (parseAll luaGrammar "hi,ho,ha")
          loc1 = NoLoc -- "hi"
          loc2 = NoLoc -- ","
          loc3 = NoLoc -- "ho"
          loc4 = NoLoc -- ","
          loc5 = NoLoc -- "ha"
      is @?= IdentList1 (NodeInfo loc1 mempty)
                        [ Ident (NodeInfo loc1 mempty) "hi"
                        , Ident (NodeInfo loc3 mempty) "ho"
                        , Ident (NodeInfo loc5 mempty) "ha"
                        ]
  , testCase "field list 1" $ do
      let Compose (Right [fs]) = fieldlist (parseAll luaGrammar "[\"a\"]=b")
          loc1 = NoLoc -- "["
          loc2 = NoLoc -- '"a"'
          loc3 = NoLoc -- "]"
          loc4 = NoLoc -- "="
          loc5 = NoLoc -- "b"
          info = NodeInfo loc1 mempty
          a_info = NodeInfo loc2 mempty
          b_info = NodeInfo loc5 mempty

      fs @?= FieldList info [ FieldExp info
                                       (String a_info "a")
                                       (PrefixExp b_info
                                        (PrefixVar b_info
                                         (VarIdent b_info
                                          (Ident b_info "b")))) ]

  , testCase "param list" $ do
      let Compose (Right [p]) = parlist (parseAll luaGrammar "...")
          loc = NoLoc
      p @?= ParamListVararg (NodeInfo loc mempty)

  , testCase "param list 2" $ do
      let Compose (Right [p]) = parlist (parseAll luaGrammar "foo, bar")
          loc1 = NoLoc
          loc2 = NoLoc
          loc3 = NoLoc
          info = NodeInfo loc1 mempty
      p @?= ParamList info (IdentList1 info [ Ident (NodeInfo loc1 mempty) "foo"
                                            , Ident (NodeInfo loc3 mempty) "bar"
                                            ])
                           False

    , testCase "parse 1.lua" (parseFile "test/samples/1.lua")
    , testCase "parse 2.lua" (parseFile "test/samples/2.lua")
    , testCaseSteps "Lua 5.3.3 test suite" (testDir "test/lua-5.3.3-tests/")
--    , buildTest (testGroup "Lua 5.3.3 test suite" <$> testDir "test/lua-5.3.3-tests/")
    ]
 where
    testDir dirName step = listDirectory dirName >>= mapM_ testFile
       where testFile fileName
                | ".lua" `isSuffixOf` fileName = step fileName >> parseFile (dirName <> fileName)
                | otherwise = return ()
--    testDir dirName = testFile <$> listDirectory dirName
    testFile fileName
       | ".lua" `isSuffixOf` fileName = testCase fileName (parseFile fileName)
    parseFile :: String -> IO ()
    parseFile file = readFile file >>= evaluate . rnf . assertRight . getCompose . chunk . parseAll luaGrammar
       where assertRight = either (\failure-> error ("Failed to parse file " ++ file ++ ": " ++ show failure)) id

luaToString :: Chunk () -> String
luaToString c = displayS (renderPretty 1.0 80 (pretty c)) ""

luaFromString :: String -> Maybe (Chunk ())
luaFromString s = either (const Nothing) Just (streamToEitherList (runLexer luaLexer "" s)) >>= \tks ->
  case fullParses (parser luaChunk) tks of
    ([c], _) -> Just (() <$ c)
    _        -> Nothing
