{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Language.Lua.Grammar as Grammar
import qualified Language.Lua.Parser as Parser

import Control.Monad
import Data.Functor.Compose (getCompose)
import Data.Typeable (Typeable)
import Options.Applicative
import Text.Grampa (parseComplete)
import Text.PrettyPrint.Leijen    (Pretty(..), displayS, renderPretty)

data GrammarMode = ChunkMode | StatementMode | ExpressionMode
    deriving Show

data BackendMode = EarleyMode | GrampaMode
    deriving Show

data Opts = Opts
    { optsInteractive :: GrammarMode
    , optsBackend     :: BackendMode
    , optsPretty      :: Bool
    , optsFile        :: Maybe FilePath
    } deriving Show

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> p)
        ( fullDesc
       <> progDesc "Parse a lua file, or parse interactively"
       <> header "Lua parser")

    p :: Parser Opts
    p = Opts
        <$> (mode <|> pure ExpressionMode)
        <*> (backend <|> pure GrampaMode)
        <*> switch
            ( long "pretty"
           <> help "Pretty-print output")
        <*> optional (strArgument
            ( metavar "FILE"
           <> help "Lua file to parse"))

    mode :: Parser GrammarMode
    mode = ChunkMode      <$ switch (long "chunk")
       <|> StatementMode  <$ switch (long "statement")
       <|> ExpressionMode <$ switch (long "expression")

    backend :: Parser BackendMode
    backend = EarleyMode <$ switch (long "earley")
              <|> GrampaMode  <$ switch (long "grampa")

main' :: Opts -> IO ()
main' Opts{..} =
    case optsFile of
        Just file -> readFile file >>= go Parser.luaChunk Grammar.chunk file
        Nothing ->
            case optsInteractive of
                ChunkMode      -> forever $ getLine >>= go Parser.luaChunk    Grammar.chunk "<stdin>"
                StatementMode  -> forever $ getLine >>= go Parser.luaStatement Grammar.stat "<stdin>"
                ExpressionMode -> forever $ getLine >>= go Parser.luaExpression Grammar.exp "<stdin>"
  where
    go :: (Show (f Parser.NodeInfo), Pretty (f Parser.NodeInfo), Typeable f) => 
          Parser.LuaGrammar f -> (forall x. Grammar.LuaGrammar Parser.NodeInfo x -> x (f Parser.NodeInfo))
          -> String -> String -> IO ()
    go g f filename contents = do
        let Right [x] = case optsBackend 
                        of EarleyMode -> Right [Parser.parseLuaWith g filename contents]
                           GrampaMode -> getCompose (f $ parseComplete Grammar.luaGrammar contents)
        if optsPretty
             then putStrLn $ displayS (renderPretty 1.0 80 (pretty x)) ""
             else print x
