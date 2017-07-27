{-|
 - Module      : Trees
 - Description : Tree utilities.
 - Copyright   : (c) Maciej Bendkowski, 2017
 -
 - License     : BSD3
 - Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 - Stability   : experimental
 -}
module Trees
    ( Tree(..)
    , parseTree
    , printError
    , printFrequencies
    ) where

import Control.Monad (void)

import System.IO

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as S

data Tree = Node [Tree]
    deriving (Show)

size :: Tree -> Int
size (Node ts) = 1 + sum (map size ts)

frequencies :: Tree -> MultiSet Int
frequencies (Node ts) = length ts `S.insert` S.unions ts'
    where ts' = map frequencies ts

printFrequencies :: Tree -> IO ()
printFrequencies t = do
    let n   = size t
    let fs  = S.toOccurList $ frequencies t
    let fs' = map (\(d, f) -> show d ++ ": " ++ show (fromIntegral f / fromIntegral n)) fs
    hPutStrLn stderr "Degree frequencies:"
    mapM_ (hPutStrLn stderr) fs'

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt  = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

treeStmt :: Parser Tree
treeStmt = do
    ts <- parens $ many treeStmt
    return $ Node ts

parseFromFile :: Parsec e String a
              -> String
              -> IO (Either (ParseError Char e) a)

parseFromFile p file = runParser p file <$> readFile file

-- | Parses the given tree specification.
parseTree :: String -> IO (Either (ParseError Char Dec) Tree)
parseTree = parseFromFile treeStmt

-- | Prints the given parsing errors.
printError :: ParseError Char Dec -> IO ()
printError err = putStr $ parseErrorPretty err
