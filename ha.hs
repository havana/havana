{-# LANGUAGE DeriveGeneric, DefaultSignatures, TupleSections #-}

module Main where

import qualified Options.Applicative as O
import qualified System.Directory as D
import qualified Data.ByteString as B
import Control.Applicative ((<|>))
import Control.Monad (liftM, foldM, forM_)
import Data.Char (isAlphaNum, toLower)
import Data.List (isSuffixOf, group, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Serialize
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(ReadMode))
import Text.Parsec (parse, many1, (<?>), eof)
import Text.Parsec.Char (alphaNum)
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

data HACommand = HABuild FilePath | HAQuery FilePath String | HADump FilePath

haCommand :: O.Parser HACommand
haCommand = haBuild <|> haQuery <|> haDump
  where haBuild = HABuild <$>
          O.strOption (O.long "build"
                        <> O.metavar "DIR"
                        <> O.help "Build index for speficied directory")
        haQuery = HAQuery
          <$> O.strOption (O.long "dir"
                             <> O.metavar "DIR"
                             <> O.help "Directory where to run query")
          <*> O.strOption (O.short 'q'
                             <> O.long "query"
                             <> O.metavar "\"QUERY\""
                             <> O.help "Query index with a boolean expression")
        haDump = HADump <$>
          O.strOption (O.long "dump"
                        <> O.metavar "DIR"
                        <> O.help "Dump index build in specified directory")

parseHACommand :: IO HACommand
parseHACommand = O.execParser $ O.info (O.helper <*> haCommand) O.fullDesc

norm :: String -> String
norm = (map toLower . filter isAlphaNum)

normTokens :: String -> [String]
normTokens = map head . group . sort . filter (not . null) . map norm . words

type II = (String,[Int])
mergeI :: [II] -> [II] -> [II]
mergeI [] a = a
mergeI a [] = a
mergeI ix1@(ti1@(t1,i1):ix1s) ix2@(ti2@(t2,i2):ix2s)
  | t1 < t2  = ti1 : mergeI ix1s ix2
  | t2 < t1  = ti2 : mergeI ix1  ix2s
  | t1 == t2 = (t1,i1++i2) : mergeI ix1s ix2s

data HAIndex = HAIndex [II] [String] deriving (Generic)
instance Serialize HAIndex

buildIndex :: FilePath -> IO ()
buildIndex dir = do
  files <- liftM (filter (isSuffixOf ".txt")) $ D.listDirectory dir
  idx <- foldM updateI [] (zip files [1..])
  B.writeFile (dir </> "haindex") (encode $ HAIndex idx files)
 where updateI idx (f,i) = readFile (dir </> f) >>=
         return . mergeI idx . map (,[i]) . normTokens

readIndex :: FilePath -> IO HAIndex
readIndex dir = do
  bytes <- B.readFile (dir </> "haindex")
  case decode bytes of
    (Left str) -> error $! "Error: " ++ str
    (Right hi) -> return $! hi

dumpIndex :: FilePath -> IO ()
dumpIndex dir = do
  readIndex dir >>= \(HAIndex ii ss) ->
    mapM_ writeEntry ii >> mapM_ listFile (zip ss [1..])
  where
   writeEntry (s,is) = putStrLn $! s ++ ": " ++ show is
   listFile   (f,i)  = putStrLn $! show i ++ ": " ++ (dir </> f)

data LogicOp = AND | OR deriving (Show)
data Expr = Logic LogicOp Expr Expr | Term String deriving (Show)
lexer = P.makeTokenParser (emptyDef { P.reservedOpNames = ["&&", "||"]
                                    , P.identStart = alphaNum})
rsvdOp = P.reservedOp lexer
parens = P.parens lexer
literal = P.identifier lexer

table = [ [boolean "&&" AND], [boolean "||" OR] ]
boolean s o = Infix (do {rsvdOp s; return (Logic o)}) AssocRight
expr = buildExpressionParser table term <?> "Boolean query"
term = parens expr <|> multiterm
multiterm = conj <$> many1 literal
  where
    conj [x] = Term x
    conj (x:xs) = Logic AND (Term x) (conj xs)

runQuery :: FilePath -> String -> IO ()
runQuery d q = do
  case parse expr "" q of
    (Left e) -> error $ show e
    (Right ast) -> readIndex d >>= \hi -> listResults d hi (eval hi ast)

eval :: HAIndex -> Expr -> [Int]
eval hi t@(Term _) = fetch hi t
eval hi (Logic AND l r) = conj (fetch hi l) (fetch hi r)
eval hi (Logic OR  l r) = disj (fetch hi l) (fetch hi r)

fetch :: HAIndex -> Expr -> [Int]
fetch    (HAIndex ii _) (Term s) = fromMaybe [] $! lookup (norm s) ii
fetch hi@(HAIndex ii _) expr     = eval hi expr

conj :: [Int] -> [Int] -> [Int]
conj lhs@(a:aa) rhs@(b:bb) | a == b = a : conj aa bb
                           | a  < b = conj aa rhs
                           | a  > b = conj lhs bb
conj _ _ = []

disj :: [Int] -> [Int] -> [Int]
disj lhs@(a:aa) rhs@(b:bb) | a == b = a : disj aa bb
                           | a  < b = a : disj aa rhs
                           | a  > b = b : disj lhs bb
disj x [] = x
disj [] x = x

listResults :: FilePath -> HAIndex -> [Int] -> IO ()
listResults d (HAIndex _ ss) ii = do
  let num = length ii
  putStrLn $! show num ++ " result(s) found" ++ if num == 0 then "." else ":"
  listResults' (zip ss [1..]) ii
  where
   listResults' :: [(String,Int)] -> [Int] -> IO ()
   listResults' db@((s,i):ss) rs@(k:ks) | i == k  = listResult s >> listResults' ss ks
                                        | i <  k  = listResults' ss rs
                                        | i >  k  = listResults' db ks
   listResults' _ _ = return ()
   listResult s = putStrLn $! "  " ++ d </> s

main :: IO ()
main = do
  cmd <- parseHACommand
  case cmd of
    (HABuild dir)     -> buildIndex dir
    (HAQuery dir str) -> runQuery dir str
    (HADump dir)      -> dumpIndex dir
  return ()
