{-# LANGUAGE DeriveGeneric, DefaultSignatures, TupleSections #-}

module Main where

import qualified Options.Applicative as O
import qualified System.Directory as D
import qualified Data.ByteString as B
import Control.Applicative ((<|>))
import Control.Monad (liftM, foldM)
import Data.List (isSuffixOf, group, sort)
import Data.Serialize
import GHC.Generics
import Data.Monoid ((<>))
import Data.Char (isAlphaNum, toLower)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(ReadMode))

data HACommand = HABuild FilePath
               | HAQuery String
               | HADump FilePath

haCommand :: O.Parser HACommand
haCommand = haBuild <|> haQuery <|> haDump
  where haBuild = HABuild <$>
          O.strOption (O.long "build"
                        <> O.metavar "DIR"
                        <> O.help "Build index for speficied directory")
        haQuery = HAQuery <$>
          O.strOption (O.short 'q'
                        <> O.long "query"
                        <> O.metavar "QUERY"
                        <> O.help "Query index with a boolean expression")
        haDump = HADump <$>
          O.strOption (O.long "dump"
                        <> O.metavar "DIR"
                        <> O.help "Dump index build in specified directory")

parseHACommand :: IO HACommand
parseHACommand = O.execParser $ O.info (O.helper <*> haCommand) O.fullDesc

normTokens :: String -> [String]
normTokens = map head . group . sort
  . filter (not . null)
  . map (map toLower . filter isAlphaNum)
  . words

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

dumpIndex :: FilePath -> IO ()
dumpIndex dir = do
  bytes <- B.readFile (dir </> "haindex")
  case decode bytes of
    (Left str) -> putStrLn $! "Error: " ++ str
    (Right (HAIndex ii ss)) -> mapM_ writeEntry ii >> mapM_ listFile (zip ss [1..])
  where
   writeEntry (s,is) = putStrLn $! s ++ ": " ++ show is
   listFile   (f,i)  = putStrLn $! show i ++ ": " ++ (dir </> f)

runQuery :: String -> IO ()
runQuery = undefined

main :: IO ()
main = do
  cmd <- parseHACommand
  case cmd of
    (HABuild dir) -> buildIndex dir
    (HAQuery str) -> runQuery str
    (HADump dir)  -> dumpIndex dir
  return ()
