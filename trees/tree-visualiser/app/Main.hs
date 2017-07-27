module Main where

import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment

import Data.List (nub)

import Trees
import Visualisation

data Flag = Version
          | Help
            deriving (Eq)

options :: [OptDescr Flag]
options = [Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",

           Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

usageHeader :: String
usageHeader = "Usage: tree-visualiser [OPTIONS...]"

versionHeader :: String
versionHeader = "Tree visualiser v1.0 (c) Maciej Bendkowski 2017"

parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute options argv of
               (ops, nonops, [])
                    | Help `elem` ops -> do
                        putStr $ usageInfo usageHeader options
                        exitSuccess
                    | Version `elem` ops -> do
                        putStrLn versionHeader
                        exitSuccess
                    | otherwise -> return (nub (concatMap mkset ops), fs)
                        where
                            fs = if null nonops then [] else nonops
                            mkset x = [x]
               (_, _, errs) -> do
                    hPutStr stderr (concat errs ++ usageInfo usageHeader options)
                    exitWith (ExitFailure 1)

run :: [Flag]
    -> String
    -> IO ()

run flags f = do
    t <- parseTree f
    case t of
      Left err -> printError err
      Right t' -> do
        printFrequencies t'
        d <- toDotRepresentation t'
        putStrLn d

main :: IO ()
main = do
    (ops, fs) <- getArgs >>= parse
    case fs of
      []     -> exitSuccess
      (f:_)  -> do run ops f
                   exitSuccess
