module Simplex.Specials (processSpecials) where

import Simplex.Config
import Simplex.ConfigData
import Simplex.CmdLineOpts
import Simplex.Parser
import Simplex.Util

import System.Directory
import System.Random

processSpecials :: Opts -> [Token] -> IO [Token]

processSpecials opts (TControl ".digraph" : TBlock b : xs) = do
    tex  <- mkDigraph b
    rest <- processSpecials opts xs
    return (TControl ".latex" : TBlock tex : rest)

processSpecials opts (TControl ".graph" : TBlock b : xs) = do
    tex  <- mkGraph b
    rest <- processSpecials opts xs
    return (TControl ".latex" : TBlock tex : rest)

processSpecials opts (TControl ".gnuplot" : TBlock b : xs) = do
    tex  <- mkGnuplot b
    rest <- processSpecials opts xs
    return (TControl ".latex" : TBlock tex : rest)

processSpecials opts (x : xs) = do
    rest <- processSpecials opts xs
    return $ x : rest

processSpecials opts [] = return []


randomString :: Int -> IO String
randomString 0 = return ""
randomString n = do
    char <- getStdRandom (randomR ('a', 'z'))
    str  <- randomString (n-1)
    return $ char : str

mkDigraph c = do
    file <- randomString 10

    writeFile (file ++ ".dot") ("digraph G {\n" ++ c ++ "\n}\n")
    
    exec False "dot" ["-Tpdf", file ++ ".dot", "-o" ++ file ++ ".pdf"]
    removeFile (file ++ ".dot")

    return $ "\\includegraphics{" ++ file ++ ".pdf}"

mkGraph c = do
    return ""

mkGnuplot c = do
    
    return ""


