module Simplex.Specials (
        processSpecials,
        newSpec, Spec(..)
    ) where

import Simplex.Config
import Simplex.ConfigData
import Simplex.CmdLineOpts
import Simplex.Parser
import Simplex.Util

import System.Directory
import System.Random

import Data.Maybe
import Data.List
import Data.List.Split

data Spec = Spec {
        sRemoveFiles :: [String]
    }

newSpec = Spec {
        sRemoveFiles = []
    }

processSpecials :: Opts -> Spec -> Document -> IO (Spec, Document)

processSpecials o s (Document b m) = do
    (s', b') <- processSpecials' o s b
    return (s', Document b' m)

processSpecials' :: Opts -> Spec -> [Block] -> IO (Spec, [Block])

processSpecials' opts spec (BVerbatim "digraph" b : xs) = do
    (spec', pdf)   <- mkGraph "dot" "digraph" opts spec b
    (spec'', rest) <- processSpecials' opts spec' xs
    return (spec'', (if null pdf
                     then BVerbatim "error" "Graphviz .digraph failed"
                     else BCommand "image" [pdf]) : rest)

processSpecials' opts spec (BVerbatim "graph" b : xs) = do
    (spec', pdf)   <- mkGraph "neato" "graph" opts spec b
    (spec'', rest) <- processSpecials' opts spec' xs
    return (spec'', (if null pdf
                     then BVerbatim "error" "Graphviz .graph failed"
                     else BCommand "image" [pdf]) : rest)

processSpecials' opts spec (BVerbatim "neato" b : xs) = do
    (spec', pdf)   <- mkGraph "neato" "" opts spec b
    (spec'', rest) <- processSpecials' opts spec' xs
    return (spec'', (if null pdf
                     then BVerbatim "error" "Graphviz .neato failed"
                     else BCommand "image" [pdf]) : rest)

processSpecials' opts spec (BVerbatim "dot" b : xs) = do
    (spec', pdf)   <- mkGraph "dot" "" opts spec b
    (spec'', rest) <- processSpecials' opts spec' xs
    return (spec'', (if null pdf
                     then BVerbatim "error" "Graphviz .dot failed"
                     else BCommand "image" [pdf]) : rest)

processSpecials' opts spec (x : xs) = do
    (spec', rest) <- processSpecials' opts spec xs
    return (spec', x : rest)

processSpecials' _ spec [] = return (spec, [])


randomString :: Int -> IO String
randomString 0 = return ""
randomString n = do
    char <- getStdRandom (randomR ('a', 'z'))
    str  <- randomString (n-1)
    return $ char : str

mkGraph e g opts spec c = do
    file <- randomString 10

    let spec' = spec { sRemoveFiles = (file ++ ".pdf") : (file ++ ".dot") : sRemoveFiles spec }
    writeFile (file ++ ".dot") (if null g then c else g ++ " G {\n" ++ c ++ "\n}\n")
    
    r <- exec (optVerbose opts) (optGraphviz opts) ["-Tpdf", "-K" ++ e, file ++ ".dot", "-o" ++ file ++ ".pdf"]
    return (spec', (either (const "") (const $ file ++ ".pdf") r))

