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

data Spec = Spec {
        sRemoveFiles :: [String]
    }

newSpec = Spec {
        sRemoveFiles = []
    }

processSpecials :: Opts -> Spec -> [Token] -> IO (Spec, [Token])

processSpecials opts spec (TControl ".digraph" : TBlock b : xs) = do
    (spec', pdf)   <- mkGraph "digraph" opts spec b
    (spec'', rest) <- processSpecials opts spec' xs
    return (spec'', TCommand "image" [pdf] : rest)

processSpecials opts spec (TControl ".graph" : TBlock b : xs) = do
    (spec', pdf)   <- mkGraph "graph" opts spec b
    (spec'', rest) <- processSpecials opts spec' xs
    return (spec'', TCommand "image" [pdf] : rest)

processSpecials opts spec (x : xs) = do
    (spec', rest) <- processSpecials opts spec xs
    return (spec', x : rest)

processSpecials _ spec [] = return (spec, [])


randomString :: Int -> IO String
randomString 0 = return ""
randomString n = do
    char <- getStdRandom (randomR ('a', 'z'))
    str  <- randomString (n-1)
    return $ char : str

mkGraph g opts spec c = do
    file <- randomString 10

    let spec' = spec { sRemoveFiles = (file ++ ".pdf") : (file ++ ".dot") : sRemoveFiles spec }
    writeFile (file ++ ".dot") (g ++ " G {\n" ++ c ++ "\n}\n")
    
    exec False "dot" ["-Tpdf", "-Kdot", file ++ ".dot", "-o" ++ file ++ ".pdf"]

    return (spec', file ++ ".pdf")


