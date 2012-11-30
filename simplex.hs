module Main (main) where

import Prelude hiding (lex)

import System.Environment
import System.Console.GetOpt
import Simplex.Parser
import Simplex.ToTeX
import System.FilePath (takeBaseName)
import System.IO
import System.Cmd (rawSystem)
import System.Exit (ExitCode (..))
import System.Directory
import Control.Monad

data Flag = Clean | OnlyTeX | Help
    deriving (Show, Eq)

opts = [
        Option "c" ["clean"]    (NoArg Clean)   "Clean up after building.",
        Option "T" ["only-tex"] (NoArg OnlyTeX) "Do not run `pdflatex`.",
        Option "h" ["help"]     (NoArg Help)    "Print this help text."
       ]

mkTeX = toTeX . parse . lex

proc file content = (takeBaseName file, mkTeX content)

write file content = writeFile (file ++ ".tex") content

pdflatex file = do
    putStrLn ("\n:=  Processing: " ++ file ++ ".tex\n")
    result <- rawSystem "pdflatex" ["-halt-on-error", file ++ ".tex"]
    putStrLn $ case result of
                ExitSuccess -> "\n=>  Success!"
                ExitFailure code -> "\n=>  Fail (" ++ show code ++ ")"
    return result

report file result = putStrLn $ file ++ ": " ++ show result

endsWith s xs = take (length s') (reverse xs) == s'
    where s' = reverse s

time simple =
    let pdf = takeBaseName simple ++ ".pdf"
        t1 = getModificationTime simple
        t2 = getModificationTime pdf
    in do
        ex <- doesFileExist pdf
        if ex then liftM2 (>) t1 t2 >>= (\x -> if x then return simple else return "") else return simple

work (_, [], _) = do
    dir <- getDirectoryContents "."
    dir' <- fmap (filter (/= "")) $ mapM time $ filter (endsWith ".simple") dir
    let filez = filter (endsWith ".simple") dir'
    case filez of
        [] -> putStrLn "* nothing to do"
        _ -> main' $ ("-c":) $ filez

work (optz, argz, _)
    | OnlyTeX `elem` optz = do
        filez <- mapM readFile argz
        let texz = zipWith proc argz filez
        mapM (uncurry write) texz
        return ()
    | Clean `elem` optz = doIt argz >> cleanUp argz
    | otherwise = doIt argz

doIt argz = do        
    filez <- mapM readFile argz
    let texz = zipWith proc argz filez
    mapM (uncurry write) texz
    resultz <- mapM (pdflatex . fst) texz
    putStrLn ""
    mapM (uncurry report) (zip argz resultz)
    return ()

clean file = mapM removeFile $ zipWith (++) (repeat file) [".aux", ".log", ".tex"] 

cleanUp argz = mapM (clean . takeBaseName) argz >> return ()

main' = work . getOpt Permute opts
main = getArgs >>= main'

