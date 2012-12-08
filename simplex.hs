module Main (main) where

import Prelude hiding (lex)

import Simplex.Parser
import Simplex.ToTeX

import System.Environment (getArgs)
import System.Console.GetOpt
import System.FilePath (takeBaseName)
import System.Cmd (rawSystem)
import System.Exit (ExitCode (..))
import System.Directory
import Control.Monad (liftM2)
import Control.Concurrent
import Data.Maybe

data Flag = Clean | OnlyTeX | Help | Poll Int
    deriving (Show, Eq)

opts = [
        Option "c" ["clean"]    (NoArg Clean)   "Clean up after building.",
        Option "T" ["only-tex"] (NoArg OnlyTeX) "Do not run `pdflatex`.",
        Option "h" ["help"]     (NoArg Help)    "Print this help text.",
        Option "p" ["poll"]     (OptArg (Poll . read . fromMaybe "2000000") "") ""
       ]

mkTeX = toTeX . parse . lex

proc file content = (takeBaseName file, mkTeX content)

write file content = writeFile (file ++ ".tex") content

pdflatex file = do
    putStrLn ("\n:=  Processing: " ++ file ++ ".tex\n")
    result <- rawSystem "pdflatex" ["-halt-on-error", file ++ ".tex"]
    case result of
                ExitSuccess -> rawSystem "pdflatex" ["-halt-on-error", file ++ ".tex"] >> putStrLn "\n=>  Success!"
                ExitFailure code -> putStrLn $ "\n=>  Fail (" ++ show code ++ ")"
    return result

report file result = putStrLn $ file ++ ": " ++ show result

endsWith s xs = take (length s') (reverse xs) == s'
    where s' = reverse s

tern a _ True = a
tern _ b _    = b

time simple =
    let pdf = takeBaseName simple ++ ".pdf"
        t1 = getModificationTime simple
        t2 = getModificationTime pdf
    in do
        ex <- doesFileExist pdf
        if ex then liftM2 (>) t1 t2 >>= (return . tern simple "") else return simple

work x@([Poll ms], [], _) = do
    dir <- getDirectoryContents "."
    dir' <- fmap (filter (/= "")) $ mapM time $ filter (endsWith ".simple") dir
    let filez = filter (endsWith ".simple") dir'
    case filez of
        [] -> threadDelay ms
        _ -> main' $ ("-c":) $ filez
    work x

work (_, [], _) = do
    dir <- getDirectoryContents "."
    dir' <- fmap (filter (/= "")) $ mapM time $ filter (endsWith ".simple") dir
    let filez = filter (endsWith ".simple") dir'
    case filez of
        [] -> return ()
        _ -> main' $ ("-c":) $ filez

work (optz, argz, _)
    | OnlyTeX `elem` optz = do
        filez <- mapM readFile argz
        let texz = zipWith proc argz filez
        _ <- mapM (uncurry write) texz
        return ()
    | Clean `elem` optz = doIt argz >> cleanUp argz
    | otherwise = doIt argz

doIt argz = do        
    filez <- mapM readFile argz
    let texz = zipWith proc argz filez
    _ <- mapM (uncurry write) texz
    resultz <- mapM (pdflatex . fst) texz
    putStrLn ""
    _ <- mapM (uncurry report) (zip argz resultz)
    return ()

removeIfExists file = do
    exists <- doesFileExist file
    if exists then removeFile file else return ()

clean file = mapM removeIfExists $ zipWith (++) (repeat file) [".toc", ".aux", ".log", ".tex", ".out"]

cleanUp argz = mapM (clean . takeBaseName) argz >> return ()

main' = work . getOpt Permute opts
main = getArgs >>= main'

