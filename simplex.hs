module Main (main) where

import Prelude hiding (lex)

import Simplex.ConfigData
import Simplex.CmdLineOpts
import Simplex.Parser
import Simplex.Specials
import Simplex.ToTeX
import Simplex.Util

import Text.Printf

import Data.Maybe

import System.Console.GetOpt (usageInfo)
import System.Directory
import System.FilePath
import System.Environment (getArgs, getProgName)
import System.IO
import System.Time

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Cont


dirtyExts = [".toc", ".aux", ".log", ".tex", ".out"]

gatherChangedFiles :: String -> FilePath -> IO [(FilePath, ClockTime)]
gatherChangedFiles ext dir = do
    files'  <- getDirectoryContents dir
    let files = filter ((".simple" ==) . takeExtension) files'
    mtimes1 <- mapM getModificationTime files
    mtimes2 <- mapM (getModificationTime' . flip addExtension ext . dropExtension) files
    let files' = zip3 files mtimes1 mtimes2
    return $ map (\(f, t, _) -> (f, t)) $ filter (\(_, t, t') -> t > t') files'


main :: IO ()
main = parseArgs >>= either (uncurry simplex) (mapM_ putStr)


simplex :: Opts -> [String] -> IO ()
-- ^ The "start up sequence" of the simplex tool
simplex opts files
    | optHelp opts = do
        cmd <- getProgName
        putStrLn (usageInfo (printf "%s [options] [files...]\n" cmd) cmdOpts)

    | isJust $ optWatch opts = do
        getClockTime >>= watch opts "." (fromJust $ optWatch opts)

    | null files && optForce opts = do
        files' <- getDirectoryContents "."
        simplex' opts $ filter ((".simple" ==) . takeExtension) files'

    | null files = do
        files' <- gatherChangedFiles (optType opts) "."
        if null files' then putStrLn "All files up-to-date, nothing to do."
                       else simplex' opts $ map fst files'

    | otherwise = simplex' opts files


simplex' :: Opts -> [String] -> IO ()
-- ^ This part delegates the list of files to @process@
simplex' opts files = do
    mapM_ (flip runContT return . callCC . process opts) files


watch :: Opts -> FilePath -> Int -> ClockTime -> IO ()
-- ^ Polls the list of changed files and runs @simplex'@ periodically
watch opts dir int time = do
    files <- gatherChangedFiles (optType opts) dir
    max <- if null files then return time else do
            let max = maximum $ map snd files
            simplex' opts $ map fst $ filter ((> time) . snd) files
            return max
    threadDelay (int * 1000)
    watch opts dir int max


loadHashbangs :: [Token] -> IO [Token]
-- ^ Loads includes (those lines starting with a hashbang)
loadHashbangs (TControl ('#':c@(_:_)) : TBlock b : xs) = do
    (c', block) <- loadHashbang c b
    rest        <- loadHashbangs xs
    return $ TControl ('.':c') : TBlock block : rest

loadHashbangs (x:xs) = loadHashbangs xs >>= return . (x :)
loadHashbangs _ = return []


loadHashbang :: String -> String -> IO (String, String)
-- ^ Loads a single hashbang reference
loadHashbang c b = do
    let f = reverse . dropWhile (`elem` " \t\n\r\"<>")
        trim = f . f
        file = trim b

    try (readFile file) >>= return . either
        (\e -> ("error", show (e :: IOException)))
        (\d -> (c, d))


loadIncludes :: [Token] -> IO [Token]
-- ^ load other simplex files included via `#include`
loadIncludes (TControl "#include" : TBlock b : xs) = do
    let f = reverse . dropWhile (`elem` " \t\n\r\"<>")
        trim = f . f
        file = trim b

    tok <- try (readFile file) >>= either
        (\e -> return [TControl ".error", TBlock $ show (e :: IOException)])
        (loadIncludes . lex)
    rest <- loadIncludes xs
    return $ tok ++ rest

loadIncludes (x:xs) = loadIncludes xs >>= return . (x :)
loadIncludes _ = return []


data Result = Exc IOException
            | Str String
            | Err String
            | Ok
    deriving Show


process :: Opts -> FilePath
        -> (Result -> ContT Result IO Result)
        -> ContT Result IO Result
-- | ^ Does the actual processing of a simplex file.
process opts file exit = do

    let filename = takeBaseName file
        prepend  = zipWith (++) (repeat filename)
        filetype = optType opts
        verbose  = optVerbose opts
        pdflatex = optPdflatex opts
        pdfcrop  = optPdfcrop opts
        convert  = optConvert opts

        pdfopts  = ["-interaction=nonstopmode", "-file-line-error"]

        print x   = liftIO (putStr   x >> hFlush stdout)
        println x = liftIO (putStrLn x >> hFlush stdout)
        print' x  = unless (optPrint opts) (print x)
        throw x   = println " Splat!" >> error x >> exit x
          where
            error (Exc e) = print "-> " >> println (show e)
            error (Str s) = print "-> " >> println s
            error (Err e)
                | length lns <= 2 = mapM_ (error . Str) (lines e)
                | otherwise       = mapM_ (error . Str) (filter isErr (lines e))
                    where lns = lines e
            isErr = (~= "^[^ ]+:[0-9]+: ")

    print' $ "Processing " ++ file

    h <- liftIO $ openFile file ReadMode
    liftIO $ hSetEncoding h utf8

    f <- liftIO $ try (hGetContents h)
    (Str c) <- either (throw . Exc) (return . Str) f

    tok <- liftIO $ loadIncludes (lex c) >>= loadHashbangs

    print "."
    let cfg = defaultConfig { oStandalone = optType opts == "png" }

    tok' <- liftIO $ processSpecials opts tok

    let tex = toTeX cfg (parse tok')
    print' "."

    unless (optDryRun opts) $ do
        -- write tex-file
        r <- liftIO $ try (writeFile (filename ++ ".tex") tex)
        _ <- either (throw . Exc) (return . const Ok) r
        print' "."

        -- run pdflatex
        r <- liftIO $ exec verbose pdflatex (pdfopts ++ [filename ++ ".tex"])
        _ <- either (throw . Err . snd) (return . const Ok) r
        print' "."

        -- run pdflatex a second time
        _ <- liftIO $ exec verbose pdflatex (pdfopts ++ [filename ++ ".tex"])
        print' "."

        -- clean files
        unless (optNoClean opts) (liftIO $ mapM_ removeIfExists (prepend dirtyExts))

        when (elem filetype ["png", "jpg", "gif"]) $ do
            print' "."
            when (optCrop opts) $ do
                r <- liftIO $ exec verbose pdfcrop [filename ++ ".pdf", filename ++ "-crop.pdf"]
                _ <- either (throw . Err . snd) (return . const Ok) r

                r <- liftIO $ try $ renameFile (filename ++ "-crop.pdf") (filename ++ ".pdf")
                _ <- either (throw . Exc) (return . const Ok) r

                print' "."

            r <- liftIO $ exec verbose convert ["-density", show $ optDensity opts, filename ++ ".pdf",
                                                "-quality", show $ optQuality opts, filename ++ "." ++ filetype]
            _ <- either (throw . Err . snd) (return . const Ok) r
            return ()

    print' " OK\n"

    when (optPrint opts) $ do
        print tex

    return Ok

