module Main (main) where

import Prelude hiding (lex)

import Simplex.Parser
import Simplex.ToTeX
import Simplex.Util

import Text.Printf

import Data.Maybe

import System.Console.GetOpt
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

data Flag = Help | Verbose | Print | NoClean
          | Pdflatex String | Pdfcrop String
          | Graphviz String | Gnuplot String
          | Watch Int | DryRun | Type String
          | Convert String 
    deriving (Show, Eq)

data Opts = Opts {
    optOnlyTeX  :: Bool,
    optHelp     :: Bool,
    optVerbose  :: Bool,
    optNoClean  :: Bool,
    optPrint    :: Bool,
    optDryRun   :: Bool,
    optWatch    :: Maybe Int,
    optType     :: String,
    optPdflatex :: String,
    optPdfcrop  :: String,
    optGraphviz :: String,
    optGnuplot  :: String,
    optConvert  :: String
  }

defOpts = Opts {
    optOnlyTeX  = False,
    optHelp     = False,
    optVerbose  = False,
    optWatch    = Nothing,
    optNoClean  = False,
    optPrint    = False,
    optDryRun   = False,
    optType     = "pdf",
    optPdflatex = "pdflatex",
    optPdfcrop  = "pdfcrop",
    optGraphviz = "dot",
    optGnuplot  = "gnuplot",
    optConvert  = "convert"
  }

cmdOpts = [
        Option "h" ["help"]     (NoArg Help)          "Print this help text.",
        Option "v" ["verbose"]  (NoArg Verbose)       "Verbose output.",
        Option "d" ["dry-run"]  (NoArg DryRun)        "Dry run (do not create any files).",
        Option "n" ["no-clean"] (NoArg NoClean)       "Do not clean up after building.",
        Option "p" ["print"]    (NoArg Print)         "Print processed tex to stdout.",
        Option "t" ["type"]     (ReqArg Type      "") "Specify type of output (pdf, png, tex)",
        Option "x" ["pdflatex"] (ReqArg Pdflatex  "") "Path to `pdflatex' executable",
        Option "k" ["pdfcrop"]  (ReqArg Pdfcrop   "") "Path to `pdfcrop'",
        Option "z" ["graphviz"] (ReqArg Graphviz  "") "Path to `dot' (graphviz)",
        Option "g" ["gnuplot"]  (ReqArg Gnuplot   "") "Path to `gnuplot'",
        Option "m" ["convert"]  (ReqArg Convert   "") "Path to `convert' (ImageMagick)",
        Option "w" ["watch"]    (OptArg (Watch . read . fromMaybe "2000") "") "Watch files or folder (optionally amount of time in ms)"
       ]

parseArgs :: IO (Either (Opts, [String]) [String])
parseArgs = do
    (opts, args, errs) <- getArgs >>= return . getOpt Permute cmdOpts
    if null errs
        then return $ Left  (parseOpts defOpts opts, args)
        else return $ Right errs
    where
        parseOpts opts (x:xs) = flip parseOpts xs $ 
            case x of
                Help       -> opts { optHelp     = True }
                Print      -> opts { optPrint    = True }
                DryRun     -> opts { optDryRun   = True }
                NoClean    -> opts { optNoClean  = True }
                Verbose    -> opts { optVerbose  = True }
                Watch d    -> opts { optWatch    = Just d }
                Type t     -> opts { optType     = t }
                Pdflatex c -> opts { optPdflatex = c }
                Pdfcrop  c -> opts { optPdfcrop  = c }
                Graphviz c -> opts { optGraphviz = c }
                Gnuplot  c -> opts { optGnuplot  = c }
                Convert  c -> opts { optConvert  = c }
        parseOpts opts _ = opts

gatherChangedFiles ext dir = do
    files'  <- getDirectoryContents dir
    let files = filter ((".simple" ==) . takeExtension) files'
    mtimes1 <- mapM getModificationTime files
    mtimes2 <- mapM (getModificationTime' . flip addExtension ext . dropExtension) files
    let files' = zip3 files mtimes1 mtimes2
    return $ map (\(f, t, _) -> (f, t)) $ filter (\(_, t, t') -> t > t') files'

main = parseArgs >>= either (uncurry simplex) (mapM_ putStr)

simplex, simplex' :: Opts -> [String] -> IO ()
simplex opts files
    | optHelp opts = do
        cmd <- getProgName
        putStrLn (usageInfo (printf "%s [options] [files...]\n" cmd) cmdOpts)

    | isJust $ optWatch opts = do
        getClockTime >>= watch opts "." (fromJust $ optWatch opts)

    | null files = do
        files' <- gatherChangedFiles (optType opts) "."
        if null files' then putStrLn "All files up-to-date, nothing to do."
                       else simplex' opts $ map fst files'

    | otherwise = simplex' opts files

simplex' opts files = do
    mapM_ (flip runContT return . callCC . process opts) files

watch opts dir int time = do
    files <- gatherChangedFiles (optType opts) dir
    max <- if null files then return time else do
            let max = maximum $ map snd files
            simplex' opts $ map fst $ filter ((> time) . snd) files
            return max
    threadDelay (int * 1000)
    watch opts dir int max

data Result = Exc IOException
            | Str String
            | Err String
            | Ok
    deriving Show

process :: Opts -> FilePath
        -> (Result -> ContT Result IO Result)
        -> ContT Result IO Result
process opts file exit = do
    let filename = takeBaseName file
    let prepend  = zipWith (++) (repeat filename)
    let pdflatex = optPdflatex opts
    let pdfopts  = ["-interaction=nonstopmode", "-file-line-error"]

    let print x   = liftIO (putStr   x >> hFlush stdout)
    let println x = liftIO (putStrLn x >> hFlush stdout)
    let print' x  = unless (optPrint opts) (print x)
    let throw x   = println " Splat!" >> error x >> exit x
          where
            error (Exc e) = print "-> " >> println (show e)
            error (Str s) = print "-> " >> println s
            error (Err e)
                | length lns <= 2 = mapM_ (error . Str) (lines e)
                | otherwise       = mapM_ (error . Str) (filter isErr (lines e))
                    where lns = lines e
            isErr = (~= "^[^ ]+:[0-9]+: ")

    print' $ "Processing " ++ file

    f <- liftIO $ try (readFile file)
    (Str c) <- either (throw . Exc) (return . Str) f

    let tex = toTeX (parse (lex c))

    print' "."

    unless (optDryRun opts) $ do
        r <- liftIO $ try (writeFile (filename ++ ".tex") tex)
        _ <- either (throw . Exc) (return . const Ok) r

        print' "."

        r <- liftIO $ exec (optVerbose opts) pdflatex (pdfopts ++ [filename ++ ".tex"])
        r <- either (throw . Err . snd) (return . const Ok) r

        print' "."

        _ <- liftIO $ exec (optVerbose opts) pdflatex (pdfopts ++ [filename ++ ".tex"])

        print' "."

        unless (optNoClean opts) (liftIO $ mapM_ removeIfExists (prepend dirtyExts))

    print' " OK\n"

    when (optPrint opts) $ do
        print tex

    return Ok

