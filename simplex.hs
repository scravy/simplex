module Main (main) where

import Prelude hiding (lex)

import Simplex.ConfigData
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
          | Density Int | Quality Int | Crop
          | Convert String | Force
    deriving (Show, Eq)

data Opts = Opts {
    optOnlyTeX  :: Bool,
    optHelp     :: Bool,
    optVerbose  :: Bool,
    optNoClean  :: Bool,
    optPrint    :: Bool,
    optDryRun   :: Bool,
    optCrop     :: Bool,
    optForce    :: Bool,
    optWatch    :: Maybe Int,
    optDensity  :: Int,
    optQuality  :: Int,
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
    optNoClean  = False,
    optPrint    = False,
    optDryRun   = False,
    optCrop     = False,
    optForce    = False,
    optWatch    = Nothing,
    optDensity  = 150,
    optQuality  = 90,
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
        Option "c" ["crop"]     (NoArg Crop)          "Crops the document so that no margins are left.",
        Option "f" ["force"]    (NoArg Force)         "Forces the creation of output files.",
        Option "t" ["type"]     (ReqArg Type      "") "Specify type of output (pdf, png, tex)",
        Option "x" ["pdflatex"] (ReqArg Pdflatex  "") "Path to `pdflatex' executable",
        Option "k" ["pdfcrop"]  (ReqArg Pdfcrop   "") "Path to `pdfcrop'",
        Option "z" ["graphviz"] (ReqArg Graphviz  "") "Path to `dot' (graphviz)",
        Option "g" ["gnuplot"]  (ReqArg Gnuplot   "") "Path to `gnuplot'",
        Option "m" ["convert"]  (ReqArg Convert   "") "Path to `convert' (ImageMagick)",
        Option "w" ["watch"]    (OptArg (Watch . read . fromMaybe "2000") "") "Watch files or folder (optionally amount of time in ms)",

        Option ""  ["density", "dpi"] (ReqArg (Density . read) "") "For output type `png' only, specifies dpi.",
        Option ""  ["quality"] (ReqArg (Quality . read) "") "For output type `png' only, specifies quality."
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
                Crop       -> opts { optCrop     = True }
                Force      -> opts { optForce    = True }
                Watch d    -> opts { optWatch    = Just d }
                Density d  -> opts { optDensity  = d }
                Quality q  -> opts { optQuality  = q }
                Type t     -> opts { optType     = t }
                Pdflatex c -> opts { optPdflatex = c }
                Pdfcrop  c -> opts { optPdfcrop  = c }
                Graphviz c -> opts { optGraphviz = c }
                Gnuplot  c -> opts { optGnuplot  = c }
                Convert  c -> opts { optConvert  = c }
        parseOpts opts _ = opts

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

simplex, simplex' :: Opts -> [String] -> IO ()

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

simplex' opts files = do
    mapM_ (flip runContT return . callCC . process opts) files

watch :: Opts -> FilePath -> Int -> ClockTime -> IO ()
-- | ^ Polls the list of changed files and runs @simplex'@ periodically
watch opts dir int time = do
    files <- gatherChangedFiles (optType opts) dir
    max <- if null files then return time else do
            let max = maximum $ map snd files
            simplex' opts $ map fst $ filter ((> time) . snd) files
            return max
    threadDelay (int * 1000)
    watch opts dir int max

loadHashbangs :: [Token] -> IO [Token]
-- | ^ Loads includes (those lines starting with a hashbang)
loadHashbangs (TControl ('#':c@(_:_)) : TBlock b : xs) = do
    (c', block) <- loadHashbang c b
    rest        <- loadHashbangs xs
    return $ TControl ('.':c') : TBlock block : rest

loadHashbangs (x:xs) = loadHashbangs xs >>= return . (x :)
loadHashbangs _ = return []

loadHashbang :: String -> String -> IO (String, String)
loadHashbang c b = do
    let f = reverse . dropWhile (`elem` " \t\n\r\"<>")
        trim = f . f
        file = trim b

    h <- openFile file ReadMode
           
    try (hGetContents h) >>= return . either
        (\e -> ("error", show (e :: IOException)))
        (\d -> (c, d))

loadIncludes :: [Token] -> IO [Token]
-- ^ load other simplex files included via `#include`
loadIncludes (TControl "#include" : TBlock b : xs) = do
    let f = reverse . dropWhile (`elem` " \t\n\r\"<>")
        trim = f . f
        file = trim b

    h <- openFile file ReadMode
    hSetEncoding h utf8

    tok <- try (hGetContents h) >>= either
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

    tok' <- liftIO $ loadIncludes (lex c) >>= loadHashbangs

    let cfg = defaultConfig { oStandalone = optType opts == "png" }
        tex = toTeX cfg (parse tok')
    print' "."

    unless (optDryRun opts) $ do
        -- write tex-file
        r <- liftIO $ try (writeFile (filename ++ ".tex") tex)
        _ <- either (throw . Exc) (return . const Ok) r
        print' "."

        -- run pdflatex
        r <- liftIO $ exec (optVerbose opts) pdflatex (pdfopts ++ [filename ++ ".tex"])
        _ <- either (throw . Err . snd) (return . const Ok) r
        print' "."

        -- run pdflatex a second time
        _ <- liftIO $ exec (optVerbose opts) pdflatex (pdfopts ++ [filename ++ ".tex"])
        print' "."

        -- clean files
        unless (optNoClean opts) (liftIO $ mapM_ removeIfExists (prepend dirtyExts))

        when (elem filetype ["png", "jpg", "gif"]) $ do
            print' "."
            when (optCrop opts) $ do
                r <- liftIO $ exec (optVerbose opts) pdfcrop [filename ++ ".pdf", filename ++ "-crop.pdf"]
                _ <- either (throw . Err . snd) (return . const Ok) r

                r <- liftIO $ try $ renameFile (filename ++ "-crop.pdf") (filename ++ ".pdf")
                _ <- either (throw . Exc) (return . const Ok) r

                print' "."

            r <- liftIO $ exec (optVerbose opts) convert ["-density", show $ optDensity opts, filename ++ ".pdf",
                                                          "-quality", show $ optQuality opts, filename ++ "." ++ filetype]
            _ <- either (throw . Err . snd) (return . const Ok) r
            return ()

    print' " OK\n"

    when (optPrint opts) $ do
        print tex

    return Ok

