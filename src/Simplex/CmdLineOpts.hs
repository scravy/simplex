{-# LANGUAGE Haskell2010 #-}

module Simplex.CmdLineOpts (
        Opts (..), Flag (..), defOpts, cmdOpts,
        parseArgs
    ) where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)

import Data.Maybe

data Flag = Help | Verbose | Print | NoClean
          | Pdflatex String | Pdfcrop String
          | Graphviz String | Gnuplot String
          | Watch Int | DryRun | Type String
          | Density Int | Quality Int | Crop
          | Convert String | Force | Version
          | ListSymbols String | ThreeTimes
    deriving (Show, Eq)

data Opts = Opts {
    optOnlyTeX  :: Bool,
    optHelp     :: Bool,
    optVersion  :: Bool,
    optVerbose  :: Bool,
    optNoClean  :: Bool,
    optPrint    :: Bool,
    optDryRun   :: Bool,
    optCrop     :: Bool,
    optForce    :: Bool,
    optThreeTimes :: Bool,
    optListSymbols :: Maybe String,
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
    optVersion  = False,
    optVerbose  = False,
    optNoClean  = False,
    optPrint    = False,
    optDryRun   = False,
    optCrop     = False,
    optForce    = False,
    optThreeTimes = False,
    optListSymbols = Nothing,
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
        Option "V" ["version"]  (NoArg Version)       "Print version information.",
        Option "v" ["verbose"]  (NoArg Verbose)       "Verbose output.",
        Option "d" ["dry-run"]  (NoArg DryRun)        "Dry run (do not create any files).",
        Option "n" ["no-clean"] (NoArg NoClean)       "Do not clean up after building.",
        Option "p" ["print"]    (NoArg Print)         "Print processed tex to stdout.",
        Option "c" ["crop"]     (NoArg Crop)          "Crops the document so that no margins are left.",
        Option "f" ["force"]    (NoArg Force)         "Forces the creation of output files.",
        Option "tT" ["type"]    (ReqArg Type      "") "Specify type of output (pdf, png, tex)",
        Option "x" ["pdflatex"] (ReqArg Pdflatex  "") "Path to `pdflatex' executable",
        Option "k" ["pdfcrop"]  (ReqArg Pdfcrop   "") "Path to `pdfcrop'",
        Option "z" ["graphviz"] (ReqArg Graphviz  "") "Path to `dot' (graphviz)",
        Option "g" ["gnuplot"]  (ReqArg Gnuplot   "") "Path to `gnuplot'",
        Option "m" ["convert"]  (ReqArg Convert   "") "Path to `convert' (ImageMagick)",
        Option "w" ["watch"]    (OptArg (Watch . read . fromMaybe "2000") "") "Watch files or folder (optionally amount of time in ms)",
        Option "3" ["three-times"] (NoArg ThreeTimes) "Execute `pdflatex' three times instead of the default two times.",
        Option "s" ["symbols"]  (OptArg (ListSymbols . fromMaybe "\\%s\n") "") "Show a list of symboles known to simplex.",

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
                Version    -> opts { optVersion  = True }
                DryRun     -> opts { optDryRun   = True }
                NoClean    -> opts { optNoClean  = True }
                Verbose    -> opts { optVerbose  = True }
                Crop       -> opts { optCrop     = True }
                Force      -> opts { optForce    = True }
                ThreeTimes -> opts { optThreeTimes = True }
                Watch d    -> opts { optWatch    = Just d }
                ListSymbols f -> opts { optListSymbols = Just f }
                Density d  -> opts { optDensity  = d }
                Quality q  -> opts { optQuality  = q }
                Type t     -> opts { optType     = t }
                Pdflatex c -> opts { optPdflatex = c }
                Pdfcrop  c -> opts { optPdfcrop  = c }
                Graphviz c -> opts { optGraphviz = c }
                Gnuplot  c -> opts { optGnuplot  = c }
                Convert  c -> opts { optConvert  = c }
        parseOpts opts _ = opts


