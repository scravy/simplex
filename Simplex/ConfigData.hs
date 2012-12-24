module Simplex.ConfigData (
        Config (..), defaultConfig
    ) where

data Config
 = Config {
    doNumberSections :: Bool,
    doSectionsCutColumns :: Bool,
    oColumns :: Int,
    oFigure :: Bool,
    oStandalone :: Bool
}

defaultConfig = Config {
    doNumberSections = False,
    doSectionsCutColumns = True,
    oColumns = 0,
    oFigure = False,
    oStandalone = False
}

