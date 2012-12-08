module Simplex.ConfigData (
        Config (..), defaultConfig
    ) where

data Config
 = Config {
    doNumberSections :: Bool,
    doSectionsCutColumns :: Bool,
    oColumns :: Int
}

defaultConfig = Config {
    doNumberSections = False,
    doSectionsCutColumns = True,
    oColumns = 0
}

