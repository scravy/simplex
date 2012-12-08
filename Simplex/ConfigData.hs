module Simplex.ConfigData where

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

