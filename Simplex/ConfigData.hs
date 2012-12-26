module Simplex.ConfigData (
        Config (..), defaultConfig
    ) where

data Config
 = Config {
    doNumberSections :: Bool,
    doSectionsCutColumns :: Bool,
    oColumns :: Int,
    oFigure :: Bool,
    oStandalone :: Bool,
    
    oImageWidth :: Maybe String,
    oImageHeight :: Maybe String,
    oImageScale :: Maybe String,
    oImageAngle :: Maybe String,
    oImagePage :: Maybe String
}

defaultConfig = Config {
    doNumberSections = False,
    doSectionsCutColumns = True,
    oColumns = 0,
    oFigure = False,
    oStandalone = False,
    
    oImageWidth = Nothing,
    oImageHeight = Nothing,
    oImageScale = Nothing,
    oImageAngle = Nothing,
    oImagePage = Nothing
}

