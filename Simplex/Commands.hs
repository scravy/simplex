{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Simplex.Commands (
        Command (..), oneArg,
        reset, image
    ) where

import Simplex.Util
import Simplex.ConfigData

import Data.Maybe
import Data.List

class Command a where
    (~>) :: String -> a -> (String, Config -> [String] -> (Config, String))

instance Command String where
    a ~> b = (a, \o _ -> (o, b))

instance Command (Config -> [String] -> (Config, String)) where
    a ~> f = (a, f)

instance Command ([String] -> String) where
    a ~> f = (a, \o x -> (o, f x))

oneArg :: (String -> String) -> Config -> [String] -> (Config, String)
oneArg f o (x:xs) = (o, f x)
oneArg f o [] = (o, "")

reset :: Config -> [String] -> (Config, String)
reset opt _
    | oColumns opt > 0 = (opt {oColumns = 0}, "\\end{multicols}}{")
    | otherwise        = (opt, "}{")

data ImageOpt = Angle Double
              | File String
              | Width Double
              | Scale Double
    deriving (Show, Eq)

imageOpt [] = []
imageOpt (x:xs)
    | isJust angle = Angle (read ((fromJust angle) !! 0)) : imageOpt xs
    | isJust width = Width (read ((fromJust width) !! 0)) : imageOpt xs
    | isJust scale = Scale (read ((fromJust scale) !! 0)) : imageOpt xs
    | otherwise = File x : imageOpt xs
        where
            angle = x ~~ "^(\\-?[0-9]+(\\.[0-9]+)?)deg$"
            width = x ~~ "^(\\-?[0-9]+(\\.[0-9]+)?)%$"
            scale = x ~~ "^(\\-?[0-9]+(\\.[0-9]+)?)$"
            dim   = x ~~ ("^(\\-?[0-9]+(\\.[0-9]+)?)" ++ concat (intersperse "|" units) ++ "$")

image :: [String] -> String
image = show . imageOpt 
