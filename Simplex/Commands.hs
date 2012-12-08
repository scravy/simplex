{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Simplex.Commands (
        Command (..), reset
    ) where

import Simplex.ConfigData

class Command a where
    (~>) :: String -> a -> (String, Config -> [String] -> (Config, String))

instance Command String where
    a ~> b = (a, \o _ -> (o, b))

instance Command (Config -> [String] -> (Config, String)) where
    a ~> f = (a, f)

instance Command ([String] -> String) where
    a ~> f = (a, \o x -> (o, f x))

reset :: Config -> [String] -> (Config, String)
reset opt _
    | oColumns opt > 0 = (opt {oColumns = 0}, "\\end{multicols}}{")
    | otherwise        = (opt, "}{")


