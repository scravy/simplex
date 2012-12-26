{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Simplex.Commands (
        Command (..), reset, image
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

instance Command (Config -> String -> (Config, String)) where
    a ~> f = (a, \o x -> if null x then (o, "") else f o (head x))

instance Command (Config -> String -> Config) where
    a ~> f = (a, \o x -> if null x then (o, "") else (f o (head x), ""))

instance Command (Config -> String -> String -> Config) where
    a ~> f = (a, \o x -> if length x < 2 then (o, "") else (f o (x !! 0) (x !! 1), ""))

instance Command (String -> String) where
    a ~> f = (a, \o x -> (o, if null x then "" else f (head x)))

instance Command (Config -> Config) where
    a ~> f = (a, \o _ -> (f o, ""))

instance Command ([String] -> String) where
    a ~> f = (a, \o x -> (o, f x))

reset :: Config -> [String] -> (Config, String)
reset opt _
    | oColumns opt > 0 = (opt {oColumns = 0}, "\\end{multicols}}{")
    | otherwise        = (opt, "}{")

image :: Config -> [String] -> (Config, String)
image opt (x:xs) = (opt, "\\includegraphics" ++ getOpts ++ "{" ++ x ++ "}\n")
    where
        getOpts = if null opts' then ""
                  else "[" ++ (concat $ intersperse "," $ map (\(a,b) -> a ++ "=" ++ b) $ opts') ++ "]"
        opts' = opts 1
        opts 1 = if isJust (oImageWidth opt)
                 then ("width", fromJust (oImageWidth opt)) : opts 2
                 else opts 2
        opts 2 = if isJust (oImageHeight opt)
                 then ("height", fromJust (oImageHeight opt)) : opts 3
                 else opts 3
        opts 3 = if isJust (oImageAngle opt)
                 then ("angle", fromJust (oImageAngle opt)) : opts 4
                 else opts 4
        opts 4 = if isJust (oImageScale opt)
                 then ("scale", fromJust (oImageScale opt)) : opts 5
                 else opts 5
        opts 5 = if isJust (oImagePage opt)
                 then ("page", fromJust (oImagePage opt)) : opts 6
                 else opts 6
        opts _ = []
image opt _ = (opt, "")


