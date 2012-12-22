{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Simplex.Util (
    when, ifElse, tail', tail'',
    skipOneSpace, (~=), (~~), units,
    exec
    ) where

import Text.Regex
import Data.Maybe

import Control.Exception
import System.Exit
import System.Process

-- common tex units
units :: [String]
units = ["pt", "mm", "cm", "in", "ex", "em",
         "bp", "pc", "dd", "cc", "sp"]

-- regex operators

(~=) :: String -> String -> Bool
s ~= rx = isJust (matchRegex (mkRegex rx) s)

(~~) :: String -> String -> Maybe [String]
s ~~ rx = matchRegex (mkRegex rx) s

-- controll structures

when :: Bool -> [a] -> [a]
when True x = x
when False _ = []

ifElse :: Bool -> a -> a -> a
ifElse True x _ = x
ifElse _    _ y = y

-- convenience functions for list processing

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

tail'' :: [a] -> [a]
tail'' [] = []
tail'' xs = tail' $ tail' xs

skipOneSpace :: String -> String
skipOneSpace (' ':xs) = xs
skipOneSpace s = s

-- process execution

class Exec a where
    exec :: a


instance Exec (String -> IO String) where
    exec cmd = exec' cmd [] "" const (\_ -> const) id

instance Exec (String -> String -> IO String) where
    exec cmd stdin = exec' cmd [] stdin const (\_ -> const) id

instance Exec (String -> [String] -> IO String) where
    exec cmd args = exec' cmd args "" const (\_ -> const) id

instance Exec (String -> [String] -> String -> IO String) where
    exec cmd args stdin = exec' cmd args stdin const (\_ -> const) id


instance Exec (String -> IO (String, String)) where
    exec cmd = exec' cmd [] "" (,) (\_ -> (,)) ((,) "")

instance Exec (String -> String -> IO (String, String)) where
    exec cmd stdin = exec' cmd [] stdin (,) (\_ -> (,)) ((,) "")

instance Exec (String -> [String] -> IO (String, String)) where
    exec cmd args = exec' cmd args "" (,) (\_ -> (,)) ((,) "")

instance Exec (String -> [String] -> String -> IO (String, String)) where
    exec cmd args stdin = exec' cmd args stdin (,) (\_ -> (,)) ((,) "")



exec' :: String -> [String] -> String
         -> (String -> String -> a)
         -> (Int -> String -> String -> a)
         -> (String -> a)
         -> IO a
exec' cmd args stdin successHandler failureHandler errorHandler = do
    result <- try $ readProcessWithExitCode cmd args stdin :: IO (Either IOException (ExitCode, String, String))
    return $ either (errorHandler . show) handle result
        where
            handle (ExitFailure 127, stdout, stderr) = errorHandler "no such command"
            handle (ExitFailure exitCode, stdout, stderr) = failureHandler exitCode stdout stderr
            handle (ExitSuccess, stdout, stderr) = successHandler stdout stderr
        



