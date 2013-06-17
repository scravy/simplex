{-# LANGUAGE Haskell2010 #-}

module Simplex.EscapeTeX (
        escapeTeX, escapeTeX', safeTeX
    ) where

import Simplex.Config
import Simplex.Util

import Data.List (intersperse)
import Data.Char

known w
    | w `elem` knownSymbols = Just ("\\ensuremath{\\" ++ w ++ "}")
    | otherwise             = lookup w specialSymbols


escapeTeX :: String -> String -> String

escapeTeX t xs
    | a /= "" = a ++ escapeTeX t b
        where (a, b) = ensureTeX xs

escapeTeX t ('$':' ':xs) = '\\' : '$' : escapeTeX t xs
escapeTeX t ('$':xs) = let (m, ms) = break (== '$') xs
                       in '$' : (safeTeX m) ++ '$' : escapeTeX t (tail' ms)

escapeTeX t ('\\':' ':xs) = "\\textbackslash{}" ++ escapeTeX t xs

escapeTeX t ('\\':'^':'^':xs) = let (m, ms) = break (== '^') xs
                                    (n, ns) = break (== ' ') m
                                    url = safeTeX n
                                in  "\\footnote{" ++ (
                                        if null ns then "\\url{" ++ url ++ "}"
                                                   else "\\href{" ++ url ++ "}{\\texttt{" ++ url ++ "} -- " ++ escapeTeX "}" ns
                                        ) ++ '}' : escapeTeX t (tail' ms)

escapeTeX t ('\\':'^':xs) = let (m, ms) = break (== '^') xs
                            in  "\\footnote{" ++ escapeTeX "}" m ++ escapeTeX t (tail' ms)

escapeTeX t ('\\':'[':xs) = let (m, ms) = break (== ']') xs
                                (n, ns) = break (== ' ') m
                                url = safeTeX n
                            in  (if null ns then "\\url{" ++ url ++ "}"
                                            else "\\href{" ++ url ++ "}{" ++ escapeTeX "}" ns)
                                ++ escapeTeX t (tail' ms)

escapeTeX t ('\\':'{':xs) = let (m, ms) = break (== '}') xs
                                (n, ns) = break (== ' ') m
                                url = safeTeX n
                            in  (if null ns then "\\url{" ++ url ++ "}"
                                            else "\\href{" ++ url ++ "}{\\texttt{" ++ url ++ "} -- " ++ escapeTeX "}" ns)
                                ++ escapeTeX t (tail' ms)

escapeTeX t ('\\':'<':xs) = let (m, ms) = break (== '>') xs
                            in  "\\ref{" ++ m ++ "}" ++ escapeTeX t (tail' ms)

escapeTeX t ('\\':'(':xs) = let (m, ms) = break (== ')') xs
                            in  "\\pageref{" ++ m ++ "}" ++ escapeTeX t (tail' ms)

escapeTeX t ('\\':x:xs)
    | x `elem` verbs = let (m, ms) = break (== x) xs
                       in "\\verb" ++ x : m ++ x : escapeTeX t (tail' ms)

escapeTeX t ('_':' ':xs) = '\\' : '_' : escapeTeX t xs
escapeTeX t ('_':xs) = let (m, ms) = break (== '_') xs
                       in "\\underline{" ++ escapeTeX "" m ++ '}' : escapeTeX t (tail' ms)

escapeTeX t ('/':'/':' ':xs) = '/' : '/' : escapeTeX t xs
escapeTeX t ('/':'/':xs) = let (m, ms) = break (== '/') xs
                       in "\\textsc{" ++ escapeTeX "" m ++ '}' : escapeTeX t (tail'' ms)

escapeTeX t ('*':'*':' ':xs) = '*' : '*' : escapeTeX t xs
escapeTeX t ('*':'*':xs) = let (m, ms) = break (== '*') xs
                       in "\\textbf{" ++ escapeTeX "" m ++ '}' : escapeTeX t (tail'' ms)

escapeTeX t ('*':' ':xs) = '*' : escapeTeX t xs
escapeTeX t ('*':xs) = let (m, ms) = break (== '*') xs
                       in "\\emph{" ++ escapeTeX "" m ++ '}' : escapeTeX t (tail' ms)

escapeTeX t s@(x:xs)
    | x `elem` "{}%#&" = '\\' : x : escapeTeX t xs
    | x == '~' = "$\\sim$" ++ escapeTeX t xs
    | x == '|' = "\\textbar " ++ escapeTeX t xs
    | x == '^' = "\\^{}" ++ escapeTeX t xs
    | x == '\\' = "\\textbackslash " ++ escapeTeX t xs
    | x == '<' = "\\textless " ++ escapeTeX t xs
    | x == '>' = "\\textgreater " ++ escapeTeX t xs
    | x == '\n' = ' ' : escapeTeX t xs
    | otherwise = x : escapeTeX t xs
escapeTeX t [] = t

escapeTeX' :: String -> String -> String
escapeTeX' t = concat . (++ [t]) . intersperse "\\\\" . map (escapeTeX "") . lines


ensureTeX str = case str of

    ('\\':xs) -> let (w, ws) = break (not.isAlpha) xs
                      in maybe ("", xs) (\x -> (x, skipOneSpace ws)) (known w)

    ('≤':xs) -> ("\\ensuremath{\\leq}", xs)
    ('≥':xs) -> ("\\ensuremath{\\geq}", xs)

    ('.':'.':'.':xs) -> ("\\ensuremath{\\dots}", xs)

    ('(':'-':')':xs) -> ("\\ensuremath{\\ominus}", xs)
    ('(':'+':')':xs) -> ("\\ensuremath{\\oplus}", xs)
    ('(':'.':')':xs) -> ("\\ensuremath{\\odot}", xs)
    ('(':'x':')':xs) -> ("\\ensuremath{\\otimes}", xs)
    ('(':'/':')':xs) -> ("\\ensuremath{\\oslash}", xs)
    ('(':'*':')':xs) -> ("\\ensuremath{\\circledast}", xs)

    ('[':'-':']':xs) -> ("\\ensuremath{\\boxminus}", xs)
    ('[':'+':']':xs) -> ("\\ensuremath{\\boxplus}", xs)
    ('[':'.':']':xs) -> ("\\ensuremath{\\boxdot}", xs)
    ('[':'x':']':xs) -> ("\\ensuremath{\\boxtimes}", xs)

    ('|':'-':'>':xs) -> ("\\ensuremath{\\mapsto}", xs)
    ('|':'-':'-':'>':xs) -> ("\\ensuremath{\\longmapsto}", xs)
    ('|':'=':'>':xs) -> ("\\ensuremath{\\Mapsto}", xs)
    ('|':'=':'=':'>':xs) -> ("\\ensuremath{\\Longmapsto}", xs)

    ('<':'-':'|':xs) -> ("\\ensuremath{\\mapsfrom}", xs)
    ('<':'-':'-':'|':xs) -> ("\\ensuremath{\\longmapsfrom}", xs)
    ('<':'=':'|':xs) -> ("\\ensuremath{\\Mapsfrom}", xs)
    ('<':'=':'=':'|':xs) -> ("\\ensuremath{\\Longmapsfrom}", xs)

    ('_':'|':'_':xs) -> ("\\ensuremath{\\bot}", xs)
    ('|':'=':xs) -> ("\\ensuremath{\\models}", xs)

    ('<':'=':'>':xs) -> ("\\ensuremath{\\Leftrightarrow}", xs)
    ('<':'=':'=':'>':xs) -> ("\\ensuremath{\\Longleftrightarrow}", xs)
    ('=':'=':'>':xs) -> ("\\ensuremath{\\Longrightarrow}", xs)
    ('<':'=':'=':xs) -> ("\\ensuremath{\\Longleftarrow}", xs)
    ('=':'!':'>':xs) -> ("\\ensuremath{\\nRightarrow}", xs)
    ('<':'!':'=':xs) -> ("\\ensuremath{\\nLeftarrow}", xs)
    ('<':'!':'>':xs) -> ("\\ensuremath{\\nLeftrightarrow}", xs)
    ('-':'!':'>':xs) -> ("\\ensuremath{\\nrightarrow}", xs)
    ('<':'!':'-':xs) -> ("\\ensuremath{\\nleftarrow}", xs)
    ('=':'<':xs) -> ("\\ensuremath{\\leq}", xs)
    ('=':'>':xs) -> ("\\ensuremath{\\Rightarrow}", xs)
    ('>':'=':xs) -> ("\\ensuremath{\\geq}", xs)
    ('<':'=':xs) -> ("\\ensuremath{\\Leftarrow}", xs)

    ('>':'-':'>':xs) -> ("\\ensuremath{\\leftarrowtail}", xs)
    ('<':'-':'<':xs) -> ("\\ensuremath{\\rightarrowtail}", xs)
    ('~':'>':xs) -> ("\\ensuremath{\\leadsto}", xs)

    ('<':'-':'>':xs) -> ("\\ensuremath{\\leftrightarrow}", xs)
    ('<':'-':'-':'>':xs) -> ("\\ensuremath{\\longleftrightarrow}", xs)
    ('-':'-':'>':xs) -> ("\\ensuremath{\\longrightarrow}", xs)
    ('<':'-':'-':xs) -> ("\\ensuremath{\\longleftarrow}", xs)
    ('-':'>':xs) -> ("\\ensuremath{\\rightarrow}", xs)
    ('<':'-':xs) -> ("\\ensuremath{\\leftarrow}", xs)

    ('=':'=':'=':xs) -> ("\\ensuremath{\\equiv}", xs)
    ('!':'=':xs) -> ("\\ensuremath{\\neq}", xs)

    ('ä':xs) -> ("\\text{\\\"a}", xs)
    ('ö':xs) -> ("\\text{\\\"o}", xs)
    ('ü':xs) -> ("\\text{\\\"u}", xs)
    ('Ä':xs) -> ("\\text{\\\"A}", xs)
    ('Ö':xs) -> ("\\text{\\\"O}", xs)
    ('Ü':xs) -> ("\\text{\\\"U}", xs)
    ('ø':xs) -> ("\\text{\\o}", xs)
    ('Ø':xs) -> ("\\text{\\O}", xs)
    ('å':xs) -> ("\\text{\\aa}", xs)
    ('Å':xs) -> ("\\text{\\AA}", xs)
    ('æ':xs) -> ("\\text{\\ae}", xs)
    ('Æ':xs) -> ("\\text{\\AE}", xs)

    ('\913':xs) -> ("A", xs)
    ('\914':xs) -> ("B", xs)
    ('\915':xs) -> ("\\ensuremath{\\Gamma}", xs)
    ('\916':xs) -> ("\\ensuremath{\\Delta}", xs)
    ('\917':xs) -> ("E", xs)
    ('\918':xs) -> ("Z", xs)
    ('\919':xs) -> ("H", xs)
    ('\920':xs) -> ("\\ensuremath{\\Theta}", xs)
    ('\921':xs) -> ("I", xs)
    ('\922':xs) -> ("K", xs)
    ('\923':xs) -> ("\\ensuremath{\\Lambda}", xs)
    ('\924':xs) -> ("M", xs)
    ('\925':xs) -> ("N", xs)
    ('\926':xs) -> ("\\ensuremath{\\Xi}", xs)
    ('\927':xs) -> ("O", xs)
    ('\928':xs) -> ("\\ensuremath{\\Pi}", xs)
    ('\929':xs) -> ("P", xs)
-- 930 / 03A2 is reserved
    ('\931':xs) -> ("\\ensuremath{\\Sigma}", xs)
    ('\932':xs) -> ("T", xs)
    ('\933':xs) -> ("\\ensuremath{\\Upsilon}", xs)
    ('\934':xs) -> ("\\ensuremath{\\Phi}", xs)
    ('\935':xs) -> ("X", xs)
    ('\936':xs) -> ("\\ensuremath{\\Psi}", xs)
    ('\937':xs) -> ("\\ensuremath{\\Omega}", xs)

    ('\945':xs) -> ("\\ensuremath{\\alpha}", xs)
    ('\946':xs) -> ("\\ensuremath{\\beta}", xs)
    ('\947':xs) -> ("\\ensuremath{\\gamma}", xs)
    ('\948':xs) -> ("\\ensuremath{\\delta}", xs)
    ('\949':xs) -> ("\\ensuremath{\\epsilon}", xs)
    ('\950':xs) -> ("\\ensuremath{\\zeta}", xs)
    ('\951':xs) -> ("\\ensuremath{\\eta}", xs)
    ('\952':xs) -> ("\\ensuremath{\\theta}", xs)
    ('\953':xs) -> ("\\ensuremath{\\iota}", xs)
    ('\954':xs) -> ("\\ensuremath{\\kappa}", xs)
    ('\955':xs) -> ("\\ensuremath{\\lambda}", xs)
    ('\956':xs) -> ("\\ensuremath{\\mu}", xs)
    ('\957':xs) -> ("\\ensuremath{\\nu}", xs)
    ('\958':xs) -> ("\\ensuremath{\\xi}", xs)
    ('\959':xs) -> ("\\ensuremath{\\omicron}", xs)
    ('\960':xs) -> ("\\ensuremath{\\pi}", xs)
    ('\961':xs) -> ("\\ensuremath{\\rho}", xs)
    ('\963':xs) -> ("\\ensuremath{\\sigma}", xs)
    ('\964':xs) -> ("\\ensuremath{\\tau}", xs)
    ('\965':xs) -> ("\\ensuremath{\\upsilon}", xs)
    ('\966':xs) -> ("\\ensuremath{\\phi}", xs)
    ('\967':xs) -> ("\\ensuremath{\\chi}", xs)
    ('\968':xs) -> ("\\ensuremath{\\psi}", xs)
    ('\969':xs) -> ("\\ensuremath{\\omega}", xs)

    xs -> ("", xs)



safeTeX s@(x:xs)
    | a /= "" = a ++ safeTeX b
    | otherwise = x : safeTeX xs
        where (a, b) = ensureTeX s
safeTeX [] = []


