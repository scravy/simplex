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

ensureTeX ('\\':xs) = let (w, ws) = break (not.isAlpha) xs
                      in maybe ("", xs) (\x -> (x, skipOneSpace ws)) (known w)

ensureTeX ('≤':xs) = ("\\ensuremath{\\leq}", xs)
ensureTeX ('≥':xs) = ("\\ensuremath{\\geq}", xs)

ensureTeX ('.':'.':'.':xs) = ("\\ensuremath{\\dots}", xs)

ensureTeX ('(':'-':')':xs) = ("\\ensuremath{\\ominus}", xs)
ensureTeX ('(':'+':')':xs) = ("\\ensuremath{\\oplus}", xs)
ensureTeX ('(':'.':')':xs) = ("\\ensuremath{\\odot}", xs)
ensureTeX ('(':'x':')':xs) = ("\\ensuremath{\\otimes}", xs)
ensureTeX ('(':'/':')':xs) = ("\\ensuremath{\\oslash}", xs)
ensureTeX ('(':'*':')':xs) = ("\\ensuremath{\\circledast}", xs)

ensureTeX ('[':'-':']':xs) = ("\\ensuremath{\\boxminus}", xs)
ensureTeX ('[':'+':']':xs) = ("\\ensuremath{\\boxplus}", xs)
ensureTeX ('[':'.':']':xs) = ("\\ensuremath{\\boxdot}", xs)
ensureTeX ('[':'x':']':xs) = ("\\ensuremath{\\boxtimes}", xs)

ensureTeX ('|':'-':'>':xs) = ("\\ensuremath{\\mapsto}", xs)
ensureTeX ('|':'-':'-':'>':xs) = ("\\ensuremath{\\longmapsto}", xs)
ensureTeX ('|':'=':'>':xs) = ("\\ensuremath{\\Mapsto}", xs)
ensureTeX ('|':'=':'=':'>':xs) = ("\\ensuremath{\\Longmapsto}", xs)

ensureTeX ('<':'-':'|':xs) = ("\\ensuremath{\\mapsfrom}", xs)
ensureTeX ('<':'-':'-':'|':xs) = ("\\ensuremath{\\longmapsfrom}", xs)
ensureTeX ('<':'=':'|':xs) = ("\\ensuremath{\\Mapsfrom}", xs)
ensureTeX ('<':'=':'=':'|':xs) = ("\\ensuremath{\\Longmapsfrom}", xs)

ensureTeX ('_':'|':'_':xs) = ("\\ensuremath{\\bot}", xs)
ensureTeX ('|':'=':xs) = ("\\ensuremath{\\models}", xs)

ensureTeX ('<':'=':'>':xs) = ("\\ensuremath{\\Leftrightarrow}", xs)
ensureTeX ('<':'=':'=':'>':xs) = ("\\ensuremath{\\Longleftrightarrow}", xs)
ensureTeX ('=':'=':'>':xs) = ("\\ensuremath{\\Longrightarrow}", xs)
ensureTeX ('<':'=':'=':xs) = ("\\ensuremath{\\Longleftarrow}", xs)
ensureTeX ('=':'!':'>':xs) = ("\\ensuremath{\\nRightarrow}", xs)
ensureTeX ('<':'!':'=':xs) = ("\\ensuremath{\\nLeftarrow}", xs)
ensureTeX ('<':'!':'>':xs) = ("\\ensuremath{\\nLeftrightarrow}", xs)
ensureTeX ('-':'!':'>':xs) = ("\\ensuremath{\\nrightarrow}", xs)
ensureTeX ('<':'!':'-':xs) = ("\\ensuremath{\\nleftarrow}", xs)
ensureTeX ('=':'<':xs) = ("\\ensuremath{\\leq}", xs)
ensureTeX ('=':'>':xs) = ("\\ensuremath{\\Rightarrow}", xs)
ensureTeX ('>':'=':xs) = ("\\ensuremath{\\geq}", xs)
ensureTeX ('<':'=':xs) = ("\\ensuremath{\\Leftarrow}", xs)

ensureTeX ('>':'-':'>':xs) = ("\\ensuremath{\\leftarrowtail}", xs)
ensureTeX ('<':'-':'<':xs) = ("\\ensuremath{\\rightarrowtail}", xs)
ensureTeX ('~':'>':xs) = ("\\ensuremath{\\leadsto}", xs)

ensureTeX ('<':'-':'>':xs) = ("\\ensuremath{\\leftrightarrow}", xs)
ensureTeX ('<':'-':'-':'>':xs) = ("\\ensuremath{\\longleftrightarrow}", xs)
ensureTeX ('-':'-':'>':xs) = ("\\ensuremath{\\longrightarrow}", xs)
ensureTeX ('<':'-':'-':xs) = ("\\ensuremath{\\longleftarrow}", xs)
ensureTeX ('-':'>':xs) = ("\\ensuremath{\\rightarrow}", xs)
ensureTeX ('<':'-':xs) = ("\\ensuremath{\\leftarrow}", xs)

ensureTeX ('=':'=':'=':xs) = ("\\ensuremath{\\equiv}", xs)
ensureTeX ('!':'=':xs) = ("\\ensuremath{\\neq}", xs)

ensureTeX ('ä':xs) = ("\\text{\\\"a}", xs)
ensureTeX ('ö':xs) = ("\\text{\\\"o}", xs)
ensureTeX ('ü':xs) = ("\\text{\\\"u}", xs)
ensureTeX ('Ä':xs) = ("\\text{\\\"A}", xs)
ensureTeX ('Ö':xs) = ("\\text{\\\"O}", xs)
ensureTeX ('Ü':xs) = ("\\text{\\\"U}", xs)

ensureTeX ('\913':xs) = ("A", xs)
ensureTeX ('\914':xs) = ("B", xs)
ensureTeX ('\915':xs) = ("\\ensuremath{\\Gamma}", xs)
ensureTeX ('\916':xs) = ("\\ensuremath{\\Delta}", xs)
ensureTeX ('\917':xs) = ("E", xs)
ensureTeX ('\918':xs) = ("Z", xs)
ensureTeX ('\919':xs) = ("H", xs)
ensureTeX ('\920':xs) = ("\\ensuremath{\\Theta}", xs)
ensureTeX ('\921':xs) = ("I", xs)
ensureTeX ('\922':xs) = ("K", xs)
ensureTeX ('\923':xs) = ("\\ensuremath{\\Lambda}", xs)
ensureTeX ('\924':xs) = ("M", xs)
ensureTeX ('\925':xs) = ("N", xs)
ensureTeX ('\926':xs) = ("\\ensuremath{\\Xi}", xs)
ensureTeX ('\927':xs) = ("O", xs)
ensureTeX ('\928':xs) = ("\\ensuremath{\\Pi}", xs)
ensureTeX ('\929':xs) = ("P", xs)
-- 930 / 03A2 is reserved
ensureTeX ('\931':xs) = ("\\ensuremath{\\Sigma}", xs)
ensureTeX ('\932':xs) = ("T", xs)
ensureTeX ('\933':xs) = ("\\ensuremath{\\Upsilon}", xs)
ensureTeX ('\934':xs) = ("\\ensuremath{\\Phi}", xs)
ensureTeX ('\935':xs) = ("X", xs)
ensureTeX ('\936':xs) = ("\\ensuremath{\\Psi}", xs)
ensureTeX ('\937':xs) = ("\\ensuremath{\\Omega}", xs)

ensureTeX ('\945':xs) = ("\\ensuremath{\\alpha}", xs)
ensureTeX ('\946':xs) = ("\\ensuremath{\\beta}", xs)
ensureTeX ('\947':xs) = ("\\ensuremath{\\gamma}", xs)
ensureTeX ('\948':xs) = ("\\ensuremath{\\delta}", xs)
ensureTeX ('\949':xs) = ("\\ensuremath{\\epsilon}", xs)
ensureTeX ('\950':xs) = ("\\ensuremath{\\zeta}", xs)
ensureTeX ('\951':xs) = ("\\ensuremath{\\eta}", xs)
ensureTeX ('\952':xs) = ("\\ensuremath{\\theta}", xs)
ensureTeX ('\953':xs) = ("\\ensuremath{\\iota}", xs)
ensureTeX ('\954':xs) = ("\\ensuremath{\\kappa}", xs)
ensureTeX ('\955':xs) = ("\\ensuremath{\\lambda}", xs)
ensureTeX ('\956':xs) = ("\\ensuremath{\\mu}", xs)
ensureTeX ('\957':xs) = ("\\ensuremath{\\nu}", xs)
ensureTeX ('\958':xs) = ("\\ensuremath{\\xi}", xs)
ensureTeX ('\959':xs) = ("\\ensuremath{\\omicron}", xs)
ensureTeX ('\960':xs) = ("\\ensuremath{\\pi}", xs)
ensureTeX ('\961':xs) = ("\\ensuremath{\\rho}", xs)
ensureTeX ('\963':xs) = ("\\ensuremath{\\sigma}", xs)
ensureTeX ('\964':xs) = ("\\ensuremath{\\tau}", xs)
ensureTeX ('\965':xs) = ("\\ensuremath{\\upsilon}", xs)
ensureTeX ('\966':xs) = ("\\ensuremath{\\phi}", xs)
ensureTeX ('\967':xs) = ("\\ensuremath{\\chi}", xs)
ensureTeX ('\968':xs) = ("\\ensuremath{\\psi}", xs)
ensureTeX ('\969':xs) = ("\\ensuremath{\\omega}", xs)

ensureTeX xs = ("", xs)

safeTeX s@(x:xs)
    | a /= "" = a ++ safeTeX b
    | otherwise = x : safeTeX xs
        where (a, b) = ensureTeX s
safeTeX [] = []


