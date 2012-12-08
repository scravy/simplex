module Simplex.Table (mkTable) where

import Simplex.EscapeTeX
import Simplex.Util
import Simplex.Parser

import Data.List (intersperse)

mkTable :: Table -> String
mkTable (caption, opt, rows@((t,r):rs))
  = let
        spec
            | opt == [] = take numCols $ repeat 'l'
            | otherwise = mkSpec $ filter (`elem` "crl|") $ head opt

        mkSpec s
            | len < numCols = s ++ (take (numCols - len) $ repeat 'l')
            | otherwise = s
                where len = length (filter (`elem` "crl") s) 

        numCols = maximum (map (length.snd) rows)

        mkRows ((NoBorder,[]):rs) = mkRows rs
        mkRows ((NoBorder,cs):rs) = mkCells cs : " \\\\\n" : mkRows rs

        mkRows ((SingleBorder,cs):rs) = mkCells cs : " \\\\ \\hline\n" : mkRows rs
        mkRows ((DoubleBorder,cs):rs) = mkCells cs : " \\\\ \\hline \\hline\n" : mkRows rs

        mkRows [] = []

        body
            | r == [] && t == SingleBorder = concat ("\\hline \n" : mkRows rs)
            | r == [] && t == DoubleBorder = concat ("\\hline \\hline \n" : mkRows rs)
            | otherwise = concat (mkRows rows)

        mkCells = concat . intersperse " & " . map mkCell

        mkCell (Cell color Nothing 1 rowSpan l r t, c)
          = rowCell color rowSpan t c
        mkCell (Cell color (Just a) colSpan rowSpan l r t, c)
          = "\\multicolumn{" ++ show colSpan ++ "}{" ++ colSpec l r a ++ "}{" ++ rowCell color rowSpan t c ++ "}"
        mkCell (Cell color _ colSpan rowSpan l r t, c)
          = "\\multicolumn{" ++ show colSpan ++ "}{" ++ colSpec l r 'c' ++ "}{" ++ rowCell color rowSpan t c ++ "}"

        colSpec True  False a = '|' : [a]
        colSpec False True  a = a : "|"
        colSpec True  True  a = '|' : a : "|"
        colSpec False False a = [a]

        rowCell color 1 t c       = colorCell color t c
        rowCell color rowSpan t c = "\\multirow{" ++ show rowSpan ++ "}{*}{" ++ colorCell color t c ++ "}"
        
        colorCell Nothing t c      = cellContent t c
        colorCell (Just color) t c = "\\cellcolor{" ++ color ++ "}" ++ cellContent t c

        cellContent Verb c = "\\verb#" ++ c ++ "#"
        cellContent Head c = "{\\bfseries " ++ escapeTeX "}" c
        cellContent Math c = '$' : c ++ "$"
        cellContent _ c = escapeTeX "" c

    in  when (caption /= "") "\\begin{table}[!h]\n"
        ++ "\\begin{center}\n"
        ++ "\\begin{tabular}{" ++ spec ++ "}\n"
        ++ body
        ++ "\n\\end{tabular}\n"
        ++ "\n\\end{center}\n"
        ++ when (caption /= "") ("\\caption{" ++ escapeTeX "}\n" caption ++ "\\end{table}\n")
        ++ "\n"

