module Simplex.Table (mkTable, mkAsciiTable) where

import Simplex.EscapeTeX
import Simplex.Util
import Simplex.Parser

import Data.List (intersperse)
import Data.List.Split
import Data.Maybe

mkAsciiTable :: String -> Table
mkAsciiTable def = (newTableOpt { tableDef = tDef }, rows)
    where
        rows = (SingleBorder, []) : (map fromJust $ filter isJust $ map f (lines def))
        numCols = maximum (map (\(_, r) -> length r) rows) + 1
        tDef = '|' : (intersperse '|' $ take numCols $ repeat 'c')
        f s = case line of
                ('+':_) -> Nothing
                _ -> Just (SingleBorder, tail' $ map g $ linesBy (== '|') line)
            where
                line = dropWhile (flip elem " \t") s
        g s = (Cell Nothing Nothing 1 1 True True Default, s)


mkTable :: Table -> String
mkTable (opt, rows@((t,r):rs))
  = let
        captionTop = maybe "" id (tableCaptionTop opt)
        captionBottom = maybe "" id (tableCaptionBottom opt)

        spec
            | tableDef opt == "" = take numCols $ repeat 'l'
            | otherwise = mkSpec $ tableDef opt

        mkSpec s
            | len < numCols = s ++ (take (numCols - len) $ repeat 'l')
            | otherwise = s
                where
                    len = length (filter (`elem` "crlpmbX") $ rem False s)
                    rem False ('{':xs) = rem True xs
                    rem False (x:xs)   = x : rem False xs
                    rem True  ('}':xs) = rem False xs
                    rem True  (x:xs)   = rem True xs
                    rem _ _ = ""

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
        cellContent Math c = '$' : safeTeX c ++ "$"
        cellContent _ c = escapeTeX "" c

    in  when' (captionTop /= "" || captionBottom /= "") "\\begin{table}[!h]\n"
        ++ "\\begin{center}\n"
        ++ when' (captionTop /= "") ("\\caption{" ++ escapeTeX "}\n" captionTop)
        ++ ifElse (tableX opt)
            ("\\begin{tabularx}{\\linewidth}{" ++ spec ++ "}\n")
            ("\\begin{tabular}{" ++ spec ++ "}\n")
        ++ body
        ++ ifElse (tableX opt) ("\n\\end{tabularx}\n") ("\n\\end{tabular}\n")
        ++ "\n\\end{center}\n"
        ++ when' (captionBottom /= "") ("\\caption{" ++ escapeTeX "}\n" captionBottom)            
        ++ when' (captionTop /= "" || captionBottom /= "") "\\end{table}\n"
        ++ "\n"


