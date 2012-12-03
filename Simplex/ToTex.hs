module Simplex.ToTeX (toTeX) where

import Simplex.Parser
import Data.List (intersperse)
import Data.Char
import Data.Maybe
import Simplex.Config

import Prelude hiding (lex)

tail' [] = []
tail' xs = tail xs

tail'' [] = []
tail'' xs = tail' $ tail' xs

skipOneSpace (' ':xs) = xs
skipOneSpace s = s

escapeTeX :: String -> String -> String

escapeTeX t xs
    | a /= "" = a ++ escapeTeX t b
        where (a, b) = ensureTeX xs

escapeTeX t ('$':' ':xs) = '\\' : '$' : escapeTeX t xs
escapeTeX t ('$':xs) = let (m, ms) = break (== '$') xs
                       in '$' : (safeTeX m) ++ '$' : escapeTeX t (tail' ms)

escapeTeX t ('\\':x:' ':xs)
    | x `elem` verbs = '\\' : x : escapeTeX t xs
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
escapeTeX' t (x:xs)
    | x `elem` "\\{}$^_%~#&" = '\\' : x : escapeTeX' t xs
    | x == '\n' = '\\' : '\\' : ' ' : escapeTeX' t xs
    | otherwise = x : escapeTeX' t xs
escapeTeX' t [] = t

known w
    | w `elem` knownSymbols = Just ("\\ensuremath{\\" ++ w ++ "}")
    | otherwise             = lookup w specialSymbols

ensureTeX ('\\':'´':x:xs)
    | isAlpha x = ("\\´" ++ [x], xs)
ensureTeX ('\\':'`':x:xs)
    | isAlpha x = ("\\`" ++ [x], xs)
ensureTeX ('\\':'^':x:xs)
    | isAlpha x = ("\\^" ++ [x], xs)
ensureTeX ('\\':'~':x:xs)
    | isAlpha x = ("\\~" ++ [x], xs)
ensureTeX ('\\':'=':x:xs)
    | isAlpha x = ("\\=" ++ [x], xs)
ensureTeX ('\\':'.':x:xs)
    | isAlpha x = ("\\." ++ [x], xs)
ensureTeX ('\\':'"':x:xs)
    | isAlpha x = ("\\\"" ++ [x], xs)

ensureTeX ('\\':'v':' ':x:xs)
    | isAlpha x = ("\\v" ++ [' ', x], xs)
ensureTeX ('\\':'H':' ':x:xs)
    | isAlpha x = ("\\H" ++ [' ', x], xs)
ensureTeX ('\\':'c':' ':x:xs)
    | isAlpha x = ("\\c" ++ [' ', x], xs)
ensureTeX ('\\':'d':' ':x:xs)
    | isAlpha x = ("\\d" ++ [' ', x], xs)
ensureTeX ('\\':'b':' ':x:xs)
    | isAlpha x = ("\\b" ++ [' ', x], xs)
ensureTeX ('\\':'t':' ':x:xs)
    | isAlpha x = ("\\t" ++ [' ', x], xs)

ensureTeX ('\\':'_':xs) = ("\\_", xs)
ensureTeX ('\\':'%':xs) = ("\\%", xs)
ensureTeX ('\\':'&':xs) = ("\\&", xs)
ensureTeX ('\\':'#':xs) = ("\\#", xs)

ensureTeX ('\\':xs) = let (w, ws) = break (not.isAlpha) xs
                      in maybe ("", xs) (\x -> (x, skipOneSpace ws)) (known w)

ensureTeX ('≤':xs) = ("\\ensuremath{\\leq}", xs)
ensureTeX ('≥':xs) = ("\\ensuremath{\\geq}", xs)

ensureTeX ('.':'.':'.':xs) = ("\\ensuremath{\\dots}", xs)

ensureTeX ('<':'=':'>':xs) = ("\\ensuremath{\\Leftrightarrow}", xs)
ensureTeX ('<':'=':'=':'>':xs) = ("\\ensuremath{\\Longleftrightarrow}", xs)
ensureTeX ('=':'=':'>':xs) = ("\\ensuremath{\\Longrightarrow}", xs)
ensureTeX ('<':'=':'=':xs) = ("\\ensuremath{\\Longleftarrow}", xs)
ensureTeX ('=':'!':'>':xs) = ("\\ensuremath{\\nRightarrow}", xs)
ensureTeX ('<':'!':'=':xs) = ("\\ensuremath{\\nLeftarrow}", xs)
ensureTeX ('<':'!':'>':xs) = ("\\ensuremath{\\nLeftrightarrow}", xs)
ensureTeX ('=':'>':xs) = ("\\ensuremath{\\Rightarrow}", xs)
ensureTeX ('<':'=':xs) = ("\\ensuremath{\\Leftarrow}", xs)

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

makePreambleLengths p
  = let s x v = "\\setlength{\\" ++ x ++ "}{" ++ v ++ "}\n"
        f (x, (def, _))
            | isJust def = maybe (s x $ fromJust def) (s x) (lookup x p)
            | otherwise  = maybe ""                   (s x) (lookup x p)
    in  concat $ map f knownLengths

toTeX doc@(Document blocks props) = concat $ preamble $ toTeX' (config doc) $ blocks
    where
        preamble xs =
            "\\documentclass[a4paper"

          : maybe "" (',':) (lookup "fontsize" props)
          : maybe "" (const ",draft") (lookup "draft" props)
          : maybe "" (const ",landscape") (lookup "landscape" props)

          : "]{article}\n"

          : "\\usepackage[utf8]{inputenc}\n"

          : "\\usepackage{eurosym}\n"
          : "\\DeclareUnicodeCharacter{20AC}{\\euro{}}\n"

          : "\\usepackage{amsmath}\n"
          : "\\usepackage{amsfonts}\n"
          : "\\usepackage{amssymb}\n"

          : "\\usepackage{verbatim}\n"
          : "\\usepackage{listings}\n"
          : "\\usepackage{multicol}\n"

          : "\\usepackage[usenames,dvipsnames]{color}\n"
          : "\\usepackage[table]{xcolor}\n"
          : "\\usepackage{multirow}\n"

          : "\\usepackage["
          : "colorlinks,"
          : "pdfpagelabels,"
          : "pdfstartview=FitH,"
          : "bookmarksopen=true,"
          : "bookmarksnumbered=true,"
          : "linkcolor=black,"
          : "plainpages=false,"
          : "hypertexnames=false,"
          : "citecolor=black,"
          : "urlcolor=black]"
          : "{hyperref}\n"

          : "\\lstset{"
          : "basicstyle=\\small\\ttfamily,"
          : "flexiblecolumns=false,"
          : "basewidth={0.5em,0.45em},"
          : "numbers=left,"
          : "numberstyle=\\tiny,"
          : "stepnumber=1,"
          : "numbersep=5pt,"
          : "keywordstyle=\\slshape,"
          : "literate={ö}{{\\\"o}}1\n{ä}{{\\\"a}}1{ü}{{\\\"u}}1{Ö}{{\\\"O}}1\n{Ä}{{\\\"ä}}1{Ü}{{\\\"ü}}1"
          : "}\n"

          : "\\makeatletter\n"
          : "\\def\\advise{\\par\\list\\labeladvise\n"
          : "{\\advance\\linewidth\\@totalleftmargin\n"
          : "\\@totalleftmargin\\z@\n"
          : "\\@listi\n"
          : "\\let\\small\\footnotesize \\small\\sffamily\n"
          : "\\parsep \\z@ \\@plus\\z@ \\@minus\\z@\n"
          : "\\topsep6\\p@ \\@plus1\\p@\\@minus2\\p@\n"
          : "\\def\\makelabel##1{\\hss\\llap{##1}}}}\n"
          : "\\let\\endadvise\\endlist\n"
          : "\\def\\advisespace{\\hbox{}\\qquad}\n"
          : "\\def\\labeladvise{$\\to$}\n"
          : "\\makeatother\n"

          : makePreambleLengths props

          : maybe "" id (lookup "preamble" props)
          : "\n\n"

          : maybe
                ""
                (const "\\usepackage{setspace}\n\\doublespacing\n")
                (lookup "doublespacing" props)

          : maybe
                ""
                (\x -> "\\setcounter{tocdepth}{" ++ x ++ "}\n")
                (lookup "tocdepth" props)

          : maybe ""
                (("\\author{" ++) . escapeTeX "}\n" . concat . intersperse ", " . lines)
                (lookup "authors" props)
          : maybe ""
                (("\\title{" ++) . escapeTeX' "}\n")
                (lookup "title" props)
          : maybe
                "\\date{\\today}\n"
                (("\\date{" ++) . escapeTeX' "}\n")
                (lookup "date" props)

          : "\n\\begin{document}\n"

          : maybe ""
                (const "\\maketitle\n\\thispagestyle{empty}\n\n")
                (lookup "title" props)
          : maybe ""
                (("\\begin{abstract}\n" ++) . escapeTeX "\\end{abstract}\n\n")
                (lookup "abstract" props)          

          : xs

toTeX' opt (BSection s : xs)
    = "\\section" : when (not $ doNumberSections opt) "*" : "{" : escapeTeX "}\n\n" s : toTeX' opt xs

toTeX' opt (BSubsection s : xs)
    = "\\subsection" : when (not $ doNumberSections opt) "*" : "{" : escapeTeX "}\n\n" s : toTeX' opt xs

toTeX' opt (BSubsubsection s : xs)
    = "\\subsubsection" : when (not $ doNumberSections opt) "*" : "{" : escapeTeX "}\n\n" s : toTeX' opt xs

toTeX' opt (BLine : xs)
    = "\n\n\\hspace{\\fill}\\rule{0.8\\linewidth}{0.7pt}\\hspace{\\fill}\n\n" : toTeX' opt xs

toTeX' opt (BTherefore s : xs)
    = "\\paragraph{$\\Rightarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BNTherefore s : xs)
    = "\\paragraph{$\\nRightarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BBecause s : xs)
    = "\\paragraph{$\\Leftarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BNBecause s : xs)
    = "\\paragraph{$\\nLeftarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BDefine w s : xs)
    = "\\paragraph{" : escapeTeX ('}' : escapeTeX "\n\n" s) w : toTeX' opt xs

toTeX' opt (BRemark w s : xs)
    = "\\underline{" : escapeTeX ('}' : escapeTeX "\n\n" s) w : toTeX' opt xs

toTeX' opt (BAdvise l : xs)
    = "\\begin{advise}\n"
    : (concat ("\\item " : intersperse "\\item " (map (escapeTeX "\n") l)))
    : "\\end{advise}\n" : toTeX' opt xs

toTeX' opt (BItemize l : xs)
    = "\\begin{itemize}\n"
    : (concat ("\\item " : intersperse "\\item " (map (escapeTeX "\n") l)))
    : "\\end{itemize}\n" : toTeX' opt xs

toTeX' opt (BEnumerate l : xs)
    = "\\begin{enumerate}\n"
    : (concat ("\\item " : intersperse "\\item " (map (escapeTeX "\n") l)))
    : "\\end{enumerate}\n" : toTeX' opt xs

toTeX' opt (BDescription l : xs)
    = "\\begin{description}\n"
    : concat (map (\(dt, dd) -> "\\item[" ++ escapeTeX (']' : ' ' : escapeTeX "\n" dd) dt) l)
    : "\\end{description}\n" : toTeX' opt xs

toTeX' opt (BDescribeItems l : xs)
    = "\\begin{itemize}\n"
    : concat (map (\(dt, dd) -> "\\item[\\textbf{" ++ escapeTeX ('}' : ']' : ' ' : escapeTeX "\n" dd) dt) l)
    : "\\end{itemize}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "ascii" l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "verbatim" l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "!" l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "code" l : xs)
    = "\\begin{lstlisting}[mathescape]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "#" l : xs)
    = "\\begin{lstlisting}[mathescape]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "php" l : xs)
    = "\\begin{lstlisting}[language = php]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "java" l : xs)
    = "\\begin{lstlisting}[language = java]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "haskell" l : xs)
    = "\\begin{lstlisting}[language = haskell]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "math" l : xs)
    = "\\begin{displaymath}\n" : l : "\\end{displaymath}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "$" l : xs)
    = "\\begin{displaymath}\n" : l : "\\end{displaymath}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "latex" l : xs)
    = "\n" : l : "\n" : toTeX' opt xs

toTeX' opt (BVerbatim "table" l : xs)
    = "\n" : l : "\n" : toTeX' opt xs

toTeX' opt (BVerbatim "comment" l : xs)
    = "\n" : (unlines $ map ('%':) $ lines l) : "\n" : toTeX' opt xs

toTeX' opt (BVerbatim "%" l : xs)
    = "\n" : (unlines $ map ('%':) $ lines l) : "\n" : toTeX' opt xs

toTeX' opt (BVerbatim _ l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' opt xs

toTeX' opt (BTable table : xs)
    = mkTable table : toTeX' opt xs

toTeX' opt (BAny "%" _ : xs)
    = toTeX' opt xs

-- maybe report unknown BAny here
toTeX' opt (BAny t s : xs)
    = "\\textcolor{red}{Unknown Block: " : escapeTeX "}\n\n" t : toTeX' opt xs

toTeX' opt (BParagraph s : xs)
    = escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BCommand "break" [x] : xs)
    = "\\hfill \\\\[" : x : "]" : toTeX' opt xs

toTeX' opt (BCommand "columns" [x] : xs)
    = "\\begin{multicols}{" : x : "}\n\n" : toTeX' opt xs

toTeX' opt (BCommand "colbreak" _ : xs)
    = "\\vfill\n\\columnbreak\n" : toTeX' opt xs

toTeX' opt (BCommand "endcolumns" _ : xs)
    = "\\end{multicols}\n\n" : toTeX' opt xs

toTeX' opt (BCommand c (x:_) : xs)
    | isJust l = "\\setlength{\\" : c : "}{" : x : "}\n" : toTeX' opt xs
        where l = lookup c knownLengths

toTeX' opt (BCommand c _ : xs)
    | c `elem` knownCommands = ('\\' : c) : "\n" : toTeX' opt xs
    | isJust c' = ('\\' : fromJust c') : "\n" : toTeX' opt xs
    | otherwise = "\\textcolor{red}{Unknown Command: " : escapeTeX "}\n\n" c : toTeX' opt xs
        where c' = lookup c specialCommands
              
toTeX' _ [] = ["\n\\end{document}\n"]

mkTable :: Table -> String
mkTable (caption, opt, rows@((t,r):rs))
  = let
        spec
            | opt == [] = take numCols $ repeat 'l'
            | otherwise = head opt
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

when True x = x
when False _ = []

ifElse True x _ = x
ifElse _    _ y = y

