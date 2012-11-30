module Simplex.ToTeX (toTeX) where

import Simplex.Parser
import Data.List

import Prelude hiding (lex)

escapeTeX :: String -> String -> String

escapeTeX t xs
    | a /= "" = a ++ escapeTeX t b
        where (a, b) = ensureTeX xs

escapeTeX t ('$':' ':xs) = '\\' : '$' : escapeTeX t xs
escapeTeX t ('$':xs) = let (m, ms) = break (== '$') xs
                       in '$' : (safeTeX m) ++ '$' : escapeTeX t (tail ms)

escapeTeX t ('_':' ':xs) = '\\' : '_' : escapeTeX t xs
escapeTeX t ('_':xs) = let (m, ms) = break (== '_') xs
                       in "\\underline{" ++ escapeTeX "" m ++ '}' : escapeTeX t (tail ms)

escapeTeX t ('*':'*':' ':xs) = '*' : escapeTeX t xs
escapeTeX t ('*':'*':xs) = let (m, ms) = break (== '*') xs
                       in "\\textbf{" ++ escapeTeX "" m ++ '}' : escapeTeX t (drop 2 ms)

escapeTeX t ('*':' ':xs) = '*' : escapeTeX t xs
escapeTeX t ('*':xs) = let (m, ms) = break (== '*') xs
                       in "\\emph{" ++ escapeTeX "" m ++ '}' : escapeTeX t (tail ms)

escapeTeX t s@(x:xs)
    | x `elem` "{}%#&" = '\\' : x : escapeTeX t xs
    | x == '~' = "$\\sim$" ++ escapeTeX t xs
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

ensureTeX xs = ("", xs)

safeTeX s@(x:xs)
    | a /= "" = a ++ safeTeX b
    | otherwise = x : safeTeX xs
        where (a, b) = ensureTeX s
safeTeX [] = []

toTeX (Document blocks props) = (concat . preamble . toTeX') blocks
    where
        preamble xs =
            "\\documentclass[a4paper,"

          : maybe "10pt" id (lookup "fontsize" props)
          : "]{article}\n"

          : "\\usepackage{color}\n"
          : "\\usepackage[utf8]{inputenc}\n"
          : "\\usepackage{amsmath}\n"
          : "\\usepackage{amsfonts}\n"
          : "\\usepackage{verbatim}\n"
          : "\\usepackage{listings}\n"
          : "\\usepackage{amssymb}\n"

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
          : " {\\advance\\linewidth\\@totalleftmargin\n"
          : " \\@totalleftmargin\\z@\n"
          : " \\@listi\n"
          : " \\let\\small\\footnotesize \\small\\sffamily\n"
          : " \\parsep \\z@ \\@plus\\z@ \\@minus\\z@\n"
          : " \\topsep6\\p@ \\@plus1\\p@\\@minus2\\p@\n"
          : " \\def\\makelabel##1{\\hss\\llap{##1}}}}\n"
          : " \\let\\endadvise\\endlist\n"
          : " \\def\\advisespace{\\hbox{}\\qquad}\n"
          : " \\def\\labeladvise{$\\to$}\n"
          : " \\makeatother\n"

          : maybe "" id (lookup "preamble" props)
          : "\n\n"

          : maybe
                "\\setlength{\\parindent}{0cm}\n"
                (\x -> "\\setlength{\\parindent}{" ++ x ++ "}\n")
                (lookup "parindent" props)
          : maybe
                "\\setlength{\\parskip}{1ex}\n"
                (\x -> "\\setlength{\\parskip}{" ++ x ++ "}\n")
                (lookup "parskip" props)

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

          : "\\maketitle\n\\thispagestyle{empty}\n\n" : xs

toTeX' (BSection s : xs)
    = "\\section*{" : escapeTeX "}\n\n" s : toTeX' xs

toTeX' (BSubsection s : xs)
    = "\\subsection*{" : escapeTeX "}\n\n" s : toTeX' xs

toTeX' (BSubsubsection s : xs)
    = "\\subsubsection*{" : escapeTeX "}\n\n" s : toTeX' xs

toTeX' (BLine : xs)
    = "\n\n\\hspace{\\fill}\\rule{0.8\\linewidth}{0.7pt}\\hspace{\\fill}\n\n" : toTeX' xs

toTeX' (BTherefore s : xs)
    = "\\paragraph{$\\Rightarrow$} " : escapeTeX "\n\n" s : toTeX' xs

toTeX' (BNTherefore s : xs)
    = "\\paragraph{$\\nRightarrow$} " : escapeTeX "\n\n" s : toTeX' xs

toTeX' (BBecause s : xs)
    = "\\paragraph{$\\Leftarrow$} " : escapeTeX "\n\n" s : toTeX' xs

toTeX' (BNBecause s : xs)
    = "\\paragraph{$\\nLeftarrow$} " : escapeTeX "\n\n" s : toTeX' xs

toTeX' (BDefine w s : xs)
    = "\\paragraph{" : escapeTeX ('}' : escapeTeX "\n\n" s) w : toTeX' xs

toTeX' (BRemark w s : xs)
    = "\\underline{" : escapeTeX ('}' : escapeTeX "\n\n" s) w : toTeX' xs

toTeX' (BAdvise l : xs)
    = "\\begin{advise}\n"
    : (concat ("\\item " : intersperse "\\item " (map (escapeTeX "\n") l)))
    : "\\end{advise}\n" : toTeX' xs

toTeX' (BItemize l : xs)
    = "\\begin{itemize}\n"
    : (concat ("\\item " : intersperse "\\item " (map (escapeTeX "\n") l)))
    : "\\end{itemize}\n" : toTeX' xs

toTeX' (BEnumerate l : xs)
    = "\\begin{enumerate}\n"
    : (concat ("\\item " : intersperse "\\item " (map (escapeTeX "\n") l)))
    : "\\end{enumerate}\n" : toTeX' xs

toTeX' (BVerbatim "ascii" l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' xs

toTeX' (BVerbatim "verbatim" l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' xs

toTeX' (BVerbatim "!" l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' xs

toTeX' (BVerbatim "code" l : xs)
    = "\\begin{lstlisting}[mathescape]\n" : l : "\\end{lstlisting}\n" : toTeX' xs

toTeX' (BVerbatim "#" l : xs)
    = "\\begin{lstlisting}[mathescape]\n" : l : "\\end{lstlisting}\n" : toTeX' xs

toTeX' (BVerbatim "php" l : xs)
    = "\\begin{lstlisting}[language = php]\n" : l : "\\end{lstlisting}\n" : toTeX' xs

toTeX' (BVerbatim "java" l : xs)
    = "\\begin{lstlisting}[language = java]\n" : l : "\\end{lstlisting}\n" : toTeX' xs

toTeX' (BVerbatim "haskell" l : xs)
    = "\\begin{lstlisting}[language = haskell]\n" : l : "\\end{lstlisting}\n" : toTeX' xs

toTeX' (BVerbatim "math" l : xs)
    = "\\begin{displaymath}\n" : l : "\\end{displaymath}\n" : toTeX' xs

toTeX' (BVerbatim "$" l : xs)
    = "\\begin{displaymath}\n" : l : "\\end{displaymath}\n" : toTeX' xs

toTeX' (BVerbatim "latex" l : xs)
    = "\n" : l : "\n" : toTeX' xs

toTeX' (BVerbatim "comment" l : xs)
    = "\n" : (unlines $ map ('%':) $ lines l) : "\n" : toTeX' xs

toTeX' (BVerbatim _ l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' xs

toTeX' (BAny t s : xs)
    = escapeTeX "\n\n" s : toTeX' xs

toTeX' (BParagraph s : xs)
    = escapeTeX "\n\n" s : toTeX' xs

toTeX' (BCommand "break" [x] : xs)
    = "\\hfill \\\\[" : x : "]" : toTeX' xs

toTeX' (BCommand c a : xs)
    =   case c of
            "pagebreak" -> "\\newpage\n" : toTeX' xs
            _ -> "\\" : c : "\n" : toTeX' xs

toTeX' [] = ["\n\\end{document}\n"]

toTeX' (x : xs) = error ("Unknown Thingie: " ++ show x)

