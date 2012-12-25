module Simplex.ToTeX (toTeX) where

import Simplex.Parser
import Simplex.Config
import Simplex.Util
import Simplex.Table
import Simplex.EscapeTeX

import Data.List (intersperse, nubBy)
import Data.List.Split
import Data.Char
import Data.Maybe

import Text.Regex
import Text.Printf

import Prelude hiding (lex)
import Debug.Trace

validUnit = mkRegex (printf "^%s( +plus +%s +minus +%s)? *$" unit unit unit)
    where unit = "([0-9]+(\\.[0-9]+)?|\\.[0-9]+)(" ++ concat (intersperse "|" units) ++ ")"

makePreambleLengths p
  = let s x v
            | isJust (matchRegex validUnit v) = ("\\setlength{\\" ++ x ++ "}{" ++ v ++ "}\n")
            | otherwise = ""
        f (x, (def, _))
            | isJust def = maybe (s x $ fromJust def) (s x) (lookup x p)
            | otherwise  = maybe ""                   (s x) (lookup x p)
    in  concat $ map f knownLengths

gatherDimensions :: [(String, String)] -> [String]
gatherDimensions (("margin-left", v) : xs)   = ("left="   ++ v) : gatherDimensions xs
gatherDimensions (("margin-right", v) : xs)  = ("right="  ++ v) : gatherDimensions xs
gatherDimensions (("margin-top", v) : xs)    = ("top="    ++ v) : gatherDimensions xs
gatherDimensions (("margin-bottom", v) : xs) = ("bottom=" ++ v) : gatherDimensions xs
gatherDimensions (("margins", v) : xs)       = zipWith (++) ["top=", "right=", "bottom=", "left="] $ words v
gatherDimensions (_ : xs) = gatherDimensions xs
gatherDimensions [] = []

makeDimensions p
    | dim == [] = ""
    | otherwise = "\\usepackage[" ++ (concat (intersperse "," dim)) ++ "]{geometry}\n"
        where
            dim = nubBy (\a b -> pref a == pref b ) $ gatherDimensions p
            pref = fst . break (== '=')

makeFancyHeader p = ""

itemsToTeX :: [Items] -> String
itemsToTeX = concatMap f
    where
        f (Item x) = "\\item " ++ escapeTeX x "\n"
        f (Items Itemize is) = "\\begin{itemize}\n" ++ itemsToTeX is ++ "\\end{itemize}\n"
        f (Items Enumerate is) = "\\begin{enumerate}\n" ++ itemsToTeX is ++ "\\end{enumerate}\n"

articleType ((x, _): xs)
    | x `elem` documentClasses = x
    | otherwise = articleType xs
articleType [] = "article"

documentClass cfg props
    | oStandalone cfg = "\\documentclass[preview]{standalone}\n"
    | otherwise = concat $ "\\documentclass[a4paper"

          : maybe "" (',':) (lookup "fontsize" props)
          : maybe "" (const ",draft") (lookup "draft" props)
          : maybe "" (const ",landscape") (lookup "landscape" props)

          : "]{" : articleType props : ["}\n"]

toTeX cfg doc@(Document blocks props) = concat $ preamble $ toTeX' (config cfg doc) $ blocks
    where
        preamble xs =
            documentClass cfg props
            
          : "\\usepackage[utf8]{inputenc}\n"
          : "\\usepackage{fancyhdr}\n"

          : "\\usepackage{eurosym}\n"
          : "\\DeclareUnicodeCharacter{20AC}{\\euro{}}\n"

          : "\\usepackage{amsmath}\n"
          : "\\usepackage{amsfonts}\n"
          : "\\usepackage{amssymb}\n"

          : "\\usepackage{stmaryrd}\n"
          : "\\usepackage{wasysym}\n"

          : "\\let\\EUR\\undefined\n"
          : "\\usepackage{marvosym}\n"

          : "\\usepackage{verbatim}\n"
          : "\\usepackage{listings}\n"
          : "\\usepackage{multicol}\n"

          : "\\usepackage[usenames,dvipsnames]{color}\n"
          : "\\usepackage[table]{xcolor}\n"
          : "\\usepackage{multirow}\n"

          : "\\usepackage{lastpage}\n"
          : "\\usepackage{graphicx}\n"

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
          : makeDimensions props
          : makeFancyHeader props

          : maybe "" id (lookup "preamble" props)
          : "\n\n"

          : maybe
                ""
                (\x -> "\\pagestyle{" ++ x ++ "}\n")
                (lookup "pagestyle" props)

          : maybe
                ""
                (\x -> "\\chead{" ++ escapeTeX' "}\n" x)
                (lookup "chead" props)
          : maybe
                ""
                (\x -> "\\lhead{" ++ escapeTeX' "}\n" x)
                (lookup "lhead" props)
          : maybe
                ""
                (\x -> "\\rhead{" ++ escapeTeX' "}\n" x)
                (lookup "rhead" props)

          : maybe
                ""
                (\x -> "\\cfoot{" ++ escapeTeX' "}\n" x)
                (lookup "cfoot" props)
          : maybe
                ""
                (\x -> "\\lfoot{" ++ escapeTeX' "}\n" x)
                (lookup "lfoot" props)
          : maybe
                ""
                (\x -> "\\rfoot{" ++ escapeTeX' "}\n" x)
                (lookup "rfoot" props)

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

          : "\n{\n"
          : xs

toTeX' opt []
    = when' (oColumns opt > 0) "\\end{multicols}" : ["\n}\n\\end{document}\n"]

toTeX' opt (BSection s : xs)
    = "\\section" : when' (not $ doNumberSections opt) "*" : "{" : escapeTeX "}\n\n" s : toTeX' opt xs

toTeX' opt (BSubsection s : xs)
    = "\\subsection" : when' (not $ doNumberSections opt) "*" : "{" : escapeTeX "}\n\n" s : toTeX' opt xs

toTeX' opt (BSubsubsection s : xs)
    = "\\subsubsection" : when' (not $ doNumberSections opt) "*" : "{" : escapeTeX "}\n\n" s : toTeX' opt xs

toTeX' opt (BPart s : xs)
    = "\\part{" : escapeTeX "}\n\n" s : toTeX' opt xs

toTeX' opt (BChapter s : xs)
    = "\\chapter{" : escapeTeX "}\n\n" s : toTeX' opt xs

toTeX' opt (BLine : xs)
    = "\n\\hspace{\\fill}\\rule{0.8\\linewidth}{0.7pt}\\hspace{\\fill}\n\n" : toTeX' opt xs

toTeX' opt (BTherefore s : xs)
    = "\\paragraph{$\\Rightarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BNTherefore s : xs)
    = "\\paragraph{$\\nRightarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BBecause s : xs)
    = "\\paragraph{$\\Leftarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BNBecause s : xs)
    = "\\paragraph{$\\nLeftarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BIff s : xs)
    = "\\paragraph{$\\Leftrightarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BNIff s : xs)
    = "\\paragraph{$\\nLeftrightarrow$} " : escapeTeX "\n\n" s : toTeX' opt xs

toTeX' opt (BDefine w s : xs)
    = "\\paragraph{" : escapeTeX ('}' : escapeTeX "\n\n" s) w : toTeX' opt xs

toTeX' opt (BRemark w s : xs)
    = "\\subparagraph{\\underline{" : escapeTeX ('}' : '}' : escapeTeX "\n\n" s) w : toTeX' opt xs

toTeX' opt (BAdvise l : xs)
    = "\\begin{advise}\n"
    : (concat ("\\item " : intersperse "\\item " (map (escapeTeX "\n") l)))
    : "\\end{advise}\n" : toTeX' opt xs

toTeX' opt (BItems (Items Itemize is) : xs)
    = "\\begin{itemize}\n"
    : itemsToTeX is
    : "\\end{itemize}\n" : toTeX' opt xs

toTeX' opt (BItems (Items Enumerate is) : xs)
    = "\\begin{enumerate}\n"
    : itemsToTeX is
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
    = "\\begin{lstlisting}\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "code$" l : xs)
    = "\\begin{lstlisting}\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "#$" l : xs)
    = "\\begin{lstlisting}[mathescape]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "#" l : xs)
    = "\\begin{lstlisting}\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "php" l : xs)
    = "\\begin{lstlisting}[language = php]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "java" l : xs)
    = "\\begin{lstlisting}[language = java]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "haskell" l : xs)
    = "\\begin{lstlisting}[language = haskell]\n" : l : "\\end{lstlisting}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "math" l : xs)
    = "\\begin{displaymath}\n" : safeTeX l : "\\end{displaymath}\n" : toTeX' opt xs

toTeX' opt (BVerbatim "error" l : xs)
    = "\\textcolor{red}{IOException: " : escapeTeX "}\n\n" l : toTeX' opt xs

toTeX' opt (BVerbatim "$" l : xs)
    = "\\begin{displaymath}\n" : safeTeX l : "\\end{displaymath}\n" : toTeX' opt xs

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
    | all isDigit x
        = when' (oColumns opt > 0) "\\end{multicols}\n\n"
        : "\\begin{multicols}{" : x : "}\n\n"
        : toTeX' (opt { oColumns = read x } ) xs

toTeX' opt (BCommand "colbreak" _ : xs)
    | oColumns opt > 0
        = "\\vfill\n\\columnbreak\n" : toTeX' opt xs

toTeX' opt (BCommand "endcolumns" _ : xs)
    | oColumns opt > 0
        = "\\end{multicols}\n\n" : toTeX' (opt { oColumns = 0 }) xs
    | otherwise
        = "\\textcolor{orange}{Columns already ended.}"
        : toTeX' opt xs

toTeX' opt (BCommand c (x:_) : xs)
    | isJust l = "\\setlength{\\" : c : "}{" : x : "}\n" : toTeX' opt xs
        where l = lookup c knownLengths

toTeX' opt (BCommand c args : xs)
    | c `elem` knownCommands = ('\\' : c) : "\n" : toTeX' opt xs
    | isJust c' = snd r : "\n" : toTeX' (fst r) xs
    | otherwise = "\\textcolor{red}{Unknown Command: " : escapeTeX "}\n\n" c : toTeX' opt xs
        where c' = lookup c specialCommands
              r  = (fromJust c') opt args


