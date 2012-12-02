module Simplex.ToTeX (toTeX) where

import Simplex.Parser
import Data.List
import Data.Char
import Data.Maybe

import Prelude hiding (lex)

-- chars that introduce an inline verbatim
verbs = "#!@"

knownSymbols
 = ["alpha", "beta", "chi", "delta", "epsilon", "eta", "gamma",
    "iota", "kappa", "lambda", "mu", "nu", "omega", "phi",
    "pi", "psi", "rho", "sigma", "tau", "theta", "upsilon",
    "xi", "zeta", "digamma", "varepsilon", "varkappa", "varphi",
    "varpi", "varrho", "varsigma", "vartheta",

    "Delta", "Gamma", "Lambda", "Omega", "Phi", "Pi", "Psi",
    "Sigma", "Theta", "Upsilon", "Xi",

    "aleph", "beth", "daleth", "gimel",

    "vert", "Vert",
    "langle", "rangle", "lfloor", "rfloor", "lceil", "rceil",
    "llcorner", "lrcorner", "ulcorner", "urcorner",

    "leftarrow", "Leftarrow", "rightarrow", "Rightarrow",
    "leftrightarrow", "Leftrightarrow", "longleftarrow",
    "Longleftarrow", "longrightarrow", "Longrightarrow",
    "longleftrightarrow", "Longleftrightarrow",

    "uparrow", "Uparrow", "downarrow", "Downarrow",
    "updownarrow", "Updownarrow", "mapsto", "hookleftarrow",
    "leftharpoonup", "leftharpoondown", "rightleftharpoons",
    "longmapsto", "hookrightarrow", "rightharpoonup",
    "rightharpoondown", "leadsto",
    "nearrow", "searrow", "swarrow", "nwarrow",

    "dashrightarrow", "leftrightarrows", "leftarrowtail",
    "curvearrowleft", "upuparrows", "multimap",
    "rightleftarrows", "twoheadrightarrow",
    "Rsh", "downharpoonright", "nleftarrow", "nRightarrow",
    "nrightarrow", "nleftrightarrow", "nLeftarrow",
    "nLeftrightarrow",
    "dashleftarrow", "Lleftarrow", "looparrowleft",
    "circlearrowleft", "upharpoonleft", "leftrightsquigarrow",
    "rightarrowtail", "curvearrowright",
    "downdownarrows", "rightsquigarrow", "leftleftarrows",
    "twoheadleftarrow", "leftrightharpoons", "Lsh",
    "downharpoonleft", "rightrightarrows",
    "looparrowright", "circlearrowright", "upharpoonright",

    "sum", "prod", "coprod", "int", "oint", "iint",
    "biguplus", "bigcap", "bigcup",
    "bigoplus", "bigotimes", "bigodot",
    "bigvee", "bigwedge", "bigsqcup",

    "ast", "star", "cdot", "circ", "bullet", "bigcirc", "diamond",
    "times", "div", "centerdot", "circledast", "circledcirc",
    "circleddash", "dotplus", "divideontimes", "pm", "mp",
    "amalg", "odot", "ominus", "oplus", "oslash", "otimes",
    "wr", "Box", "boxplus", "boxminus", "boxtimes", "boxdot",
    "square", "cap", "cup", "uplus", "sqcap", "sqcup",
    "wedge", "vee", "dagger", "ddagger", "barwedge", "curlywedge",
    "Cap", "bot", "intercal", "doublebarwedge", "lhd", "rhd",
    "triangleleft", "triangleright", "unlhd", "unrhd",
    "bigtriangledown", "bigtriangleup", "setminus", "veebar",
    "curlyvee", "Cup", "top", "rightthreetimes", "leftthreetimes",

    "equiv", "cong", "neq", "sim", "simeq", "approx", "asymp",
    "doteq", "propto", "models", "leq", "prec", "preceq", "ll",
    "subset", "subseteq", "sqsubset", "sqsubseteq", "dashv",
    "in", "ni", "notin", "geq", "succ", "succeq", "gg", "supset",
    "supseteq", "sqsupset", "sqsupseteq", "vdash", "perp", "mid",
    "parallel", "bowtie", "Join", "ltimes", "rtimes", "smile",
    "frown",

    "approxeq", "thicksim", "backsim", "backsimeq", "triangleq",
    "circeq", "bumpeq", "Bumpeq", "doteqdot", "thickapprox",
    "fallingdotseq", "risingdotseq", "varpropto", "therefore",
    "because", "eqcirc", "leqq", "leqslant", "lessapprox",
    "lll", "lessdot", "lesssim", "eqslantless", "precsim",
    "precapprox", "Subset", "subseteqq", "preccurlyeq",
    "curlyeqprec", "blacktriangleleft", "trianglelefteq",
    "vartriangleleft", "geqq", "geqslant", "gtrapprox", "ggg",
    "gtrdot", "gtrsim", "eqslantgtr", "succsim", "succapprox",
    "Supset", "supseteqq", "succcurlyeq",
    "curlyeqsucc", "blacktriangleright", "trianglerighteq",
    "vartriangleright", "lessgtr", "lesseqgtr", "gtreqqless",
    "gtreqless", "gtrless", "backepsilon", "between", "pitchfork",
    "shortmid", "smallfrown", "smallsmile", "Vdash", "vDash",
    "Vvdash", "shortparallel",

    "ncong", "nmid", "nparallel", "nshortmid", "nshortparallel",
    "nsim", "nVDash", "nvDash", "nvdash", "ntriangleleft",
    "ntrianglelefteq", "ntriangleright", "ntrianglerighteq",
    "nleq", "nleqq", "nleqslant", "nless", "nprec", "npreceq",
    "precnapprox", "precnsim", "lnapprox", "lneq", "lneqq", "lnsim",
    "lvertneqq", "ngeq", "ngeqq", "ngeqslant", "ngtr", "nsucc",
    "nsucceq", "succnapprox", "succnsim", "gnapprox", "gneq",
    "gneqq", "gnsim", "gvertneqq", "nsubseteq", "nsupseteq",
    "nsubseteqq", "nsupseteqq", "subsetneq", "supsetneq",
    "subsetneqq", "supsetneqq", "varsubsetneq", "varsupsetneq",
    "varsubsetneqq", "varsupsetneqq",

    "infty", "nabla", "partial", "eth", "clubsuit", "diamondsuit",
    "heartsuit", "spadesuit", "cdots", "vdots", "ldots", "ddots",
    "Im", "Re", "forall", "exists", "nexists", "emptyset",
    "varnothing", "imath", "jmath", "ell", "iiiint", "iiint",
    "sharp", "flat", "natural", "Bbbk", "bigstar", "diagdown",
    "diagup", "Diamond", "Finv", "Game", "hbar", "hslash", "lozenge",
    "mho", "prime", "surd", "wp", "angle", "complement",
    "measuredangle", "sphericalangle", "triangledown", "triangle",
    "vartriangle", "blacklozenge", "blacksquare", "blacktriangle",
    "blacktriangledown", "backprime", "circledS" ]

specialSymbols =
   [("A", "\\ensuremath{\\mathbb{A}}"),
    ("B", "\\ensuremath{\\mathbb{B}}"),
    ("C", "\\ensuremath{\\mathbb{C}}"),
    ("D", "\\ensuremath{\\mathbb{D}}"),
    ("E", "\\ensuremath{\\mathbb{E}}"),
    ("F", "\\ensuremath{\\mathbb{F}}"),
    ("G", "\\ensuremath{\\mathbb{G}}"),
    ("H", "\\ensuremath{\\mathbb{H}}"),
    ("I", "\\ensuremath{\\mathbb{I}}"),
    ("J", "\\ensuremath{\\mathbb{J}}"),
    ("K", "\\ensuremath{\\mathbb{K}}"),
    ("L", "\\ensuremath{\\mathbb{L}}"),
    ("M", "\\ensuremath{\\mathbb{M}}"),
    ("N", "\\ensuremath{\\mathbb{N}}"),
    ("O", "\\ensuremath{\\mathbb{O}}"),
    ("P", "\\ensuremath{\\mathbb{P}}"),
    ("Q", "\\ensuremath{\\mathbb{Q}}"),
    ("R", "\\ensuremath{\\mathbb{R}}"),
    ("S", "\\ensuremath{\\mathbb{S}}"),
    ("T", "\\ensuremath{\\mathbb{T}}"),
    ("U", "\\ensuremath{\\mathbb{U}}"),
    ("V", "\\ensuremath{\\mathbb{V}}"),
    ("W", "\\ensuremath{\\mathbb{W}}"),
    ("X", "\\ensuremath{\\mathbb{X}}"),
    ("Y", "\\ensuremath{\\mathbb{Y}}"),
    ("Z", "\\ensuremath{\\mathbb{Z}}"),

    ("c", "\\text{\\copyright}"),
    ("copyright", "\\text{\\copyright}"),
    ("ae", "\\text{\\ae}"),
    ("AE", "\\text{\\AE}"),
    ("oe", "\\text{\\oe}"),
    ("OE", "\\text{\\OE}"),

    ("par", "\\text{\\P}"),
    ("sec", "\\text{\\S}"),
    ("sect", "\\text{\\S}"),
    ("section", "\\text{\\S}"),

    ("aa", "\\text{\\aa}"),
    ("AA", "\\text{\\AA}"),
    ("ss", "\\text{\\ss}"),
    ("dag", "\\text{\\dag}"),
    ("ddag", "\\text{\\ddag}"),
    ("pounds", "\\text{\\pounds}"),

    ("bullet", "\\text{\\textbullet}"),
    ("backslash", "\\text{\\textbackslash}"),

    ("clubs", "\\ensuremath{\\clubsuit}"),
    ("hearts", "\\ensuremath{\\heartsuit}"),
    ("spades", "\\ensuremath{\\spadesuit}"),
    ("diamonds", "\\ensuremath{\\diamondsuit}"),

    ("space", "~"),
    ("nbsp", "~")]

knownCommands
 = ["newpage", "vfill", "hfill", "normalsize", "normalfont",
    "noindent",

    "tiny", "scriptsize", "footnotesize", "small",
    "large", "Large", "LARGE", "huge", "Huge",

    "rmfamily", "sffamily", "ttfamily", "mdseries", "bfseries",
    "upshape", "itshape", "slshape", "scschape", "em"]

specialCommands
 = [("pagebreak", "newpage")]

tail' [] = []
tail' xs = tail xs

tail'' [] = []
tail'' xs = tail' xs

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

toTeX (Document blocks props) = (concat . preamble . toTeX') blocks
    where
        preamble xs =
            "\\documentclass[a4paper"

          : maybe "" (',':) (lookup "fontsize" props)
          : maybe "" (const ",draft") (lookup "draft" props)
          : maybe "" (const ",landscape") (lookup "landscape" props)

          : "]{article}\n"

          : "\\usepackage[utf8]{inputenc}\n"

          : "\\usepackage{amsmath}\n"
          : "\\usepackage{amsfonts}\n"
          : "\\usepackage{amssymb}\n"

          : "\\usepackage{verbatim}\n"
          : "\\usepackage{listings}\n"

          : "\\usepackage{color}\n"
          : "\\usepackage[table]{xcolor}\n"
          : "\\usepackage{multirow}\n"

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

          : maybe ""
                (const "\\maketitle\n\\thispagestyle{empty}\n\n")
                (lookup "title" props)
          : maybe ""
                (("\\begin{abstract}\n" ++) . escapeTeX "\\end{abstract}\n\n")
                (lookup "abstract" props)          

          : xs

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

toTeX' (BDescription l : xs)
    = "\\begin{description}\n"
    : concat (map (\(dt, dd) -> "\\item[" ++ escapeTeX (']' : ' ' : escapeTeX "\n" dd) dt) l)
    : "\\end{description}\n" : toTeX' xs

toTeX' (BDescribeItems l : xs)
    = "\\begin{itemize}\n"
    : concat (map (\(dt, dd) -> "\\item[\\textbf{" ++ escapeTeX ('}' : ']' : ' ' : escapeTeX "\n" dd) dt) l)
    : "\\end{itemize}\n" : toTeX' xs

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

toTeX' (BVerbatim "table" l : xs)
    = "\n" : l : "\n" : toTeX' xs

toTeX' (BVerbatim "comment" l : xs)
    = "\n" : (unlines $ map ('%':) $ lines l) : "\n" : toTeX' xs

toTeX' (BVerbatim "%" l : xs)
    = "\n" : (unlines $ map ('%':) $ lines l) : "\n" : toTeX' xs

toTeX' (BVerbatim _ l : xs)
    = "\\begin{verbatim}\n" : l : "\\end{verbatim}\n" : toTeX' xs

toTeX' (BTable table : xs)
    = mkTable table : toTeX' xs

-- maybe report unknown BAny here
toTeX' (BAny _ s : xs)
    = escapeTeX "\n\n" s : toTeX' xs

toTeX' (BParagraph s : xs)
    = escapeTeX "\n\n" s : toTeX' xs

toTeX' (BCommand "break" [x] : xs)
    = "\\hfill \\\\[" : x : "]" : toTeX' xs

-- use arguments “a” (!!) - “_” by now
toTeX' (BCommand c _ : xs)
    | c `elem` knownCommands = ('\\' : c) : "\n" : toTeX' xs
    | isJust c' = ('\\' : fromJust c') : "\n" : toTeX' xs
    | otherwise = "\\textcolor{red}{Unknown Command: " : c : "}\n\n" : toTeX' xs
        where c' = lookup c specialCommands

toTeX' [] = ["\n\\end{document}\n"]

-- toTeX' (x : xs) = error ("Unknown Thingie: " ++ show x)

mkTable :: Table -> String
mkTable (caption, opt, rows@((t,r):rs))
  = let
        spec
            | opt == [] = intersperse '|' $ take numCols $ repeat 'l'
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

        mkCell (Cell, c) = escapeTeX "" c
        mkCell (CellVerb, c) = "\\verb#" ++ c ++ "#"
        mkCell (CellMath, c) = '$' : c ++ "$"
        mkCell (CellHead, c) = "\\multicolumn{1}{c}{\\bfseries " ++ escapeTeX "}" c

        mkCell (CustomCell color Nothing 1 rowSpan, c)
          = rowCell color rowSpan c
        mkCell (CustomCell color (Just a) colSpan rowSpan, c)
          = "\\multicolumn{" ++ show colSpan ++ "}{" ++ [a] ++ "}{" ++ rowCell color rowSpan c ++ "}"
        mkCell (CustomCell color _ colSpan rowSpan, c)
          = "\\multicolumn{" ++ show colSpan ++ "}{c}{" ++ rowCell color rowSpan c ++ "}"

        rowCell color 1 c       = colorCell color c
        rowCell color rowSpan c = "\\multirow{" ++ show rowSpan ++ "}{*}{" ++ colorCell color c ++ "}"
        
        colorCell Nothing c      = mkCell (Cell, c)
        colorCell (Just color) c = "\\cellcolor{" ++ color ++ "}" ++ mkCell (Cell, c)

    in  "\\begin{table}[!h]\n"
     ++ "\\begin{center}\n"
     ++ "\\begin{tabular}{" ++ spec ++ "}\n"
     ++ body
     ++ "\n\\end{tabular}\n"
     ++ "\n\\end{center}\n"
     ++ when (caption /= "") ("\\caption{" ++ escapeTeX "}\n" caption)
     ++ "\\end{table}\n\n"

     ++ escapeTeX "\n" (map f $ show rows)

f ',' = '\n'
f x   = x

when True x = x
when False _ = []
