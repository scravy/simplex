module Simplex.Config (
        verbs, knownLengths, knownSymbols, knownCommands,
        specialCommands, specialSymbols,
        Config (..), config
    ) where

import Simplex.Parser
import Data.List (sort)
import Simplex.Commands
import Simplex.ConfigData

config (Document blocks props)
 = conf defaultConfig blocks

conf c (BCommand "tableofcontents" [] : xs) = conf (c { doNumberSections = True }) xs
conf c (x : xs) = conf c xs
conf c _ = c

-- chars that introduce an inline verbatim
verbs = "#!@"

knownLengths
 = [("baselineskip",    (Nothing, "The normal vertical distance between lines in a paragraph")),
    ("baselinestretch", (Nothing, "Multiplies @baselineskip")),
    ("columnsep",       (Just "20pt",  "The distance between columns")),
    ("columnwidth",     (Nothing, "The width of the column")),
    ("evensidemargin",  (Nothing, "The margin for 'even' pages (think of a printed booklet)")),
    ("oddsidemargin",   (Nothing, "The margin for 'odd' pages (think of a printed booklet)")),
    ("paperwidth",      (Nothing, "The width of the page")),
    ("paperheight",     (Nothing, "The height of the page")),
    ("textfloatsep",    (Just "10pt plus 4pt minus 3pt", "")),
    ("parskip",         (Just "1ex",  "The extra vertical space between paragraphs")),
    ("parindent",       (Just "0cm",   "The normal paragraph indentation")),
    ("textwidth",       (Nothing, "The width of the text on the page")),
    ("textheight",      (Nothing, "The height of text on the page")),
    ("tabcolsep",       (Nothing, "The default separation between columns in a tabular environment")),
    ("topmargin",       (Nothing, "The size of the top margin")),
    ("columnseprule",   (Nothing, "")),
    ("headheight",      (Nothing, ""))]

knownSymbols
 = ["thepage", "thechapter",

    "alpha", "beta", "chi", "delta", "epsilon", "eta", "gamma",
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
   [("lastpage", "\\pageref{LastPage}"),

    ("A", "\\ensuremath{\\mathbb{A}}"),
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

    ("Af", "\\ensuremath{\\mathfrak{A}}"),
    ("Bf", "\\ensuremath{\\mathfrak{B}}"),
    ("Cf", "\\ensuremath{\\mathfrak{C}}"),
    ("Df", "\\ensuremath{\\mathfrak{D}}"),
    ("Ef", "\\ensuremath{\\mathfrak{E}}"),
    ("Ff", "\\ensuremath{\\mathfrak{F}}"),
    ("Gf", "\\ensuremath{\\mathfrak{G}}"),
    ("Hf", "\\ensuremath{\\mathfrak{H}}"),
    ("If", "\\ensuremath{\\mathfrak{I}}"),
    ("Jf", "\\ensuremath{\\mathfrak{J}}"),
    ("Kf", "\\ensuremath{\\mathfrak{K}}"),
    ("Lf", "\\ensuremath{\\mathfrak{L}}"),
    ("Mf", "\\ensuremath{\\mathfrak{M}}"),
    ("Nf", "\\ensuremath{\\mathfrak{N}}"),
    ("Of", "\\ensuremath{\\mathfrak{O}}"),
    ("Pf", "\\ensuremath{\\mathfrak{P}}"),
    ("Qf", "\\ensuremath{\\mathfrak{Q}}"),
    ("Rf", "\\ensuremath{\\mathfrak{R}}"),
    ("Sf", "\\ensuremath{\\mathfrak{S}}"),
    ("Tf", "\\ensuremath{\\mathfrak{T}}"),
    ("Uf", "\\ensuremath{\\mathfrak{U}}"),
    ("Vf", "\\ensuremath{\\mathfrak{V}}"),
    ("Wf", "\\ensuremath{\\mathfrak{W}}"),
    ("Xf", "\\ensuremath{\\mathfrak{X}}"),
    ("Yf", "\\ensuremath{\\mathfrak{Y}}"),
    ("Zf", "\\ensuremath{\\mathfrak{Z}}"),

    ("Ac", "\\ensuremath{\\mathcal{A}}"),
    ("Bc", "\\ensuremath{\\mathcal{B}}"),
    ("Cc", "\\ensuremath{\\mathcal{C}}"),
    ("Dc", "\\ensuremath{\\mathcal{D}}"),
    ("Ec", "\\ensuremath{\\mathcal{E}}"),
    ("Fc", "\\ensuremath{\\mathcal{F}}"),
    ("Gc", "\\ensuremath{\\mathcal{G}}"),
    ("Hc", "\\ensuremath{\\mathcal{H}}"),
    ("Ic", "\\ensuremath{\\mathcal{I}}"),
    ("Jc", "\\ensuremath{\\mathcal{J}}"),
    ("Kc", "\\ensuremath{\\mathcal{K}}"),
    ("Lc", "\\ensuremath{\\mathcal{L}}"),
    ("Mc", "\\ensuremath{\\mathcal{M}}"),
    ("Nc", "\\ensuremath{\\mathcal{N}}"),
    ("Oc", "\\ensuremath{\\mathcal{O}}"),
    ("Pc", "\\ensuremath{\\mathcal{P}}"),
    ("Qc", "\\ensuremath{\\mathcal{Q}}"),
    ("Rc", "\\ensuremath{\\mathcal{R}}"),
    ("Sc", "\\ensuremath{\\mathcal{S}}"),
    ("Tc", "\\ensuremath{\\mathcal{T}}"),
    ("Uc", "\\ensuremath{\\mathcal{U}}"),
    ("Vc", "\\ensuremath{\\mathcal{V}}"),
    ("Wc", "\\ensuremath{\\mathcal{W}}"),
    ("Xc", "\\ensuremath{\\mathcal{X}}"),
    ("Yc", "\\ensuremath{\\mathcal{Y}}"),
    ("Zc", "\\ensuremath{\\mathcal{Z}}"),

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
    ("nbsp", "~"),

    ("today", "\\text{\\today}"),
    ("TeX", "\\text{\\TeX}"),
    ("LaTeX", "\\text{\\LaTeX}"),
    ("LaTeXe", "\\text{\\LaTeXe}")]

knownCommands
 = ["newpage", "vfill", "hfill", "normalsize", "normalfont",
    "noindent", "tableofcontents",

    "tiny", "scriptsize", "footnotesize", "small",
    "large", "Large", "LARGE", "huge", "Huge",

    "rmfamily", "sffamily", "ttfamily", "mdseries", "bfseries",
    "upshape", "itshape", "slshape", "scschape", "em"]

specialCommands
 = ["pagebreak" ~> "\\newpage",
    "italic" ~> "\\itshape",
    "bold" ~> "\\bfseries",
    "right" ~> "\\raggedleft",
    "left" ~> "\\raggedright",
    "center" ~> "\\centering",
    "reset" ~> reset,
    "pagestyle" ~> (\[x] -> "\\pagestyle{" ++ x ++ "}"),
    "thispagestyle" ~> (\[x] -> "\\thispagestyle{" ++ x ++ "}")]

showSymbols = do
    let symbols = sort knownSymbols
    let p x = "\n>   " ++ "\\ " ++ x ++ "\n>   \\" ++ x
    let f a b
         | b == 4 = p a ++ "\n--"
         | True   = p a
    writeFile "doc/Symbols.simple" $ concat $ zipWith f symbols (cycle [1,2,3,4])

