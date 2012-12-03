module Simplex.Config where

import Simplex.Parser

config (Document blocks props) = findToC blocks

findToC (BCommand "tableofcontents" _ : _) = True
findToC (x : xs) = findToC xs
findToC _ = False

doNumberSections x = x
dontNumberSections = not . doNumberSections

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
    "noindent", "tableofcontents",

    "tiny", "scriptsize", "footnotesize", "small",
    "large", "Large", "LARGE", "huge", "Huge",

    "rmfamily", "sffamily", "ttfamily", "mdseries", "bfseries",
    "upshape", "itshape", "slshape", "scschape", "em"]

specialCommands
 = [("pagebreak", "newpage")]

