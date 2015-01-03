Simple LaTeX.

Current version is v0.3.7, as of 2013-05-06.

See Simplex-v0.3.7.pdf for more information
and more detailed installation instructions.

Installing
==========

In order to build simplex you need the Haskell Platform.
You can download it at http://www.haskell.org/platform/ 

You can use the Haskell Cabal to install the package,
for example like so:

    cabal install simplex

This is similar to `./configure; make; make install`.

simplex requires a latex distribution which provides the
`pdflatex` command. TeX Live is recommended, as it provides
all packages that simplex uses.

For certain features you will also need `graphviz` and
`ImageMagick`, but simplex will run without.

On Ubuntu
---------

On Ubuntu you will have to install the following packages:

    haskell-platform
    texlive
    texlive-latex-extra
    texlive-math-extra
    latex-xcolor

You should also install:

    graphviz

You might need to run `cabal update` once. For a global
installation use `sudo cabal install --global`. This will
install the `simplex` executable in a folder on your $PATH.

On Mac OS X
-----------

For Mac OS X there is Mac TeX (http://www.tug.org/mactex/),
which should include all relevant packages. Graphviz can be
obtained from http://www.graphviz.org/Download_macos.php .

If you are using Homebrew (http://mxcl.github.com/homebrew/)
you might want to install ImageMagick and Graphviz like so:

    brew install graphviz
    brew install imagemagick


Using Simplex
=============

simplex will automatically process all files in the
current working directory, but you may also specify
which files to process. Here is what simplex will tell
you when you ask it for help (`--help` or `-h`):

    simplex [options] [files...]

      -h        --help              Print this help text.
                --version           Print version information.
      -v        --verbose           Verbose output.
      -d        --dry-run           Dry run (do not create any files).
      -n        --no-clean          Do not clean up after building.
      -p        --print             Print processed tex to stdout.
      -c        --crop              Crops the document so that no margins are left.
      -f        --force             Forces the creation of output files.
      -t , -T   --type=             Specify type of output (pdf, png, tex)
      -x        --pdflatex=         Path to `pdflatex' executable
      -k        --pdfcrop=          Path to `pdfcrop'
      -z        --graphviz=         Path to `dot' (graphviz)
      -g        --gnuplot=          Path to `gnuplot'
      -m        --convert=          Path to `convert' (ImageMagick)
      -w[]      --watch[=]          Watch files or folder (optionally amount of time in ms)
      -3        --three-times       Execute `pdflatex' three times instead of the default two times.
                --density=, --dpi=  For output type `png' only, specifies dpi.
                --quality=          For output type `png' only, specifies quality.


