all: X.pdf 

%.pdf: %.tex mktex.hs Simplex.hs
	pdflatex $^
	pdflatex $^

%.tex: %.simple
	cat $^ | runhaskell mktex.hs > $@

install:
	cabal install --prefix=${HOME}/Repository --user

clean:
	rm -rf *.log *.aux *.out *.tex *.toc

purge: clean
	rm -rf *.pdf *.png *.gif *.jpg *.dot

