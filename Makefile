all: X.pdf 

%.pdf: %.tex mktex.hs Simplex.hs
	pdflatex $^
	pdflatex $^

%.tex: %.simple
	cat $^ | runhaskell mktex.hs > $@

clean:
	rm -rf *.log *.aux *.out *.tex *.toc

purge: clean
	rm -rf *.pdf *.png

