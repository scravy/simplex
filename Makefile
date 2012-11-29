all: X.pdf A7.pdf

%.pdf: %.tex mktex.hs Simplex.hs
	pdflatex $^
	pdflatex $^

%.tex: %.simple
	cat $^ | runhaskell mktex.hs > $@

clean:
	rm -rf *.log *.aux *.out *.tex

purge: clean
	rm -rf *.pdf

