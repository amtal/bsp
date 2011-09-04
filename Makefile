all: bsp.pdf

bsp.pdf: src/bsp.tex
	pdflatex -interaction nonstopmode src/bsp.tex

src/bsp.tex: src/bsp.nw
	cd src;noweb bsp.nw
