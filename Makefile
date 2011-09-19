all: bsp.pdf ebin/bsp.beam

ebin/bsp.beam: src/bsp.lfe
	rebar compile skip_deps=true

bsp.pdf: src/bsp.tex
	pdflatex -interaction nonstopmode src/bsp.tex

src/bsp.tex: src/bsp.nw
	cd src;noweb bsp.nw

git:
	git add src/*.lfe src/bsp.nw README.md;git commit
