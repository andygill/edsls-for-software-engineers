
build::
	pdflatex paper.tex


all::
	pdflatex paper.tex
	bibtex paper
	pdflatex paper.tex
	pdflatex paper.tex

wc::
	pandoc paper.tex -t plain | wc -w

