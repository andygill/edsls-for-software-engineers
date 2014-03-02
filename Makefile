
build::
	pdflatex paper.tex

wc::
	pandoc paper.tex -t plain | wc -w

