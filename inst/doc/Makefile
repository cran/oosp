makeall:
	make makedev
	make clean

makedev:
	make oosp1.pdf
	make oosp2.pdf
	make oosp3.pdf

oosp1.pdf: oosp1.tex maia.sty
	pdflatex oosp1.tex

oosp2.pdf: oosp2.tex maia.sty
	pdflatex oosp2.tex

oosp3.pdf: oosp3.tex maia.sty
	pdflatex oosp3.tex

oosp1.tex: oosp1.rnw
	R CMD Sweave oosp1.rnw

oosp2.tex: oosp2.rnw
	R CMD Sweave oosp2.rnw

oosp3.tex: oosp3.rnw
	R CMD Sweave oosp3.rnw

clean:
	rm *.tex
	rm *.aux
	rm *.log
	rm tmp*
	rm Rplots*

