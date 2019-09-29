man-html:man-ps
	latex2html -no_navigation -info "0" -address " " man
	-rm -rf ../help/man
	mv man ../help
	(cd ../help/man; ../link node*.html)

man-ps:
	latex man
	makeindex man.idx
	latex man
	dvips man

clean:
	rm -f *.dvi *.log *.aux *.idx unpack
	-gzip man.ps
