#
#       This makefile is copied from the Lick Mongo manual
#       It has not been fully converted, the only thing it does now
#       is to pack up this directory for anonymous ftp.
#
#       Define where the compressed tar file will be made
COMPTAR=/usr2/vista/vistacook.tar
#       Some new suffixes are required for dealing with LaTeX and TeX
.SUFFIXES: .dvi .tex .aux .toc
TeXFILES=cook_intro.tex cook_tutor.tex cook_basic.tex cook_2d.tex \
cook_spec.tex cook_ham.tex cook_dirt.tex cook_appa.tex cook_appb.tex \
cook_appc.tex cook_master.tex
AUXFILES=cook_master.aux
# we have a special version of LaTeX which uses PostScript fonts
LATEX=latex
# rule for converting a .tex file into a .dvi file
.tex.dvi: ; -$(LATEX) $*.tex
# rule for creating a .aux file
.tex.aux: ; -$(LATEX) $*.tex
#
# default target is the whole Lick Vista Cookbook
#
#       In order to properly make the Lick Mongo manual, LaTeX must be
#       run iteratively over the inputs.  Note that in order to insure that
#       the Table of Contents gets built properly, you MUST run LaTeX
#       at least twice after any major change to the text.
#       This may be done by repeatedly typing "make" until
#       1) "make" performs no further action
#       or
#       2) you are tired of iterating.
#
default::cook_master.dvi
.PRECIOUS: cook_master.toc # mongoindx.tex
# targets for starting over from scratch
clean:
	-rm *.dvi *.aux *.toc *.idx *.log *.ilg *.ps \#*
tidy:
	-rm *.dvi *.log mongo.ps \#*
#
#       LaTeX the manual
cook_master.dvi: cook_master.aux
#       makeindex mongo.idx
#       -rm mongoindx.new
#       cat index.beg mongo.ind index.end > mongoindx.new
#       ./newindex
#
cook_master.aux: $(TeXFILES) cook_master.toc
#
mongoindx.tex:
	./newindex
#
#       Insure that we run LaTeX twice the first time.
cook_master.toc:
	-$(LATEX) cook_master
#       makeindex mongo.idx
#       -rm mongoindx.new
#       cat index.beg mongo.ind index.end > mongoindx.new
#       ./newindex
#
#       This is pretty much local to Lick, you can ignore it.
cook_master.ps:
	pubps -o nissho.opt cook_master.dvi
#
#       Reverse the pages if desired
reverse:
	-rm cook_rev.ps
	psrev cook_master.ps > cook_rev.ps
#
#       Shovel all this directory up for anonymous ftp
comptar: tidy
	-rm -f $(COMPTAR) $(COMPTAR).Z
	(cd .. ; tar cf $(COMPTAR) cookbook)
	compress $(COMPTAR)
#
#
#       Process the LaTeX generated index almost into the right form.
#       If you use this, there is still some hand-editing required.
#       This is a fallback for sites which do not have "MakeIndex".
#       "MakeIndex" is a publicly-available program which can be
#       found at most net-archives of TeX paraphernalia.
#       We got ours from LaBrea.Stanford.EDU
#mongoindx.almost: mongo.dvi
#        -rm mongo.idx.sort
#        cat mongo.idx index.mid | sort -f | sed -f index.sed > mongo.idx.sort
#        cat index.beg mongo.idx.sort index.end > mongoindx.almost
