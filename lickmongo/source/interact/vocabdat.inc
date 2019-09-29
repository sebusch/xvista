C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       The declarations for the variables referenced here are in vocab.inc .
C       This is the compiled in vocabulary of Lick Mongo.
C       There should be NBUILTIN non-blank initializations here, plus
C       the macro '*all'.  See vocab.inc for the value of NBUILTIN.
C       Vocabulary items with '*' as the first character are not
C       inserted into the interactive command buffer.
	data    (vocab(lc),lc=1,10) /
     &          ' angle',
     &          ' autolweight',
     &          ' axis',
     &          ' box',
     &          ' chdir',
     &          ' color',
     &          ' connect',
     &          ' contour',
     &          ' data',
     &          ' device'/
	data    (vocab(lc),lc=11,20) /
     &          ' dot',
     &          ' draw',
     &          ' dxcolumn',
     &          ' dycolumn',
     &          ' ecolumn',
     &          ' errorbar',
     &          ' expand',
     &          ' grid',
     &          ' halftone',
     &          ' histogram'/
	data    (vocab(lc),lc=21,30) /
     &          ' id',
     &          ' image',
     &          ' input',
     &          ' label',
     &          ' levels',
     &          ' limits',
     &          ' lines',
     &          ' location',
     &          ' ltype',
     &          ' lvisibility'/
	data    (vocab(lc),lc=31,40) /
     &          ' lweight',
     &          ' margins',
     &          ' maxdigit',
     &          ' minmax',
     &          ' notation',
     &          ' page',
     &          ' pcolumn',
     &          ' physical',
     &          ' pl3box',
     &          ' plt3d'/
	data    (vocab(lc),lc=41,50) /
     &          ' points',
     &          ' psfmode',
     &          ' psfname',
     &          ' ptype',
     &          ' putlabel',
     &          ' quarter',
     &          ' rdstring',
     &          ' relocate',
     &          ' reset',
     &          ' rlabel'/
	data    (vocab(lc),lc=51,60) /
     &          ' rlimits',
     &          ' scontour',
     &          ' skipchars',
     &          ' square',
     &          ' submargins',
     &          ' tcontour',
     &          ' ticksize',
     &          ' tlabel',
     &          ' vector',
     &          ' vfield'/
	data    (vocab(lc),lc=61,70) /
     &          ' window',
     &          ' xcolumn',
     &          ' xlabel',
     &          ' xlimits',
     &          ' xlogarithm',
     &          ' xsixty',
     &          ' xspline',
     &          ' ycolumn',
     &          ' ylabel',
     &          ' ylimits'/
	data    (vocab(lc),lc=71,80) /
     &          ' ylogarithm',
     &          ' ysixty',
     &          '*curses',
     &          '*define',
     &          '*delete',
     &          '*edit',
     &          '*end',
     &          '*erase',
     &          '*hardcopy',
     &          '*help'/
	data    (vocab(lc),lc=81,90) /
     &          '*imlong',
     &          '*imprint',
     &          '*insert',
     &          '*list',
     &          '*pause',
     &          '*playback',
     &          '*printronix',
     &          '*pslandscape',
     &          '*psportrait',
     &          '*read'/
	data    (vocab(lc),lc=91,95) /
     &          '*show',
     &          '*terminal',
     &          '*versatec',
     &          '*vtlong',
     &          '*write'/
C       The following commands are new, not documented, and somewhat
C       experimental for the moment.
	data    (vocab(lc),lc=96,NBUILTIN) /
     &          '*reverse',
     &          '*x11geometry',
     &          ' softphysical',
     &          ' postscript',
     &          ' polygon',
     &          ' wxfrac', ' wyfrac',
     &          ' rectangle',
     &          ' enum',
     &          '*undefine',
     &          ' derivative', ' integral', ' mathop',
     &          ' strings', ' rcstring',
     &          '*epslandscape', '*epsportrait', ' set', ' echo'
C       INSERT ANY NEW COMMANDS JUST _ABOVE_ THIS LINE
     &          /
C       Lick Mongo requires that the (NBUILTIN+1st) entry in VOCAB be '*all'
	data    vocab(NBUILTIN+1)       /'*all'/
C       The last USERDEF commands are blank at compile time and filled
C       dynamically during run whenever a Lick Mongo "macro" is defined.
	data    (vocab(lc),lc=NBUILTIN+2,MAXCOM)
     &          /USERDEF * '              '/
