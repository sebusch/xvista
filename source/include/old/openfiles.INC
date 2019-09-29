#include "Vista.h"
C       CABAL opened files data

	INTEGER         MAXOPEN
	PARAMETER       (MAXOPEN=10)
	INTEGER         MAXSKIP
	PARAMETER       (MAXSKIP=100)

C                       The file names
	CHARACTER*80    OPENFILES(MAXOPEN)
C                       Their logical names
	CHARACTER*10    OPENNAMES(MAXOPEN)
C                       Current input line from file
	CHARACTER*500   CURLINE(MAXOPEN)
C                       True if opened
	LOGICAL         ISOPENED(MAXOPEN)
C                       True if have an input line
	LOGICAL         HAVELINE(MAXOPEN)
C                       FORTRAN channel numbers
	INTEGER         CHANNUM(MAXOPEN)
C                       Current line number
	INTEGER*2       LINENUM(MAXOPEN)
C                       Line skip list
	INTEGER*2       SKIPLINES(MAXSKIP,MAXOPEN)
C                       Initial index into SKIPLINES
	INTEGER*2       LASTSKIP(MAXOPEN)
C                       Skip if expression is true
	CHARACTER*80    SKIPTEST(MAXOPEN)

	COMMON/FILEC/ OPENFILES,OPENNAMES,CURLINE
	COMMON/FILEL/ ISOPENED,HAVELINE,CHANNUM,LINENUM,SKIPLINES,
     .          LASTSKIP
	COMMON/FILES/ SKIPTEST

