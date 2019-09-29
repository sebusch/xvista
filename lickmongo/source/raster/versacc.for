	SUBROUTINE VERSACC
C
C       This routine is called to charge for a 1 page versatek plot
C       to the caller.
C
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT INTEGER*4 (S)
	EXTERNAL    ACC$K_INSMESG
	INTEGER*2 BUFFER(9)
	INTEGER*4 DESC(2)
	INTEGER*4 IB(3)
	EQUIVALENCE (BUFFER(4),IB)
	DATA BUFFER/0,357,7*0/
	BUFFER(1) = %LOC(ACC$K_INSMESG)
	IB(1) = 0
	IB(2) = 0
	IB(3) = 8
	DESC(1) = 18
	DESC(2) = %LOC(BUFFER)
c       CALL IFERR(SYS$SNDACC(DESC,),
c    &     'WRTLPACC UNABLE TO WRITE ACCOUNTING MESSAGE')
	RETURN
	END
