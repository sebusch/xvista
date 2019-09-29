C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       These are the Tektronix terminal modes
	integer         ALPHA,GRAFIX,ANSI
	parameter       (ALPHA = 0, GRAFIX = 1, ANSI = 2)
C
	character       ff, ec, gs, us
	integer         mode
	character       outbuf(5)
	common  /t4010/ mode,outbuf,ff,ec,gs,us
