C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c                       A location which is probably off the paper
	integer         OFFTHEPAGE
	parameter       (OFFTHEPAGE = -100000000)
C
	character*5     SEMI
	parameter       (SEMI = '('';'')')
	character*4     IN0
	parameter       (IN0 = '(a2)')
	character*7     IN1
	parameter       (IN1 = '(a2,i6)')
	character*14    IN2
	parameter       (IN2 = '(a2,i6,'','',i6)')
	character*21    IN3
	parameter       (IN3 = '(a2,i6,'','',i6,'','',i6)')
	character*28    IN4
	parameter       (IN4 = '(a2,i6,'','',i6,'','',i6,'','',i6)')
	character*15    PAR2
	parameter       (PAR2 = '('','',i6,'','',i6)')
c
c                       current location of pen (true device coords)
	integer         ixp, iyp
	character*80    hpstring
	common  /mgohpgl/       ixp, iyp, hpstring
