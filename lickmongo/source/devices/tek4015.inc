C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This common is set up by the call to t4015setup
	common/tek4015/mode,gs,us,ec,ff,ixp,iyp,startit,lowx,lowy,highx,
     2          highy
	integer*4 mode
	character*1 gs,us,ec,ff
	logical startit
	integer highx, highy, lowx, lowy
C       mode = 0 -> character
C       mode = 1 -> graphics
C       ixp     x pen position
C       iyp     y pen position

