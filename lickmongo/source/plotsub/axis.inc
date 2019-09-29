C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       Parameter Definitions for AXIS.For
	integer         IFORMAT
	parameter       (IFORMAT = 2)
	integer         GFORMAT
	parameter       (GFORMAT = 0)
	integer         EFORMAT
	parameter       (EFORMAT = 1)
	integer         FFORMAT
	parameter       (FFORMAT = -1)
	integer         TMINOR
	parameter       (TMINOR = 1)
	integer         TMAJOR
	parameter       (TMAJOR = 0)
	integer         JLEFT
	parameter       (JLEFT = 0)
	integer         JCENTR
	parameter       (JCENTR = 1)
	integer         JRIGHT
	parameter       (JRIGHT = 2)
	integer         LPERPEND
	parameter       (LPERPEND = 2)
	integer         LPARALLEL
	parameter       (LPARALLEL = 1)
	integer         GHORIZ
	parameter       (GHORIZ = 1)
	integer         GVERTI
	parameter       (GVERTI = 0)
	integer         CLKW, CCLKW
	parameter       (CLKW = 1, CCLKW = 0)
	integer         APMIN, APMAX
	parameter       (APMIN = 1, APMAX = 5)
C------------------------------------------------------------------------------
c       EXPNOT tells the limiting values of axis tick labels to be expressed
c       in exponential notation (numbers between the limits will be expressed
c       in floating point or integer notation)
	real            xlexp, xhexp, ylexp, yhexp
	common  /expnot/ xlexp, xhexp, ylexp, yhexp
C------------------------------------------------------------------------------
c       MAXDIGIT is used by SETDIGITS and TICKLABEL to control how many digits
c       are plotted on the axis labels
	integer         maxdigit
	common  /ndigit/ maxdigit
C------------------------------------------------------------------------------
c       Common SIX consists of variables to control which if either of the axes
c       are to be plotted "base 60"  (i.e. for R.A., Dec, Time, ...)
	integer         ndecimx, ndecimy, ndecim
	logical         xsixty, ysixty, sixties
	common  /six/   ndecimx, ndecimy, ndecim,
     &                  xsixty, ysixty, sixties
C------------------------------------------------------------------------------
