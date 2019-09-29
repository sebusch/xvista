#include "Config.h"
c       define DEBUG
C==============================================================================
C==============================================================================
	subroutine PMGO(setdigit)(iwant)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'axis.inc'
C       External Variable
	integer         iwant
C       Executable Code
	maxdigit = iwant
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(setexp)(arg1,arg2,arg3,arg4)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'axis.inc'
C       External Variables
	real            arg1, arg2, arg3, arg4
C       Executable Code
	xlexp = arg1
	xhexp = arg2
	ylexp = arg3
	yhexp = arg4
	return
	end
C==============================================================================
C==============================================================================
#       ifdef VMS
	options /g_floating
#       endif /* VMS */
	subroutine PMGO(axis)(sa1,sa2,asmall,abig,ax,ay,alen,
     &  ilabel,iclock)
C       ---------------------
C       Makes an axis labelled from SA1 to SA2 at location AX, AY, length ALEN
c       (You figure out what "length" means if device pixels are not square!)
C       ANGLE determines the angle of the axis.
c       (Again, just what does "angle" mean if device pixels are not square?)
C       if ABIG > 0 use that for spacing of large ticks.
C       if ASMALL < 0 make a logarithmic axis, if ASMALL = 0, do the default
C       if ASMALL > 0 try to use that for the spacing of small ticks.
C       ILABEL is -1 for no axis at all, 0 for ticks but no labels,
C       1 for labels parallel to axis, 2 for labels perpendicular to axis,
C       3 for labels parallel but no ticks, 4 for labels perp. but no ticks
C       5 for a line "to close the box" and nothing else.
C       ICLOCK = 1 for clockwise ticks on the axis, 0 for counter
C       ---------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'axis.inc'
C       External Variables
c                       user coordinates of the axis endpoints
	real            sa1, sa2
c                       tick spacing
	real            asmall, abig
c                       device coordinates of begin-point of axis
	real            ax, ay
c                       "length" of axis in device units
	real            alen
	integer         ilabel, iclock
C       Internal Variables
	real            par(APMIN:APMAX)
	real            ay2, ax2
	external        PTUV(ticklabel), PTUV(tickdraw)
	real*8          a1, a2
C       Executable Code
	if (asmall .lt. 0) then
c           the axis limits are logarithmic
	    if (abs(sa1) .le. 308. .and. abs(sa2) .le. 308.) then
#               ifdef sun386i
c               Some aspects of the Sun386i Fortran are incredibly broken...
		a1 = 10.D0**dble(sa1)
		a2 = 10.D0**dble(sa2)
#               else  /* sun386i */
		a1 = 10.D0**sa1
		a2 = 10.D0**sa2
#               endif /* sun386i */
	    else
		write(STDERR,*) ' Logarithmic axis limit too big.'
		return
	    endif
	else
c           the axis limits are not logarithmic
	    a1 = sa1
	    a2 = sa2
	endif
	ax2 = alen*cosang + ax
	ay2 = alen*sinang + ay
#       ifdef _PostScript
	if (ipsopen) call PTUV(psclip)(.false.)
#       endif /* _PostScript */
c       if this is supposed to draw numerals only, we do not draw the line
	if (ilabel .ne. 3 .and. ilabel .ne. 4)
     &  call PMGO(line)(ax,ay,ax2,ay2)
c       if this is supposed to draw a line only, we are already done
	if (ilabel .gt. 4) return
	par(2) = ilabel
	if (par(2) .le. 2) then
c           go and draw the ticks along the axis
	    par(1) = iclock
	    call PTUV(axistick)(a1,a2,asmall,abig,ax,ay,alen,
     &      par, PTUV(tickdraw))
	else
c           we do not want ticks, but we do want labels
	    par(2) = nint(par(2)) - 2
	endif
	if (par(2) .gt. 0) then
	    if (par(2) .eq. LPARALLEL) then
c               labels are parallel to the axis
		par(1) = JCENTR
	    elseif (par(2) .eq. LPERPEND) then
c               labels are perpendicular to the axis
		par(1) = 2 * iclock
	    endif
	    par(3) = iclock
	    if (angle .le. 45.) then
c               The axis is most nearly horizontal, and not inverted.
c               So we assume that axes at negative angles do not occur, and
c               that this axis is an X-axis.
c               Good grief, what a poor assumption!
		sixties = xsixty
		if (xsixty) ndecim = ndecimx
	    else
c               Whatever the orientation angle of this axis, it is
c               assumed to be a Y-axis.  Another poor assumption.
		sixties = ysixty
		if (ysixty) ndecim = ndecimy
	    endif
#           ifdef OLD
	    par(4) = GFORMAT
#           else  /* OLD */
	    par(4) = IFORMAT
#           endif /* OLD */
	    call PTUV(axistick)(a1,a2,asmall,abig,ax,ay,alen,
     &      par,PTUV(ticklabel))
	    sixties = .false.
	endif
	return
	end
C==============================================================================
C==============================================================================
#       ifdef VMS
	options /g_floating
#       endif /* VMS */
	subroutine PTUV(axistick)(a1,a2,asmall,abig,ax,ay,alen,par,func)
C       ---------------------
C       Makes an axis labelled from A1 to A2 at location AX, AY, length ALEN
C       if ASMALL < 0 make a logarithmic axis, if ASMALL = 0, do the default
C       if ASMALL > 0 try to use that for the spacing of small ticks.
C       if ABIG > 0 use that for spacing of large ticks.
C       Passes the x,y positions, the label, the tick size, and
C       the parameters in PAR to the function FUNC.
C       ANGLE determines the angle of the axis.
C       ---------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'axis.inc'
C       Parameter
	integer         MAXBIGTICK
	parameter       (MAXBIGTICK = 100)
C       External Variables
	real*8          a1, a2
	real            asmall, abig
	real            ax, ay, alen
	real            par(APMIN:APMAX)
	integer         func
	external        func
C       Internal Variables
c                       internal place for keeping user limits a1, a2
	real*8          ax1, ax2
c                       the interval between ticklabels in user units
	real*8          step
c                       user coordinate value of previous ticklabel
	real*8          tick
c                       user coordinate value at a tick
	real*8          value
c                       number of small ticks between ticklabels
	integer         nsmall
c                       expected maximum number of characters in a ticklabel
	integer         nch
c                       number of ticklabels which will be made along the axis
	integer         nlabel
	real            diff
	integer         iexp, num, off
	real            amant
	integer         i, j
c                       "distance" along the axis of a tick (if pix are square)
	real            xlen
c                       device coordinates of intersection of a tick & the axis
	real            xpos, ypos
	integer		ierr
C       Executable Code
	if (a1 .eq. a2 .or. expand .eq. 0.) return
	ax1 = a1
	ax2 = a2
	if (abig .eq. 0 .and. asmall .ge. 0) then
c           user has given no hints, Lick Mongo decides its own tick spacings
	    if (sixties) then
c               allot 12 characters as the width of a ticklabel
		nch = 12
	    else
c               allot 10 characters as the width of a ticklabel
		nch = 10
	    endif
c           Determine roughly how many ticklabels will fit along this axis, ...
	    nlabel = nint(alen/(nch*cwidth*expand))
c           ... but always try for at least 4 ticklabels
	    nlabel = max(nlabel,4)
c           Estimate the number of logarithmic decades between ticklabels
	    diff = log10(abs(a2 - a1)/nlabel)
c           Find the greatest integer which is less than diff.
c           Note that iexp is our initial guess at the least significant
c           digit which varies from label to label.  We may revise it later.
	    iexp = diff
	    if (diff .lt. 0) iexp = iexp - 1
c           Determine the fractional part of the estimated number of decades
	    amant = diff - iexp
c           Based upon this estimated fraction, pick the actual tick spacing
	    nsmall = 5
	    if (sixties .and. amant .le. 29) then
		num = 5
	    elseif (sixties .and. amant .le. 800) then
		num = 6
		nsmall = 4
	    elseif (amant .lt. .15) then
c               10.**.15 = 1.41
c               choose a step of 1 in the decimal digit that changes
		num = 1
	    elseif (amant .lt. .50) then
c               10.**.5 = 3.16
c               choose a step of 2 in the decimal digit that changes
		num = 2
		nsmall = 4
	    elseif (amant .lt. .85) then
c               10.**.85 = 7.08
c               choose a step of 5 in the decimal digit that changes
		num = 5
	    else
c               decide that we might as well step by 10, which really means 1
		iexp = iexp + 1
		num = 1
	    endif
	    par(5) = iexp
c           Note that iexp is very important, because it tells us
c           the least significant digit that we will need to display.
	    step = (num * 10.D0**iexp)
#           ifdef ISI
C           ISI f77 gives bus error unless we use sngl()!!!!!
	    if (asmall .gt. 0) nsmall = abs(nint(sngl(step/asmall)))
#           else  /* ISI */
	    if (asmall .gt. 0) nsmall = abs(nint(step/asmall))
#           endif /* ISI */
	elseif (abig .gt. 0 .and. asmall .gt. 0) then
c           user has specified the desired tick spacings, so use them
	    step = abig
#           ifdef ISI
C           ISI f77 gives bus error unless we use sngl()!!!!!
	    nsmall = abs(nint(sngl(step/asmall)))
#           else  /* ISI */
	    nsmall = abs(nint(step/asmall))
#           endif /* ISI */
	    diff = log10(abs(asmall))
c           find the greatest integer which is less than diff
	    iexp = diff
	    if (diff .lt. 0) iexp = iexp - 1
	    par(5) = iexp
	elseif (asmall .lt. 0) then
c           user has requested logarithmic axis
	    if (ax1 .le. 0 .or. ax2 .le. 0) return
	    ax1 = log10(ax1)
	    ax2 = log10(ax2)
	    step = 1.D0
	    nsmall = 10
	endif
c       ok, we have now determined step and nsmall for further use
C       fudge AX1 and AX2 to allow for unfavorable integer roundoff
	ax1 = ax1 - 1.D-6*(ax2-ax1)
	ax2 = ax2 + 1.D-6*(ax2-ax1)
c       always start close to the user begin-point and move away
	if (ax2 .lt. ax1) step = -step
c       set tick to the user value of the previous ticklabel that would
c       have been done just before or at the user begin-point
c       again, we use the "greatest integer which is less than" function
	off = ax1 / step
	if (ax1/step .lt. 0.) off = off - 1
	tick = step * off
c       start making all the ticks
	do 20 j = 1,MAXBIGTICK
	  if (asmall .lt. 0.) then
c             logarithmic axes only have 1 significant digit
	      par(5) = nint(tick)
	  endif
	  do 15 i = 1,nsmall
#           ifdef INTERRUPT_HANDLE
	    if (nogo) goto 999
#           endif /* INTERRUPT_HANDLE */
	    if (asmall .ge. 0.) then
c               we are doing a linear axis
c               determine the numerical value of user coords at this tick
		value = tick + (step*(i-1))/nsmall
c               the next line should not be necessary with a smart number subr
c               if (abs(value) .lt. abs(step*1.D-12)) value = 0.D0
c               determine how far along the axis to place this tick
		xlen = alen*(value-ax1)/(ax2-ax1)
	    else
c               we are doing a logarithmic axis
		if (ax2 .gt. ax1) then
		    value = i
		else
c                   fixes a bug in negative logarithmic axes
		    value = (11-i)
		endif
c               determine how far along the axis to place this tick
		xlen = alen*((log10(value) + tick)-ax1)/(ax2-ax1)
c               determine the numerical value of user coords at this tick
		value = value * 10.D0**nint(tick)
	    endif
	    if (xlen .gt. alen) goto 999
	    if (xlen .ge. -1) then
		xpos = xlen*cosang + ax
		ypos = xlen*sinang + ay
c               Major ticks occur when (i .eq. 1)
		ierr = func(xpos,ypos,value,(i .eq. 1),par)
	    endif
15        continue /* loop over small ticks */
c         set tick up to the user value of the next ticklabel
	  tick = tick + step
20      continue /* loop over large labelled ticks */
999     continue
	return
	end
C==============================================================================
C==============================================================================
#       ifdef VMS
	options /g_floating
#       endif /* VMS */
	subroutine PTUV(ticklabel)(x,y,value,major,par)
c       ANGLE determines the angle of the axis labels
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'axis.inc'
C       External Variables
c                       device coords of point where this tick intersects axis
	real            x, y
C                       VALUE is the number to write for this ticklabel
	real*8          value
	logical         major
C       PAR(1) is 0, 1, 2 for label to be left, center, or right justified
C       PAR(2) = 1 for labels parallel to axis, and 2 for perpendicular to axis.
C       PAR(3) = 0 for clockwise labels on the axis, 1 for counter
C       PAR(4) = 0 for default notation (i.e. floating point if 1e-4<value<1e4,
C                exponential otherwise), 1 for exponential only
c       par(5) tells what the least significant digit is
	real            par(APMIN:APMAX)
C       Internal Variables
	real            savang, savcos, savsin
	real            standoff
	integer         nchar, notation
c                       position on which label is centered
	real            xs, ys
	character*40    string
	logical         tuvify
C       External Function
	integer PMGO(lenc)
C       Executable Code
	if (.not. major) return
c       convert VALUE into a string
	if (sixties) then
	    nchar = 11 + ndecim
	    call PTUV(cflab)(string,value,nchar,ndecim)
	else
	    notation = nint(par(4))
#           ifdef OLD
	    if (value .eq. 0.D0 .and. notation .eq. IFORMAT) then
c               do not muck with the notation format
#               ifdef DEBUG
		write(92,*) 'value 0 and notation I'
#               endif /* DEBUG */
	    elseif (angle .le. 45.) then
#           else /* OLD */
	    if (abs(mod(angle,180.)) .le. 45.) then
#           endif /* OLD */
c               make the blind assumption that this is the X-axis
		if (abs(value).le.xlexp .or. abs(value).ge.xhexp)
     &          notation = EFORMAT
#               ifdef DEBUG
		write(92,*) 'x[lh]exp, aval', xlexp, xhexp, abs(value)
#               endif /* DEBUG */
	    else
c               make the blind assumption that this is the Y-axis
		if (abs(value).le.ylexp .or. abs(value).ge.yhexp)
     &          notation = EFORMAT
#               ifdef DEBUG
		write(92,*) 'y[lh]exp, aval', ylexp, yhexp, abs(value)
#               endif /* DEBUG */
	    endif
#           ifdef DEBUG
	    write(92,*) 'notation', notation
#           endif /* DEBUG */
	    tuvify = .true.
#           ifdef OLD
	    call PTUV(number)(value,nchar,string,
     &      notation,tuvify,maxdigit)
#           else /* OLD */
	    call PTUV(number)(value,nint(par(5)),string,notation,
     &      .true.,tuvify)
	    nchar = PMGO(lenc)(string)
#           endif /* OLD */
	endif
c       We need this amount of distance to keep the exponents from touching
c       the axis when labelling the bottom axis ...
	standoff = cheight * expand
	if ((par(2) .eq. LPERPEND) .or. (par(3) .eq. CLKW)) then
c           ... but when doing any other axis, we do not need as much.
	    standoff = standoff * 0.5
	    if (par(3) .eq. CLKW) standoff = -standoff
	endif
	xs = x + standoff * sinang
	ys = y - standoff * cosang
	if (par(2) .eq. LPERPEND) then
C           Labels are perpendicular to axis
	    savang = angle
	    savcos = cosang
	    savsin = sinang
	    angle = angle - 90.
	    cosang =  savsin
	    sinang = -savcos
	endif
	call PMGO(grelocate)(xs,ys)
	call PMGO(putlabel)(nchar,string,6-nint(par(1)))
	if (par(2) .eq. LPERPEND) then
	    angle = savang
	    cosang = savcos
	    sinang = savsin
	endif
	return
	end
C==============================================================================
#ifdef  OLD
C==============================================================================
#       ifdef VMS
	options /g_floating
#       endif /* VMS */
	subroutine PTUV(number)(var,n,string,notation,tuvify,maxdigit)
C       NUMBER converts the dp floating point number VAR to a STRING of text.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include         'axis.inc'
C       Parameters
	character*(*)   MINUS
	parameter       (MINUS = '-')
	character*(*)   BSLASH
	parameter       (BSLASH = AsciiBackSlash)
c                       representation for 'Times'is '\g*'
	character*(*)   TIMES
	parameter       (TIMES = BSLASH  )//'g*')
c                       representation for '10 To The Power' is '10\\u'
	character*(*)   TENUP
	parameter       (TENUP = '10'  )//BSLASH//BSLASH//'u')
C       External Variables
c                       the numerical value we want to convert to text string
	real*8          var
c                       non-blank length of returned resulting string
	integer         n
c                       this is the returned result string
	character*(*)   string
c                       if (notation = -1) fp
			if (notation =  0) default
			if (notation =  1) exponential notation
	integer         notation
c                       if (.not.tuvify) return plain text, i.e., 1.2E7
c                       if (     tuvify) return Mongo text, i.e., 1.2\g*10\\u7
	logical         tuvify
C       Internal Variables
	real*8          value
c       integer         nsign
	real*8          expo
	integer         iexp,iexp1,iexp2,iexp0
	real*8          amant
	integer         nexp
	integer         iexit
	integer         j
	character*1     numeral(0:9)
        character*132   fstrcat
C       Data
	data    numeral /'0','1','2','3','4','5','6','7','8','9'/
C       Executable Code
	n = 0
	value = var
c       nsign = 1
C       Check for negative or zero number
	if (value .eq. 0.) then
	    n = n + 1
	    string(n:n) = numeral(0)
	    return
	elseif (value .lt. 0.) then
	    n = n + 1
	    string(n:n) = MINUS
c           nsign = n
	    value = -value
	endif
c
C       Pull off the exponent and get the mantissa
	expo = log10(value)
	iexp = int(abs(expo) + 1.D-5)
	if (expo .lt. -1.D-5) iexp = -iexp - 1
	amant = value * 10.D0 ** (-iexp) + 1.D-10
	amant = amant + 10.D0**(-maxdigit)
	if (amant .ge. 10.D0) then
	    amant = amant/10.D0
	    iexp = iexp + 1
	endif
c
C       Pull off the various digits, decrementing NEXP
	nexp = iexp
	if (notation .ne. FFORMAT) then
	    if (abs(iexp) .gt. 4 .or. notation .eq. EFORMAT) nexp = 0
	endif
c
C       For numbers to be written as .0xxx, change the exponent to -1
	if ((iexp .lt. -1) .and. (iexp .ge. -4) .and.
     &  (notation .eq. GFORMAT)) then
	    amant = amant * 10.D0**(iexp + 1)
	    nexp = -1
	endif
c
	iexit = 0
	do 30 j = 1,maxdigit
c
C         AMANT less than 1D-4 means that there is very little left
	  if (amant .lt. 1.D-4) iexit = 1
	  if (iexit .ne. 0 .and. nexp .lt. 0) goto 31
c
C         NEXP equal -1 means write a decimal point
	  if (nexp .eq. -1) then
	      n = n + 1
	      string(n:n) = '.'
	  endif
	  n = n + 1
	  string(n:n) = numeral(int(amant))
	  amant = 10. * (amant - int(amant))
	  nexp = nexp - 1
30      continue
31      continue
c
c       if VALUE needs an exponent, get it here
	if ((abs(iexp) .gt. 4 .and. notation .eq. GFORMAT) .or.
     &      (    iexp  .ne. 0 .and. notation .eq. EFORMAT)) then
	    if (tuvify) then
		if (n.gt.1 .or. string(1:1).ne.'1') then
		    string(n+1:n+8) = FSTRCAT(TIMES,TENUP)
		    n = n + 8
		else
		    string(1:5) = TENUP
		    n = 5
		endif
	    else
		n = n + 1
		string(n:n) = 'E'
	    endif
	    if (iexp.le.0) then
		n = n + 1
		string(n:n) = MINUS
	    endif
	    iexp0 = abs(iexp) / 100
	    iexp1 = abs(iexp) / 10 - iexp0 * 10
	    iexp2 = abs(iexp) - iexp1 * 10 - iexp0 * 100
	    if (iexp0 .ne. 0) then
		n = n + 1
		string(n:n) = numeral(iexp0)
	    endif
	    if (iexp0 .ne. 0 .or. iexp1 .ne. 0) then
		n = n + 1
		string(n:n) = numeral(iexp1)
	    endif
	    n = n + 1
	    string(n:n) = numeral(iexp2)
	endif
	return
	end
C==============================================================================
#else   /* OLD */
C==============================================================================
#       ifdef VMS
	options /g_floating
#       endif /* VMS */
	subroutine PTUV(number)(val,iexp,string,type,notrail,touva)
c       take a numerical value val whose least significant digit is
c       10.**iexp and convert that into a string
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'axis.inc'
C       Parameters
c                       this is the format we use to write the format we use
	character*(*)   FORMAT
	parameter       (FORMAT = '(i3.3,a,i3.3)')
c                       set the maximum number of significant digits ever used
c                       this is determined by the number of significant decimal
c                       digits provided by the double precision of this machine.
	integer         MXSIGFIG
	parameter       (MXSIGFIG = 16)
C       External Variables
c                       this is the numerical value we want converted to string
	real*8          val
c                       the logarithm of the least significant digit (lsd)
c                       10.**iexp must be represented in the output string
	integer         iexp
c                       the space where we put the output
	character*(*)   string
c                       what type of format do we want?
c                       i --> try to do integer if possible
c                       f --> try to do floating if possible
c                       e --> default if others do not match, exponential
	integer         type
c                       should we strip off trailing zeros where possible?
c                       this also determines stripping of leading zeros.
	logical         notrail
c                       should we rewrite these suitably for plotting?
	logical         touva
C       Internal Variables
c                       this is the number of digits before the fmt decimal
	integer         w
c                       this is the number of digits after  the fmt decimal
	integer         d
c                       output format
	character*16    tamrof
c                       local copy of iexp
	integer         lexp
c                       local copy of val
	real*8          rval
c                       floor(log10(abs(val))) ==> most significant digit
	integer         iflr
c                       do fp nums have leading zero?
	integer         mfwid
c                       temporary variables
	real*8          temp, tens
c                       loop
	integer         i
C       External Function
C       Executable Code
#       ifdef DEBUG
c       call sunieee(i)
c       if (i .ne. 0) write(92,*) 'begin number', i
	write(92,*) '=================='
	write(92,*) 'val, iexp, type', val, iexp, type
#       endif /* DEBUG */
c       expand rval out such that the nearest integer to it contains
c       exactly all of the significant digits of val according to iexp
	rval = abs(val)*10.D0**dble(-iexp)
c       check the result against zero, because zero is a special case
	if (rval .lt. 0.5) then
c           this number is effectively zero, because it is less than
c           the value of the least significant digit we want to see
	    iflr = 0
	    rval = 0.
	    lexp = 0
#           ifdef DECMIPSFMTBUG
c           problem with DecStation 5000/200, Ultrix 4.3, DEC FORTRAN 3.1
c           idiots think that 0.D0 written as (1pe005.000e1) gives '*****'
	    type = IFORMAT
#           endif /* DECMIPSFMTBUG */
	else
c           this number is non-zero according to values of val and iexp
#           ifdef DEBUG
c           call sunieee(i)
c           if (i .ne. 0) write(92,*) 'nonzero number', i
#           endif /* DEBUG */
c           find that nearest integral value, note this is always >=1
	    rval = dnint(rval)
#           ifdef DEBUG
	    write(92,*) rval, (rval - 1.D0), log10(rval),
     &      int(log10(rval)),
     &      log10(rval)-dnint(log10(rval))
c           call sunieee(i)
c           if (i .ne. 0) write(92,*) 'rval,log... number', i
#           endif /* DEBUG */
c           try to identify the most significant digit (MSD)
	    iflr = int(log10(rval))
c           fix any problems with the above determination of MSD
	    if ((rval - 10.D0**(iflr+1)) .ge. -0.5) iflr = iflr + 1
c           recover the actual exponent of the MSD
	    iflr = iflr + iexp
c           set a reasonable cutoff on the number of silly significant digits
	    lexp = max(iexp,iflr-(MXSIGFIG-1))
c           reset rval to be our current remaining value
	    rval = val
	endif
c       check sign of remaining value, make room for the minus sign
	if (rval .lt. 0.) then
	    w = 1
	else
	    w = 0
	endif
c       See if there will be trailing zeros that should be stripped.
	if (notrail) then
	    temp = dnint(abs(rval) * 10.D0**(-dble(lexp)))
c           The trailing zeros are stripped by modifying lexp.
#           define UP
#           ifdef UP      /* this should usually be faster, but not for log */
	    i = 0
	    tens = 1.D0
10          continue
	    tens = tens * 10.D0
	    if (dnint(mod(temp, tens)) .eq. 0.D0) then
#               ifdef DEBUG
		write(92,*) i, mod(temp, tens), temp, tens
c               call sunieee(i)
c               if (i .ne. 0) write(92,*) 'i,mod... number', i
#               endif /* DEBUG */
		i = i + 1
		if (i .lt. (iflr - lexp)) goto 10
	    endif
#           else /* UP */
	    i = iflr - lexp
10          continue
	    if (dnint(mod(temp, 10.D0**i)) .ne. 0.D0) then
#               ifdef DEBUG
		write(92,*) i, mod(temp, 10.D0**i), temp, 10.D0**i
c               call sunieee(i)
c               if (i .ne. 0) write(92,*) 'i,mod... number', i
#               endif /* DEBUG */
		i = i - 1
		if (i .gt. 0) goto 10
	    endif
#           endif /* UP */
	    lexp = lexp + i
c           we do not allow fp numbers to have a leading zero (e.g., .4)
	    mfwid = -1
	else /* (notrail) */
c           we allow fp numbers to have a leading zero (e.g., 0.4)
	    mfwid = 0
	endif /* (notrail) */
#       ifdef DEBUG
	write(92,*) 'lexp, iflr, mfwid', lexp, iflr, mfwid
c       call sunieee(i)
c       if (i .ne. 0) write(92,*) 'lexp... number', i
#       endif /* DEBUG */
c       strip off trailing digits that are not significant
	rval = dnint((rval * 10.D0**(-dble(lexp)))) * 10.D0**dble(lexp)
c       construct the format statement tamrof which the internal write will use
	tamrof(12:) = ')    '
#       ifdef DEBUG
	write(92,*) 'rval',rval,'type',type,'val',val
c       call sunieee(i)
c       if (i .ne. 0) write(92,*) 'rval... number', i
#       endif /* DEBUG */
	if (type .eq. IFORMAT .and. lexp .ge. 0 .and.
     &  abs(val) .lt. 2.D0**31) then
c           we are doing I format
	    d = iflr + 1
	    w = w + d
	    tamrof(1:4) = '  (i'
	elseif (type .ne. EFORMAT) then
c           we are doing F format
	    d = max(0,-lexp)
	    w = w + max(iflr,mfwid) + 2 + d
	    tamrof(1:4) = '  (f'
	else
c           we are doing E format
	    d = max(iflr - lexp,0)
	    w = w + d + 5
	    if (abs(iflr) .ge. 100) then
		w = w + 2
		tamrof(12:) = 'e3)  '
	    elseif (abs(iflr) .ge. 10) then
		w = w + 1
		tamrof(12:) = 'e2)  '
	    else
		tamrof(12:) = 'e1)  '
	    endif
	    tamrof(1:4) = '(1pe'
	endif
	w = min(w,len(string))
#       ifdef MIPS_FMT_BUG
c       just try writing the value 1.e9 using the format 1pe5.0e1 (idiots)
	if ((abs(rval) .ge.  1.e9) .and. (abs(rval) .lt. 1.e10))  then
	    w = w + 1
	endif
#       endif /* MIPS_FMT_BUG */
	write(tamrof(5:11),FORMAT) w, '.', d
#       ifdef MIPS_FMT_BUG
c       now continue fixing the format botch those idiots pulled
	if ((abs(rval) .ge.  1.e9) .and. (abs(rval) .lt. 1.e10))  then
	    tamrof(13:13) = '2'
	endif
#       endif /* MIPS_FMT_BUG */
#       ifdef DEBUG
	write(92,*) tamrof, w, d, rval
c       call sunieee(i)
c       if (i .ne. 0) write(92,*) 'tamrofwdrval', i
#       endif /* DEBUG */
	if (tamrof(4:4) .eq. 'i') then
	    write(string,tamrof) nint(rval)
	else
	    write(string,tamrof) rval
	endif
#       ifdef MIPS_FMT_BUG
c       now finish fixing the format botch those idiots pulled
	if ((abs(rval) .ge.  1.e9) .and. (abs(rval) .lt. 1.e10))  then
	    string(w-1:w-1) = string(w:w)
	    string(w:w) = ' '
	endif
#       endif /* MIPS_FMT_BUG */
#       ifdef DEBUG
	write(92,*) string
c       call sunieee(i)
c       if (i .ne. 0) write(92,*) 'end number', i
#       endif /* DEBUG */
	if (touva) call PTUV(tuvify)(string)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(tuvify)(string)
	IMPLICIT_NONE
c       take a string with a number and convert it for plotting
c       m.mmD-nn becomes m.mm\g*10\\unn
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Parameters
	character*(*)   BSLASH
	parameter       (BSLASH = AsciiBackSlash)
c                       representation for 'Times'is '\g*'
C	character*(*)   TIMES
C	parameter       (TIMES = BSLASH//'g*')
c                       representation for '10 To The Power' is '10\\u'
C	character*(*)   TENUP
	character*10    TIMES
        character*10    TENUP 
C	parameter       (TENUP = '10'//BSLASH//BSLASH//'u')
C       External Variable
c                       a string representing a number
	character*(*)   string
C       Internal Variables
	integer         i, j, k, l, ltenup, ltimes
	character       temp*64
        character*132   fstrcat
        TIMES = FSTRCAT(BSLASH,'g*')
	TENUP = FSTRCAT('10',FSTRCAT(BSLASH,FSTRCAT(BSLASH,'u')))
        do i=1,len(tenup)
          if (tenup(i:i) .ne. ' ') ltenup = i
        end do
        do i=1,len(times)
          if (times(i:i) .ne. ' ') ltimes = i
        end do
C       Executable Code
	i = index(string,'.E+')
	j = index(string,'.E')
	k = index(string,'E+')
	l = index(string,'E')
	temp = string
	if (string(1:3) .eq. '0.E') then
	    temp = '0'
	elseif (string(1:4) .eq. '1.E+') then
	    temp = FSTRCAT(TENUP(1:ltenup),string(5:))
	elseif (string(1:4) .eq. '1.E-') then
	    temp = FSTRCAT(TENUP(1:ltenup),string(4:))
	elseif (string(1:5) .eq. '-1.E+') then
	    temp = FSTRCAT('-',FSTRCAT(TENUP(1:ltenup),string(6:)))
	elseif (string(1:5) .eq. '-1.E-') then
	    temp = FSTRCAT('-',FSTRCAT(TENUP(1:ltenup),string(5:)))
	elseif (i .ne. 0) then
	    temp = FSTRCAT(string(1:i-1),
     &         FSTRCAT(TIMES(1:ltimes),FSTRCAT(TENUP(1:ltenup),string(i+3:))))
	elseif (j .ne. 0) then
	    temp = FSTRCAT(string(1:j-1),
     &         FSTRCAT(TIMES(1:ltimes),FSTRCAT(TENUP(1:ltenup),string(j+2:))))
	elseif (k .ne. 0) then
	    temp = FSTRCAT(string(1:k-1),
     &         FSTRCAT(TIMES(1:ltimes),FSTRCAT(TENUP(1:ltenup),string(k+2:))))
	elseif (l .ne. 0) then
	    temp = FSTRCAT(string(1:l-1),
     &         FSTRCAT(TIMES(1:ltimes),FSTRCAT(TENUP(1:ltenup),string(l+1:))))
	endif
	string = temp
	return
	end
C==============================================================================
#endif  /* OLD */
C==============================================================================
