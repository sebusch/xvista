#include "Conf.h"
	subroutine PMGO(plt3d1)(a,nx,ny,m,n,work,dthe,dphi,datmin,datmax)
c       a         - real data array, represents height of surface as
c                   function of location in plane
c       nx,ny     - dimensions of data array a
c       m,n       - plotrange (1-m,1-n)
c       work      - real work array, length .ge. 4*min(m,n)
c       dthe,dphi - theta, phi viewing angles in degrees
c       zfac0     - scaling of z-axis 
c                   (inverse of range, i.e. default 1/datamax)
c       zoff0     - offset of z-origin in data units
c       jan 86  wh   exchange x and y to match other plots
c                       m should be x dir and n y dir
	IMPLICIT_NONE
        include 'MONGOPAR.inc'

C       External Variables
	integer         m, n , nx, ny
	real            a(nx,ny), work(*)
	real            dthe, dphi, datmin, datmax
C       Internal Variables
	real            the, phi
	real            xsc, ysc, zsc, xx, yy
	integer         lmax, iaz, i, j, ll, lli, ic, ibeg, jbeg
	integer         lnth, ier
	integer         ifirst, istep, ilast
	integer         jfirst, jstep, jlast
	real            a3, b3, b4

C       Common Block
	real sphi, cphi, sthe, cthe
	real a1, a2, b1, b2
        real xlen,ylen,zlen,xoff,yoff,zoff
        common /commgo_axis3d/ sphi,cphi,sthe,cthe
     &                     ,a1,a2,b1,b2
     &                     ,xlen,ylen,zlen,xoff,yoff,zoff

C       Executable Code

	xlen = 2*(gx2-gx1)/3.3
	ylen = xlen
	zlen = ylen/1.5

	xoff = .5*(gx2+gx1)
	yoff = .5*(gy2+gy1)
        zoff = - ylen/4


	lmax = 2*min0(m,n)
#ifdef  FIVEHUNDRED
	if (l .lt. lmax) goto 500
#endif  /* FIVEHUNDRED */
	phi = dphi*0.0174532925
	the = dthe*0.0174532925

	sphi = sin(phi)
	cphi = cos(phi)
	sthe = sin(the)
	cthe = cos(the)

        if (datmax.eq.datmin) then
          datmin=0.1*datmax
        end if

	xsc = xlen/float(m-1)
	ysc = ylen/float(n-1)
        zsc = zlen/(datmax-datmin)

	a1 = -sphi*xsc
	a2 = cphi*ysc
 	a3 = xoff-0.5*(a1*float(m+1)+a2*float(n+1))
	b1 = -cphi*cthe*xsc
	b2 = -sphi*cthe*ysc
	b3 = zsc*sthe

	b4 = sthe*zoff + yoff-0.5*(b1*float(m+1)+b2*float(n+1))


	iaz = 1
	if (a1 .le. 0.0) iaz = iaz+1
	if (a2 .le. 0.0) iaz = iaz+2

	if (iaz .eq. 1 .or. iaz .eq. 3) then
	    ifirst = 1
	    istep = 1
	    ilast = n
	else if (iaz .eq. 2 .or. iaz .eq. 4) then
	    ifirst = n
	    istep = -1
	    ilast = 1
	end if
	if (iaz .eq. 3 .or. iaz .eq. 4) then
	    jfirst = 1
	    jstep = 1
	    jlast = m
	elseif (iaz .eq. 1 .or. iaz .eq. 2) then
	    jfirst = m
	    jstep = -1
	    jlast = 1
	endif
	if (iaz .eq. 2 .or. iaz .eq. 3) then
	    lli = 1
	elseif (iaz .eq. 1 .or. iaz .eq. 4) then
	    lli = -1
	endif
	ic = 0
	ibeg = ifirst+istep
70      continue
	lnth = min0(2*iabs(ibeg-ifirst)+1,lmax)
	if (lli .eq. -1) then
	    ll = lnth+1
	else
	    ll = 0
	endif
	i = ibeg
	j = jfirst
	xx = float(j)
	yy = float(i)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)-datmin)+b4
80      continue
	i = i-istep
	yy = float(i)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)-datmin)+b4
	if (j .eq. jlast) goto 85
	j = j+jstep
	xx = float(j)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)-datmin)+b4
	if (i .ne. ifirst) goto 80
85      continue
	call PTUV(nxtvu)(ic,work(1),work(lmax+1),lnth,ier)
	if (ier .ne. 0) return
	ic = 1
	if (ibeg .eq. ilast) goto 90
	ibeg = ibeg+istep
	goto 70

 90     continue
	jbeg = jfirst
 100    continue
	lnth = min0(2*iabs(jbeg-jlast)+1,lmax)
	if (lli.eq.-1) then
	    ll = lnth+1
	  else
	    ll = 0
	endif
	i = ilast
	j = jbeg
	xx = float(j)
	yy = float(i)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)-datmin)+b4
 110    continue
	j = j+jstep
	xx = float(j)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)-datmin)+b4
	if (i .eq. ifirst) goto 120
	i = i-istep
	yy = float(i)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)-datmin)+b4
	if (j .ne. jlast) goto 110
 120    continue
	call PTUV(nxtvu)(1,work(1),work(lmax+1),lnth,ier)
	if (ier .ne. 0) return
	jbeg = jbeg+jstep
	if (jbeg.eq.jlast) return
	goto 100
#ifdef  FIVEHUNDRED
500     continue
	ier = 2
	return
#endif  /* FIVEHUNDRED */
	end

	subroutine PMGO(box3d1)(xmin,xmax,chtxtx,ichtxtx
     &                         ,ymin,ymax,chtxty,ichtxty)
	IMPLICIT_NONE
        include 'MONGOPAR.inc'
        include 'axis.inc'

C       External Variables
	real            xmin,xmax,ymin,ymax
        character*(*)   chtxtx,chtxty
        integer         ichtxtx,ichtxty

C       Internal Variables
        integer         labelx,labely
	real            angsave
	integer         ltysave
        real            xax1,xax2,xaxang,yax1,yax2,yaxang
        real            xlab1,xlab2,ylab1,ylab2
	real            xlength, ylength ,xticang, yticang
	integer         iclwk,jclwk
	real            xmin1,xmax1,ymin1,ymax1

C       Common Blocks
	real sphi, cphi, sthe, cthe
	real a1, a2, b1, b2
        real xlen,ylen,zlen,xoff,yoff,zoff
        common /commgo_axis3d/ sphi,cphi,sthe,cthe
     &                     ,a1,a2,b1,b2
     &                     ,xlen,ylen,zlen,xoff,yoff,zoff

        real axangle,ticang,cticang,sticang,fraclen
        common /commgo_axistic/ axangle,ticang,cticang,sticang,fraclen

	real            asmallx, abigx, asmally, abigy
	common  /gridpar/       asmallx, abigx, asmally, abigy


C       Executable Code

        labelx=1
        labely=1

	angsave = angle
	ltysave = ltype
	call PMGO(setltype)(0)

	xlength = sqrt((sphi*xlen)**2+(cphi*cthe*xlen)**2)
	ylength = sqrt((cphi*ylen)**2+(sphi*cthe*ylen)**2)

**  Location of Axis (depend on Orientation of plot)
        if (sphi.le.0.0) then
          xax1= xoff - 0.5       *(-sphi*xlen + cphi*ylen*1.15)
          xax2= yoff - 0.5 *cthe *(-cphi*xlen - sphi*ylen*1.15)
     &            + sthe*zoff
          iclwk=0
          xaxang=atan2(b1,a1)
          xticang=atan2(b2,a2)
          xlab1= xoff - 0.5       *(+ cphi*ylen*1.4)
          xlab2= yoff - 0.5 *cthe *(- sphi*ylen*1.4)
     &            + sthe*zoff - sthe*zlen/9.
          xmin1=xmin
          xmax1=xmax
        else if (sphi.gt.0.0) then
          xax1= xoff - 0.5       *( sphi*xlen - cphi*ylen*1.15)
          xax2= yoff - 0.5 *cthe *( cphi*xlen + sphi*ylen*1.15)
     &            + sthe*zoff
          iclwk=0
          xaxang=atan2(b1,a1)+pi
          xticang=atan2(b2,a2)+pi
          xlab1= xoff - 0.5       *(- cphi*ylen*1.4)
          xlab2= yoff - 0.5 *cthe *(+ sphi*ylen*1.4)
     &            + sthe*zoff - sthe*zlen/9.
          xmin1=xmax
          xmax1=xmin
        end if

        if (cphi.ge.0.0) then
          yax1= xoff + 0.5       *(-sphi*xlen*1.15 - cphi*ylen)
          yax2= yoff + 0.5 *cthe *(-cphi*xlen*1.15 + sphi*ylen)
     &            + sthe*zoff
          jclwk=0
          yaxang=atan2(b2,a2)
          yticang=atan2(b1,a1)+pi
          ylab1= xoff + 0.5       *(-sphi*xlen*1.4)
          ylab2= yoff + 0.5 *cthe *(-cphi*xlen*1.4)
     &             + sthe*zoff - sthe*zlen/9.
          ymin1=ymin
          ymax1=ymax
        else if (cphi.lt.0.0) then
          yax1= xoff + 0.5       *( sphi*xlen*1.15 + cphi*ylen)
          yax2= yoff + 0.5 *cthe *( cphi*xlen*1.15 - sphi*ylen)
     &            + sthe*zoff
          ymin1=ymax
          ymax1=ymin
          jclwk=0
          yaxang=atan2(b2,a2)+pi
          yticang=atan2(b1,a1)
          ylab1= xoff + 0.5       *( sphi*xlen*1.4)
          ylab2= yoff + 0.5 *cthe *( cphi*xlen*1.4)
     &             + sthe*zoff - sthe*zlen/9.
        end if


c       For X axis

	call setangle (xaxang*180.0/pi)
        axangle=xaxang

        ticang=xticang
        cticang=cos(ticang)
        sticang=sin(ticang)
        fraclen=ylength/ylen

	sixties = xsixty
	if (xsixty) ndecim = ndecimx
	call PMGO(axis1)(xmin1,xmax1,asmallx,abigx,xax1,xax2
     &           ,xlength,labelx,iclwk)
*  Labeling
        call grelocate (xlab1,xlab2)
        call putlabel (ichtxtx,chtxtx,5)


c       For Y axis

	call setangle (yaxang*180.0/pi)
        axangle=yaxang

        ticang=yticang
        cticang=cos(ticang)
        sticang=sin(ticang)
        fraclen=xlength/xlen

	sixties = ysixty
	if (ysixty) ndecim = ndecimy
	call PMGO(axis1)(ymin1,ymax1,asmally,abigy,yax1,yax2
     &                 ,ylength,labely,jclwk)
*  Labeling
        call grelocate (ylab1,ylab2)
        call putlabel (ichtxty,chtxty,5)


c       Clean up
	sixties = .false.
	call PMGO(setangle)(angsave)
	call PMGO(setltype)(ltysave)

	return
	end

C==============================================================================
C==============================================================================
	subroutine PTUV(tickdraw1)(x,y,value,major,par)
C       Tick's location is X,Y
C       MAJOR determines whether a big or small tick
C       PAR(1) = 1 for clockwise ticks on the axis, 0 for counter
C       ANGLE (in COMMON) determines the angle of the axis.

	IMPLICIT_NONE

        include 'MONGOPAR.inc'
        include 'axis.inc'

**  Angle of the Ticks (in case they are NOT perpendicular to the axis)
        real axangle,ticang,cticang,sticang,fraclen
        common /commgo_axistic/ axangle,ticang,cticang,sticang,fraclen

C       External Variables
	real            X, Y
	real*8          value
	logical         major
	real            par(*)

C       Internal Variables
	real            ticklen
	real            xtick, ytick

C       Executable Code
	ticklen = cheight * expand * fraclen
	if (.not. major) ticklen = ticklen * 0.5
	if (par(1) .eq. CLKW) ticklen = -ticklen

	xtick = x + ticklen*cticang
	ytick = y + ticklen*sticang

	call PMGO(line)(x,y,xtick,ytick)
	return
	end

C==============================================================================
C==============================================================================
#ifdef  VMS
	options /g_floating
#endif  /* VMS */
	subroutine PMGO(axis1)(sa1,sa2,asmall,abig,ax,ay,alen,
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
	real            par(5)
	real            ay2, ax2
	external        PTUV(ticklabel), PTUV(tickdraw1)
	real*8          a1, a2

        real axangle,ticang,cticang,sticang,fraclen
        common /commgo_axistic/ axangle,ticang,cticang,sticang,fraclen

C       Executable Code
	if (asmall .lt. 0) then
c           the axis limits are logarithmic
	    if (abs(sa1) .le. 308. .and. abs(sa2) .le. 308.) then
#ifdef          sun386i
c               Some aspects of the Sun386i Fortran are incredibly broken...
		a1 = 10.D0**dble(sa1)
		a2 = 10.D0**dble(sa2)
#else           /* sun386i */
		a1 = 10.D0**sa1
		a2 = 10.D0**sa2
#endif          /* sun386i */
	    else
		write(STDERR,*) ' Logarithmic axis limit too big.'
		return
	    endif
	else
c           the axis limits are not logarithmic
	    a1 = sa1
	    a2 = sa2
	endif

        cosang=cos(axangle)
        sinang=sin(axangle)
	ax2 = alen*cosang + ax
	ay2 = alen*sinang + ay

#ifdef  _PostScript
	if (ipsopen) call PTUV(psclip)(.false.)
#endif  /* _PostScript */
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
     &      par, PTUV(tickdraw1))
	else
c           we don't want ticks, but we do want labels
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
#ifdef      OLD
	    par(4) = GFORMAT
#else       /* OLD */
	    par(4) = IFORMAT
#endif      /* OLD */
	    call PTUV(axistick)(a1,a2,asmall,abig,ax,ay,alen,
     &      par,PTUV(ticklabel))
	    sixties = .false.
	endif
	return
	end
