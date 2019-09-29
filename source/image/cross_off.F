#include "Vista.h"
      SUBROUTINE CROSSOFF(A,ASR,AER,ASC,AEC,B,BSR,BER,BSC,BEC)

C     Cross-correlation centroid offset routine.

C     Keywords:

      INTEGER ASC,AEC,ASR,AER,BSC,BEC,BSR,BER
      REAL A(ASC:AEC,ASR:AER), B(BSC:BEC,BSR:BER), g(2)
      CHARACTER PARM*8
      LOGICAL KEYCHECK, cenobj, cencomp, border, silent, border_last
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC', border_last
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C     Allowed keywords

      	CALL KEYINIT
      	CALL KEYDEF('R=')
      	CALL KEYDEF('NPIXS=')
      	CALL KEYDEF('SUB=')
      	CALL KEYDEF('CO=')
      	CALL KEYDEF('CC=')
      	CALL KEYDEF('SKY=')
      	CALL KEYDEF('SILENT')

C     Return if there is an unknown keyword.

      	IF (.NOT.KEYCHECK()) THEN
	  	XERR = .TRUE.
	  	RETURN
      	END IF

C     Check keywords.

	rad	=2.0	! Radius of object to include
	npixs	=1	! Half width of box to search in object pixels.
	nsub	=2	! Subsampling factor of comparison.
	sky	=0	! Sky value for error calculation
	cenobj	=.false.	! Center of object input?
	cencomp	=.false.	! Center of comparison input?
	silent	=.false.	! Printout?
      	do i=1,ncon
	  	if (word(i)(1:2) .eq. 'R=') THEN
	      		CALL ASSIGN(WORD(I),F,PARM)
	      		IF (XERR) RETURN
			rad	=f

	  	else if (word(i)(1:6) .eq. 'NPIXS=') THEN
	      		CALL ASSIGN(WORD(I),F,PARM)
	      		IF (XERR) RETURN
			npixs	=nint(f)

	  	else if (word(i)(1:4) .eq. 'SUB=') THEN
	      		CALL ASSIGN(WORD(I),F,PARM)
	      		IF (XERR) RETURN
			nsub	=nint(f)

	  	else if (word(i)(1:4) .eq. 'SKY=') THEN
	      		CALL ASSIGN(WORD(I),F,PARM)
	      		IF (XERR) RETURN
			sky	=f

	  	else if (word(i)(1:3) .eq. 'CO=') THEN
			CALL ASSIGNV(WORD(I),2,G,NG,PARM)
			if (xerr) return
			if (ng .lt. 2) then
				print *,'Missing object center...'
				xerr	=.true.
				return
			end if

			cenobj	=.true.
			robj	=g(1)
			cobj	=g(2)

	  	else if (word(i)(1:3) .eq. 'CC=') THEN
			CALL ASSIGNV(WORD(I),2,G,NG,PARM)
			if (xerr) return
			if (ng .lt. 2) then
				print *,'Missing comparison center...'
				xerr	=.true.
				return
			end if

			cencomp	=.true.
			rcomp	=g(1)
			ccomp	=g(2)

		else if (silent) then
			silent	=.true.

		end if

	end do

C	Check input parameters

	if (rad .lt. 1.0) then
		xerr	=.true.
		print *,'Object radius too small...'
		return
	end if

	if (nsub .lt. 1) then
		xerr	=.true.
		print *,'Bad subsampling factor...'
		return
	end if

	if (npixs .le. 0) then
		xerr	=.true.
		print *,'Bad search dimension...'
		return
	end if

	if (.not. cenobj) then
		xerr	=.true.
		print *,'Object centroid was not specified...'
		return
	end if

	if (.not. cencomp) then
		xerr	=.true.
		print *,'Comparison centroid was not specified...'
		return
	end if

	if ((robj+rad .gt. AER) .or. (robj-rad .lt. ASR) .or.
     1	    (cobj+rad .gt. AEC) .or. (cobj-rad .lt. ASC)) then
		xerr	=.true.
		print *,'Object radius plus center out of bounds...'
		return
	end if

	crad	=nsub*npixs
	if ((rcomp+crad .gt. BER) .or. (rcomp-crad .lt. BSR) .or.
     1	    (ccomp+crad .gt. BEC) .or. (ccomp-crad .lt. BSC)) then
		xerr	=.true.
		print *,'Comparison search plus center out of bounds...'
		return
	end if

C	Loop over a box centerred on the comparison object.  At each
C	center position draw samples to match the object and compute
C	chi^2.  Save the position at which chi2 is minimized.  Do
C	this twice - the second time zoom into the first minimum
C	by a factor 10X.

	irad	=int(rad)+1
	nrb	=ber-bsr+1
	ncb	=bec-bsc+1
	pc	=binset(b,nrb,ncb,.false.)
	border	=.false.
	border_last	=.false.
	niter	=0
	ntotal	=0
	do while (niter .lt. 3 .and. ntotal .lt. 6)
        	if (.not. silent)
     &          	print *, 'Niter: ', niter,' border: ', border
		if (niter .eq. 0) then
			dpix	=float(nsub*npixs)/10.0
			yc_cen	=rcomp
			xc_cen	=ccomp
			niter	=1
		else
			if (border .and. border_last) then
				xerr	=.true.
				print *,'Offset out of search range...'
				return
			end if

			if (.not. border) then
				dpix	=dpix/10.0
				niter	=niter+1
				border_last	=.false.
				niter	=niter+1

			else
				border_last	=.true.

			end if

			yc_cen	=yc_min
			xc_cen	=xc_min
		end if
        	if (.not. silent)
     &          	print *, ' xc_cen, yc_cen: ', xc_cen, yc_cen

C	For these two loops we vary the center over which the samplling
C	of the comparison object takes place.

		do ir=-12,12
			ycomp	=yc_cen+float(ir)*dpix
			do ic=-12,12
				xcomp	=xc_cen+float(ic)*dpix
				chi2	=0.0

C	For a given center or offset position, compute chi^2 by
C	integrating over the radius of the input object.

				do ior=nint(robj)-irad,nint(robj)+irad
					yo	=ior-robj
				do ioc=nint(cobj)-irad,nint(cobj)+irad
					xo	=ioc-cobj
					prad	=sqrt(yo**2+xo**2)

C	The exact pixel value of the input object are used, while
C	those for the comparison are extracted by high precision
C	sinc interpolation as specified by the center.

					if (prad .le. rad) then
					po	=a(ioc,ior)
					x	=nsub*(ioc-nint(cobj))
     1						 +xcomp-bsc+1
					y	=nsub*(ior-nint(robj))
     1						 +ycomp-bsr+1
					pc=xbin(x,y,b,nrb,ncb,.false.)
					full	=sky+po
					if (full .gt. 0.0) then
					chi2	=chi2+(po-pc)**2
					end if
					end if
				end do
				end do

C	The chi^2 calculation is complete.  See if it is a minimum
C	compared to previous values.

C	if (.not. silent) print *, ' ', ir, ic, chi2, chi2min
				if (ic .eq. -12 .and. ir .eq. -12) then
					chi2min	=chi2
					yc_min	=ycomp
					xc_min	=xcomp
					border	=.true.
				else if (chi2 .lt. chi2min) then
					chi2min	=chi2
					yc_min	=ycomp
					xc_min	=xcomp

					if (ir.eq.-12 .or. ir.eq.12 .or.
     1					ic.eq.-12 .or. ic.eq.12) then
						border	=.true.

					else
						border	=.false.

					end if

				end if

			end do		! Center column loop

		end do			! Center row loop

		if (.not. silent) 
     &            print *, ' end of iteration, xc_min, yc_min: ', xc_min, yc_min

		ntotal	=ntotal+1
	end do				! Iteration loop

	if (border) then
		print *,'Offset out of fine search range...'
c		xerr	=.true.
c		return
	end if

C	Return the final offset as Vista variables DC and DR.

	dc	=(ccomp-xc_min)/float(nsub)
	dr	=(rcomp-yc_min)/float(nsub)

        CALL VARIABLE('DR',dr,.TRUE.)
        CALL VARIABLE('DC',dc,.TRUE.)

	if (.not. silent) then
		print 100, dr, dc
100		format ('Cross-correlation offsets:  ',f6.3,', ',f6.3,
     1 			' loaded into Vista variables: DR, DC')
	end if

	return
	end
