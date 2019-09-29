#include "Vista.h"
        SUBROUTINE PHASE(A,NROW,NCOL)

C       Routine to phase shift a Fourier domain image or calculate
C	phases for building subsampled images.

C	This file really holds two separate routines.  The first is
C	a simple phase-shifting routine that can be used for shifting
C	images purely in the Fourier domain.  One specifies the phase
C	shifts in terms of row or column shifts in the standard Vista
C	sense.

C	Keywords:

C	DR=dr		Row offset (default 0.0)
C	DC=dc		Column offset (default 0.0)

C	Note these keywords are only used for the simple phase shift.

C	The second routine is more complex and is designed to generate
C	complex weight for which to combine several different images
C	into a Fourier transfrom of a subsampled image.  Each individual
C	images is an undersampled images shifted from others in the set by
C	a known amount.  If the shifts among the set of images come
C	reasonable close to the nominal substeps, then its possible to
C	build a subsampled image from the full set of undersampled images.
C	This routine can presently handle 2X2 or 3X3 subsampling.  Operation
C	of this routine first requires generation of an ``offset'' file,
C	which include the relative location of a star or more precisely
C	the relative offsets of the images in pixels.  The format of the
C	file is to have a line for each image consisting of:
C
C		DR	DC	WT
C
C	where DR and DC give the relative offset of the particular image
C	and WT is a weighting factor (set it to unity is no weighting is 
C	desired).  Operation of this routine is as follows:
C
C	1. Before calling measure the relative offsets of the images
C	in a set and record (with weights) into a single offset file.
C
C	2. Use the Vista EXPAND routine to expand the image into a sparse
C	2X2 or 3X3 representation.
C
C	3. Compute the Fourier transform of the expanded buffer.
C
C	4. Call this routine on the Fourier buffer.  Give the order
C	of the image in the full set of images and the desired subsampling.
C	This routine computes and applies a complex multiplicative
C	coefficient to the Fourier image - the coeficient takes on different
C	values in different sectors of the domain.
C
C	5. Add the modified buffer into a Fourier accumulator buffer.
C
C	6. Transform back after the final image is has been added in.

C	Keywords:

C	OFFSETS=filename	Filename holding the relative offsets
C	N=n			Position of the current image to be
C				processed in the offset file.
C	SUB=n			Subsampling factor: 2X2 (default) or
C				3X3.


C       Author: Tod R. Lauer            Date: 2/11/98
C       				Mods: 9/14/98

        PARAMETER (PI=3.1415926, RADIAN=PI/180.0, NSTMAX=140, NDIV=3)
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:imagelink.inc'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	DIMENSION A(NCOL,NROW), PHIX(NSTMAX), PHIY(NSTMAX), WT(NSTMAX)
	real*8 spr, spi, rpr, rpi, cpr, cpi, ypr, ypi, tpr, tpi
	real*8 fr, fi
 	complex*16 phasem(NDIV**2,NDIV**2), vec(NDIV**2,NDIV**2)
	complex*16 rat, coef(NDIV**2), phases(NDIV**2,NSTMAX), temp
	character*80 filein, out
        CHARACTER*8 PARM
        INTEGER ROW, COL, vrow, key(NDIV**2)
	LOGICAL KEYCHECK, shift, offsets, position, substepin, pivot

C       Check command string

        IF (.NOT. GO) RETURN
        CALL KEYINIT
        CALL KEYDEF('DR=')		! Row shift amount
        CALL KEYDEF('DC=')		! Column shift amount
        CALL KEYDEF('OFFSETS=')		! Centroids filename
        CALL KEYDEF('N=')		! Position of image in stack
        CALL KEYDEF('SUB=')		! Subsampling factor
        IF (.NOT. KEYCHECK()) THEN
                XERR = .TRUE.
                RETURN
        END IF

C       Initialize parameters

	DR	=0.0
	DC	=0.0
	nsub	=2
	shift	=.false.		! Use SHIFT option only?
	offsets	=.false.		! Offset file given?
	position=.false.		! Order of image in list given?
	substepin	=.false.	! Substep specified?
	filein	=' '
        DO I=1, NCON
                IF (WORD(I)(1:3) .EQ. 'DR=') THEN
                        CALL ASSIGN(WORD(I),DR,PARM)
			shift	=.true.
                        IF (XERR) RETURN
                END IF

                IF (WORD(I)(1:3) .EQ. 'DC=') THEN
                        CALL ASSIGN(WORD(I),DC,PARM)
			shift	=.true.
                        IF (XERR) RETURN
                END IF

                IF (WORD(I)(1:8) .EQ. 'OFFSETS=') THEN
			filein	=word(i)(9: )
			offsets	=.true.
                END IF

                IF (WORD(I)(1:2) .EQ. 'N=') THEN
                        CALL ASSIGN(WORD(I),f,PARM)
			position=.true.
			npos	=nint(f)
                        IF (XERR) RETURN
                END IF

                IF (WORD(I)(1:4) .EQ. 'SUB=') THEN
                        CALL ASSIGN(WORD(I),f,PARM)
			substepin	=.true.
			nsub	=nint(f)
                        IF (XERR) RETURN
                END IF

	END DO

C	The image is complex, but stored and accessed as a real image

	if ((position .or. offsets .or. substepin) .and. shift) then
		print *,'Can''t request both alias stack and shifts...'
		xerr	=.true.
		return
	end if

C	In this block of code just apply a phase shift to the image and return.

	if (shift) then

        DO ROW=1,NROW
		IF (ROW .LE. NROW/2) THEN
			U	=FLOAT(ROW-1)/FLOAT(NROW)
		ELSE
			U	=FLOAT(ROW-NROW-1)/FLOAT(NROW)
		END IF

		RPHASE	=2.0*PI*DR*U
		RPR	=COS(RPHASE)
		RPI	=SIN(RPHASE)
                DO COL=1, NCOL, 2
			V	=FLOAT(COL-1)/FLOAT(NCOL)/2.0
			CPHASE	=2.0*PI*DC*V
			CPR	=COS(CPHASE)
			CPI	=SIN(CPHASE)
			FR	=A(COL,ROW)		! Real part
			FI	=A(COL+1,ROW)		! Imaginary part
			TPR	=RPR*CPR-RPI*CPI
			TPI	=RPI*CPR+RPR*CPI
			A(COL,ROW)	=FR*TPR-FI*TPI
			A(COL+1,ROW)	=FI*TPR+FR*TPI
		END DO
	END DO

	return
	end if

C	This next block of code is for calculating the phases with which
C	to add aliassed datasets.

	if (.not. position) then
		print *,'Position of image in stack not given...'
		xerr	=.true.
		return
	end if

	if (.not. offsets) then
		print *,'Offset data file not given...'
		xerr	=.true.
		return
	end if

	if (npos .gt. NSTMAX) then
	print *,'No more than ', NSTMAX,' images can be stacked...'
		xerr	=.true.
		return
	end if

	if (nsub .gt. NDIV) then
		print *,'No more than ', NDIV,'X subsampling allowed...'
		xerr	=.true.
		return

	else if (nsub .lt. 2) then
		print *,'Subsampling must be 2 or more...'
		xerr	=.true.
		return

	end if

	nsub2	=nsub*nsub

c	Construct the phase matrix

C	An external file gives the relative pixel offsets of the images.

	call filedef (filein,out,' ','.d')
	call vslower(out)
	open (10,file=out,status='old',iostat=ierr)
        IF (IERR .NE. 0) THEN
                PRINT *,'Cannot open ',out
                XERR = .TRUE.
                RETURN
        END IF
	ierr	=0
	np	=0

C	Count the number of images, and calculate realtive phases, referenced
C	to the first image.

	do while (ierr .eq. 0)
	read (10,*,iostat=ierr), yc, xc, weight
	if (ierr .eq. 0 .and. np .lt. NSTMAX) then
		np	=np+1
		if (np .eq. 1) then
			ycr	=yc
			xcr	=xc
			phix(np)=0.0
			phiy(np)=0.0
			wt(np)	=weight
		else
			dy	=yc-ycr
			dx	=xc-xcr
			phix(np)=float(nsub)*pi*dx
			phiy(np)=float(nsub)*pi*dy
			wt(np)	=weight
		end if
	end if
	end do

	close (10)

	if (np .lt. nsub2) then
		print *,'The centroids file must have at least ',
     1			nsub2, ' images...'
		xerr	=.true.
		return
	end if

C	Calculate the coefficients for each image.  The coefficient varies
C	accross the complex plane in segments depending on which aliassed
C	satellites contribute.  Find the coefficient for each segment.

	nsy	=nsub			! Number of V (Y) sections
	nsx	=(nsub-1)/2+1		! Number of U (X) sections
	isy	=-(nsub-1)/2
 	isy	=0
	isec	=0	! Section counter

	do iy	=isy,isy+nsy-1
	do ix	=0,nsx-1

	isec	=isec+1

C	First load the phase column matrix.  This gives the phases of the
C	aliased satellites and fundamental.  Count the satellites in X
C	and then Y.

	do nim=1,np
 		px	=-phix(nim)*2.0/float(nsub)	! Phase increments
 		py	=-phiy(nim)*2.0/float(nsub)
 		nuin	=ix-(nsub-1)/2
 		nvin	=iy
		pxi	=nuin*px	! Initial phases
		pyi	=-nvin*py

C		Calculate the phase of each satellite that contributes
C		to a given section.

		isat	=0
		do isaty=0,nsub-1
			do isatx=0,nsub-1
				isat	=isat+1
				phit	=isatx*px+pxi+isaty*py+pyi
 				phases(isat,nim)=cmplx(cos(phit),
     1					sin(phit))/float(nsub2)
			end do
		end do

C	Pivot if required so that the fundamental is always in column 1

			nfund	=1+nsub*nvin-nuin
			temp	=phases(1,nim)
			phases(1,nim)	=phases(nfund,nim)
			phases(nfund,nim)	=temp
	end do

C	This loads an identity matrix, which will be used to invert
C	the phase matrix.

	do i=1,nsub2
		do j=1,nsub2
 			vec(j,i)	=(0.0,0.0)
		end do

 		vec(i,i)=(1.0,0.0)
		key(i)	=i		! Initialize pivot pointers
	end do

C	If N>nsub2, then the problem is over determined - generate
C	a matrix that is the product of phases and its Hermitian
C	transpose, otherwise, just load the phase matrix.

C	The weighting factor is used at this point.

	if (np .gt. nsub2) then
		do i=1,nsub2
			do j=1,nsub2
				phasem(j,i)	=(0.0,0.0)
				do k=1,np
					phasem(j,i)	=phasem(j,i)+
     1				conjg(phases(i,k))*wt(k)*phases(j,k)
				end do
			end do
		end do
	else
		do i=1,nsub2
			do j=1,nsub2
				phasem(j,i)	=phases(j,i)
			end do
		end do
	end if

c	Solve for the data vector phases

	do i=1,nsub2-1
		do j=i+1,nsub2

c	Check for zero division and pivot if required.

			if (phasem(i,i)*conjg(phasem(i,i)).eq.0.0) then
				pivot	=.false.
				k	=i+1
				do while (.not. pivot .and. k.le.nsub2)
			if (phasem(i,k)*conjg(phasem(i,k)).ne.0.0) then

c			Pivot found - do pivot

				pivot	=.true.
				itemp	=key(i)		! Record pivot
				key(i)	=key(k)
				key(k)	=itemp
				do kk=1,nsub2
					temp	=phasem(kk,i)
					phasem(kk,i)	=phasem(kk,k)
					phasem(kk,k)	=temp
					temp	=vec(kk,i)
					vec(kk,i)	=vec(kk,k)
					vec(kk,k)	=temp
				end do

			else

c			Pivot not found, keep looking

				k	=k+1

			end if
				end do		! Pivot check loop

 				if (.not. pivot) then
				print *,'Singular phase matrix...'
					xerr	=.true.
					return
				end if

			end if

c	Any pivoting required is now completed.

			rat	=phasem(i,j)/phasem(i,i)
			do k=i,nsub2
				phasem(k,j)=phasem(k,j)-rat*phasem(k,i)
			end do

			do k=1,nsub2
				vec(k,j)	=vec(k,j)-rat*vec(k,i)
			end do
		end do
	end do

	do i=nsub2,2,-1
		rat	=phasem(i,i)
		do j=1,nsub2
			vec(j,i)	=vec(j,i)/rat
		end do

		do j=i-1,1,-1
			rat	=phasem(i,j)
			do k=1,nsub2
				vec(k,j)	=vec(k,j)-rat*vec(k,i)
			end do
		end do
	end do

	do j=1,nsub2
		vec(j,1)	=vec(j,1)/phasem(1,1)
	end do

C	The vec array now holds the inverse of the original phasem array.

C	If any pivoting has been done, undo it.

	do i=1,nsub2
		if (key(i) .ne. i) then
			k	=i+1
			do while (key(k) .ne. i .and. k .lt. nsub2)
				k	=k+1

			end do

			do kk=1,nsub2
				temp	=vec(kk,i)
				vec(kk,i)	=vec(kk,k)
				vec(kk,k)	=temp
			end do

			key(k)	=key(i)

		end if
	end do

C	For NSUB2 images, we are done

	if (np .eq. nsub2) then
		coef(isec)	=vec(npos,1)	! Scale factor for given image

C	Otherwise, we need to do a little more work.  Here we just solve
C	for the fundamental image.

	else
		coef(isec)	=0.0
		do i=1,nsub2
		coef(isec)=coef(isec)+vec(i,1)*conjg(phases(i,npos))
		end do
	end if

C	Addin weighting factor

	coef(isec)	=coef(isec)*wt(npos)

	print 102, npos, coef(isec)*conjg(coef(isec)), isec,
     1		 coef(isec)
101	format ('Image ',i2,' power ',f8.3,' sector ',i1)
102	format ('Image ',i2,' power ',f8.3,' sector ',i1,2(2x,f8.3))

	end do			! X section loop
	end do			! Y section loop

C	Now apply the complex scale factor to the transform.  A phase factor
C	is also applied to account for the fact that the image origin itself
C	needs to be shifted.

	isec	=0
	isv	=nrow/2
	iev	=isv-nrow/nsub+1
	do iy	=isy,isy+nsy-1

	ieu	=ncol-(nsub-1)*(nsx-1)*ncol/nsub	! Starting and ending U
	isu	=1
	do ix	=0,nsx-1

	isec	=isec+1

	SPR	=coef(isec)
	SPI	=dimag(coef(isec))
	do vrow=isv,iev,-1
		if (vrow .gt. 0) then
			row	=vrow
		else
			row	=nrow+vrow
		end if

		if (row .gt. NROW/2) then
			V	=FLOAT(ROW-NROW-1)/FLOAT(NROW)
		else
			V	=FLOAT(ROW-1)/FLOAT(NROW)
		end if

		RPHASE	=-2.0*phiy(npos)*V
		RPR	=COS(RPHASE)
		RPI	=SIN(RPHASE)
		ypr	=rpr*spr-rpi*spi
		ypi	=rpi*spr+rpr*spi
                DO COL=isu, ieu, 2
			U	=FLOAT(COL-1)/FLOAT(NCOL-2)/2.0
 			CPHASE	=-2.0*phix(npos)*U
			CPR	=COS(CPHASE)
			CPI	=SIN(CPHASE)
			tpr	=ypr*cpr-ypi*cpi
			tpi	=ypi*cpr+ypr*cpi
			FR	=A(COL,ROW)		! Real part
			FI	=A(COL+1,ROW)		! Imaginary part
			A(COL,ROW)	=FR*TPR-FI*TPI
			A(COL+1,ROW)	=FI*TPR+FR*TPI
		END DO
	END DO

	isu	=ieu+1
	ieu	=ncol-(nsx-2-ix)*ncol/nsub
	end do						! U loop

	isv	=iev-1
	iev	=isv-nrow/nsub+1
	if (iy .eq. isy+nsy-2) iev=-nrow/2+1
	end do						! V loop

        RETURN
        END
