#include "Config.h"
!------------------------------------------------------------------------------
!  HALFTONE.FOR
!------------------------------------------------------------------------------
C===============================================================================
C===============================================================================
	SUBROUTINE PMGO(Halftone)(Nx, Ny, p, pmax, pmin)
!------------------------------------------------------------------------------
!       HALFTONE
!------------------------------------------------------------------------------
!  Halftone plot routine.  Intended to serve under MONGO graphics system.  One
!  may remove MONGO dependence by modifying the Halflim, driver routines.
!
!  Halftone produces a bitmap file (BITMAP.TMP) which is used by any of the
!  halftone device driver routines (e.g. HalfTT) to produce a random dot
!  halftone image.
!
!  Scaling of the intensities is done by drawing a dot with a probability
!               f = (p[x,y] - pmin)/(pmax - pmin).
!  In cases where p[x,y] is less than pmin or greater than pmax, truncation
!  or saturation takes place.
!
!  Whenever the picture size exceeds display screen workspace, sampling takes
!  place.  (Note: This may cause the max or min picture element to be missed.)
!------------------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	  INTEGER Nx, Ny  ! IN                  ! Size of picture data array
	  REAL p(Nx,Ny)   ! IN                  ! Picture data
	  REAL pmax, pmin ! IN,OUT              ! Limits

!------------------------------------------------------------------------------
	  INCLUDE 'JHALF.inc'                   ! Initialization of tables

	  PARAMETER (Mrn=4096)                  ! Size of random number table
	  INTEGER Bx, By                        ! Bitmap limits
	  REAL prange                           ! Range of picture values
	  REAL pmag                             ! Picture magnification factor
	  INTEGER isamp                         ! Sample spacing
	  INTEGER irep                          ! Number of cells to repeat
	  INTEGER x,y,dx,dy,i,j,h1,h2,bit,word,y_image
	  REAL rands(Mrn)                       ! Random number table
	  INTEGER irand                         ! Starting point in table
	  INTEGER krand                         ! Current random value
	  REAL seed /3.14159265/, MTH$RANDOM    ! Seed for MTH$RANDOM, decl for
	  EXTERNAL MTH$RANDOM                   ! library random number gen.
	  INTEGER chan,s                        ! I/O channel, status

	  INTEGER brecl                         ! Bitmap record length
	  INTEGER bp(129)                       ! Enough room for 4096 bits
						! --1 picture row for output
	  INTEGER Nbx, Nby                      ! Size of bitmap, bits
	  COMMON /half/ brecl,bp,Nbx,Nby

	  CHARACTER*4  switch                   ! either VERS or IMAG
	  COMMON /pswch/ switch

!------------------------------------------------------------------------------
! Get picture limits

	  CALL PTUV(Halflim)(Nx, Ny, p, pmax, pmin, Bx, By)
	  pmag = FLOAT(Bx)/FLOAT(Nx)
	  IF (pmag .GE. 1.0) THEN
	    irep = Bx/Nx
	    isamp = 1
	  ELSE
	    irep = 1
	    isamp = Nx/Bx
	  ENDIF
	  prange = pmax - pmin
	  brecl = (Bx+31)/32            ! Note: recl's in longwords for
					! unformatted I/O.
	  Nbx = Bx
	  Nby = By

!------------------------------------------------------------------------------
! Open the bitmap file

	  CALL LIB$GET_LUN (chan)
	  OPEN (unit=chan,file='BITMAP.TMP',err=999,iostat=s,recl=brecl,
	1       form='unformatted',status='new')

!------------------------------------------------------------------------------
! Build random number table

	  DO i=1,Mrn
	    rands(i) = prange*MTH$RANDOM (seed) + pmin
	  ENDDO
	  irand = 1
	  krand = 1

!------------------------------------------------------------------------------
! Do a row at a time, filling the bitmap with random 1's and 0's, depending
! upon picture brightness.

	  h1 = -isamp/2
	  h2 = h1+isamp-1
	  y = Ny
	  DO WHILE (y .GT. 0) ! rows
	    y_image = y
	    IF (switch.eq.'IMAG') y_image = 1 + Ny -y   !Imagen is upside down
	    DO j=1,irep ! repeat rows
	      DO i=1,128
		bp(i) = 0
	      ENDDO                             ! Erase bit row to 0's.
	      bit = 0
	      word = 1
	      x = 1 - h1
	      DO WHILE (x .LE. Nx) ! cols
		DO i=1,irep ! repeat pixels
		  IF (p(x,y_image) .GT. rands(krand))
	1               bp(word) = bp(word) .OR. bits(bit)
						! Insert bit in row
		  krand = krand + 1
		  IF (krand .GT. Mrn) krand = 1
		  IF (krand .EQ. irand) THEN
		    krand = 1 + Mrn*MTH$RANDOM(seed)
		    irand = krand
		  ENDIF
						! If out of random numbers,
						! choose a new starting place.
		  bit = (bit + 1) .AND. 31
		  IF (bit .EQ. 0) word = word + 1
		ENDDO ! repeat pixels
		x = x + isamp
	      ENDDO ! cols
	      WRITE (chan, err=998) (bp(i),i=1,brecl)
						! Write row out to file.
	    ENDDO ! repeat rows
	    y = y - isamp
	  ENDDO ! rows

!------------------------------------------------------------------------------
! Close bitmap file

	  CLOSE (chan)
	  CALL LIB$FREE_LUN (chan)
	  RETURN


!------------------------------------------------------------------------------
! Error handlers
999       TYPE *, 'HALFTONE--I/O error trying to open BITMAP file.'
	  CALL LIB$FREE_LUN (chan)
	  RETURN
998       TYPE *, 'HALFTONE--I/O error trying to write BITMAP file.'
	  CALL CLOSE (chan)
	  CALL LIB$FREE_LUN (chan)
	  RETURN
	END
C===============================================================================
C===============================================================================
	SUBROUTINE PTUV(Halflim)(Nx, Ny, p, pmax, pmin, Bx, By)
!------------------------------------------------------------------------------
!               HALFLIM
!------------------------------------------------------------------------------
!  Find limits for plot.  MONGO-dependent.
!------------------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	  INTEGER Nx, Ny  ! IN                  ! Size of picture data array
	  REAL p(Nx,Ny)   ! IN                  ! Picture data
	  REAL pmax, pmin ! IN,OUT              ! Limits
	  INTEGER Bx, By                        ! Bitmap dimensions
	  INTEGER Dx, Dy                        ! Plotter window dimensions
	  INTEGER mag, demag                    ! Picture magnification
	  INTEGER x, y

!------------------------------------------------------------------------------

	  INCLUDE 'MONGOPAR.inc'                ! Mongo declarations,
						! such as LX1..LY2, GX1..GX2

!------------------------------------------------------------------------------
!  Picture max and min...

	  IF (pmax .EQ. pmin) THEN ! default to max,min value actually found
	    pmin = p(1,1)
	    pmax = p(1,1)
	    DO y=1,Ny
	      DO x=1,Nx
		IF (p(x,y) .GT. pmax) pmax = p(x,y)
		IF (p(x,y) .LT. pmin) pmin = p(x,y)
	      ENDDO
	    ENDDO
	  ENDIF

!------------------------------------------------------------------------------
!  Bitmap limits...

	  Dx = GX2 - GX1
	  Dy = GY2 - GY1
	  mag = MIN (Dx/Nx, Dy/Ny)
	  IF (mag .GT. 0) THEN
	    Bx = mag*Nx
	    By = mag*Ny
	  ELSE
	    demag = 1 + MAX (Nx/Dx, Ny/Dy)
	    Bx = Nx/demag
	    By = Ny/demag
	  ENDIF

!------------------------------------------------------------------------------
!  Re-do plotter limits

	  GX2 = GX1 + Bx
	  GY2 = GY1 + By

	END
C===============================================================================
C===============================================================================
!------------------------------------------------------------------------------
!  Device Drivers
!------------------------------------------------------------------------------
	SUBROUTINE PMGO(HalfTT)(IOOK)
!------------------------------------------------------------------------------
!               HALFTT
!------------------------------------------------------------------------------
!  Write bitmap halftone picture to TT: device (VT100 + Retrographics)
!  MONGO-dependent.
!------------------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	  LOGICAL IOOK                          ! Whether I/O went OK.

	  INTEGER brecl                         ! Bitmap record length
	  INTEGER bp(129)                       ! Enough room for 4096 bits
						! --1 picture row for output
	  INTEGER Nbx, Nby                      ! Size of bitmap, bits
	  COMMON /half/ brecl,bp,Nbx,Nby

	  INTEGER bits (0:31)                   ! Powers of 2
	  INTEGER bitrev5 (0:31)                ! Table for bit-reversing 5-bit
						! characters
	  INTEGER bitrev8 (-128:127)            ! Table for bit-reversing 8-bit
						! characters
	  COMMON /halftbls/ bits, bitrev5, bitrev8

	  INTEGER x,y,s,i
	  CHARACTER*200 plotline                ! Line of characters to plot
	  INTEGER ixpos, iypos                  ! Screen coord.
	  CHARACTER*3 xpos, ypos                ! Character codes for (x,y)
	  INTEGER Nchar                         ! How many characters to plot
						!  /raster line
	  INTEGER byt                           ! One 5-bit nibble from bp
	  CHARACTER*1 CAN, ESC, GS              ! ASCII control characters
!------------------------------------------------------------------------------
	  INCLUDE 'MONGOPAR.inc'               ! Mongo declarations,
						! such as LX1..LY2, GX1..GX2

!------------------------------------------------------------------------------
! Open the bitmap file

	  IOOK = .FALSE.
	  CALL LIB$GET_LUN (chan)
	  OPEN (unit=chan,file='BITMAP.TMP',err=999,iostat=s,recl=brecl,
	1       form='unformatted',status='old')


!------------------------------------------------------------------------------
! Read lines from bitmap file; convert to ASCII chars and plot on screen.

	  ixpos = GX1/5
	  GX1 = 5*ixpos                 ! Truncate to factor of 5 (Retro-Gr.
					! forces this in x positioning)
	  GX2 = GX1 + Nbx               ! Re-do MONGO limits
	  iypos = GY2                   ! Top left corner
	  CAN = CHAR(24)
	  ESC = CHAR(27)
	  GS = CHAR(29)

	  DO y=Nby,1,-1
	    ENCODE (3, 1, xpos) ixpos
	    ENCODE (3, 1, ypos) iypos
1           FORMAT (I3.3)
					! Convert (x,y) to character commands.
	    READ (chan, err=998) (bp(x),x=1,brecl)
					! Read row from file.
	    iypos = iypos - 1           ! Step y position down one notch.
	    ibit = 0                    ! Set bit pointer to start of line
	    nb = 5
	    i = 0
	    DO WHILE (nb .GT. 0)
	      i = i + 1
	      byt = LIB$EXTZV (ibit, nb, bp)
	      plotline(i:i) = CHAR ( bitrev5(byt) + '40'X)
	      ibit = ibit + nb
	      nb = MIN ((j+1)*Nbx-ibit, 5)
	    ENDDO
	    CALL PTUV(vtgwrite)
c           Send GS to enter vector mode.  Send escape sequence for dot address.
     &      (GS // ESC // '"' // xpos // ';' // ypos // 'a')
c           Load row of dots in VTG memory.  (Immediately, these are displayed.)
	    CALL PTUV(vtgwrite)
     &      (ESC // '+' // plotline(1:i) // '#' // CAN)
	  ENDDO

!------------------------------------------------------------------------------
! Close bitmap file

	  CLOSE (unit=chan, status='DELETE')
	  CALL LIB$FREE_LUN (chan)
	  IOOK = .TRUE.
	  RETURN


!------------------------------------------------------------------------------
! Error handlers
999       TYPE *, 'HALFTONE--I/O error trying to open BITMAP file.'
	  CALL LIB$FREE_LUN (chan)
	  RETURN
998       TYPE *, 'HALFTONE--I/O error trying to read BITMAP file.'
	  CLOSE (unit=chan, status='DELETE')
	  CALL LIB$FREE_LUN (chan)
	  RETURN
	END
C===============================================================================
C===============================================================================
	SUBROUTINE PMGO(HalfLV)(IOOK)
!------------------------------------------------------------------------------
!               HALFLV
!------------------------------------------------------------------------------
!  FOR VERSETEC.
!  Write bitmap1 file ready for inclusion by MONGO with its box, labels, etc.
!  MONGO-dependent.  Must have a bitmap.tmp input file available.
!------------------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	  LOGICAL IOOK                          ! Whether I/O went OK.

	  INTEGER brecl                         ! Bitmap record length
	  INTEGER bp(129)                       ! Enough room for 4096 bits
						! --1 picture row for output
	  INTEGER Nbx, Nby                      ! Size of bitmap, bits
	  COMMON /half/ brecl,bp,Nbx,Nby

	  INTEGER bits (0:31)                   ! Powers of 2
	  INTEGER bitrev5 (0:31)                ! Table for bit-reversing 5-bit
						! characters
	  INTEGER bitrev8 (-128:127)            ! Table for bit-reversing 8-bit
						! characters
	  COMMON /halftbls/ bits, bitrev5, bitrev8

	  INTEGER x,y,s,i,k,ibit,lh
	  BYTE bytebuf(528)                     ! Buffer for raster
	  INTEGER ixpos, iypos                  ! Screen coord. (left marg)
	  INTEGER ixbyte, ixword, iwoff         ! Byte # in output buffer,
						! word # in input
						! buffer, bit offset.
	  INTEGER in, out                       ! I/O channels
	  CHARACTER*15 homedir                  ! User's login directory name
	  CHARACTER*40 tempfile                 ! Name of file generated by
						! HalfLV
!------------------------------------------------------------------------------
	  INCLUDE 'MONGOPAR.inc'               ! Mongo declarations,
						! such as LX1..LY2, GX1..GX2

!------------------------------------------------------------------------------
! Open the bitmap files

	  IOOK = .FALSE.
	  CALL LIB$GET_LUN (in)
	  OPEN (unit=in,file='BITMAP.TMP',err=999,iostat=s,recl=brecl,
	1       form='unformatted',status='old')
	  tempfile = 'SYS$LOGIN:BITMAP1.TMP'
	  CALL LIB$GET_LUN (out)
	  OPEN (unit=out,file=tempfile,err=997,iostat=s,recl=66,
	1       form='unformatted',status='new')

!------------------------------------------------------------------------------
! Write vertical data into file
	  WRITE (out, err=996) GY1, GY2, LY1, LY2

!------------------------------------------------------------------------------
! Read lines from bitmap file; convert to ASCII chars and plot on screen.

	  ixpos = MIN (IFIX(GX1), 2000)
	  ixword = ixpos / 32
	  iwoff = ixpos - 32*ixword
	  ixbyte = (ixpos-iwoff) / 8
	  DO i=1, 264
	    bytebuf(i) = 0
	  ENDDO
						! Clear output buffer.
	  DO i=1, 128
	    bp(i) = 0
	  ENDDO
						! Clear input buffer.
	  DO y=Nby,1,-1
	    READ (in, err=998) (bp(x+1),x=1,brecl)
					! Read row from file.
	    iypos = iypos - 1           ! Step y position down one notch.
	    ibit = 32 - iwoff           ! Set bit pointer to output byte
					! boundary.
	    nb = 8
	    i = 0

	    DO WHILE (nb .GT. 0)        ! For each pixel,...
	      i = i + 1
	      k = 0                             ! Clear out old garbage.
	      k = LIB$EXTZV (ibit, nb, bp) .AND. (bits(nb)-1)
						! Pick up 8 bit pattern
						! (returned as INT*4)
	      IF (k .GT. 127) k=k-256           ! Make into signed integer
	      bytebuf(ixbyte+i) = bitrev8(k)    ! Use bit-reverse table for
						! 8-bit byte.
	      ibit = ibit + nb                  ! Bump pointer
	      nb = MIN (32+Nbx-ibit, 8)         ! Decide how many bits to get
						! next.  Partial field for
						! last byte, zero at end.
	    ENDDO                       ! pixels
	    WRITE (out, err=996) (bytebuf(x),x=1,264)
						! Write all 264 bytes.
	  ENDDO

!------------------------------------------------------------------------------
! Close bitmap files

	  CLOSE (in,status='DELETE')
	  CLOSE (out)
	  CALL LIB$FREE_LUN (in)
	  CALL LIB$FREE_LUN (out)
	  IOOK = .TRUE.
	  RETURN

!------------------------------------------------------------------------------
! Error handlers
996       TYPE *, 'HALFTONE--I/O error trying to write BITMAP1 file.'
c         CLOSE (out, status='DELETE')
	  CALL LIB$FREE_LUN (out)
	  RETURN
997       TYPE *, 'HALFTONE--I/O error trying to open BITMAP1 file.'
	  CALL LIB$FREE_LUN (out)
	  RETURN
998       TYPE *, 'HALFTONE--I/O error trying to read BITMAP file.'
c         CLOSE (in, status='DELETE')
	  CALL LIB$FREE_LUN (in)
	  RETURN
999       TYPE *, 'HALFTONE--I/O error trying to open BITMAP file.'
	  CALL LIB$FREE_LUN (in)
	  RETURN

	END
C===============================================================================
C===============================================================================
	SUBROUTINE PMGO(HalfIMAGEN)(IOOK)
!------------------------------------------------------------------------------
!               HALFIMAGEN
!------------------------------------------------------------------------------
!  FOR IMAGEN.
!  Write bitmap1 file ready for inclusion by MONGO with its box, labels, etc.
!  MONGO-dependent.  Must have a bitmap.tmp input file available.
!------------------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	  LOGICAL IOOK                          ! Whether I/O went OK.

	  INTEGER brecl                         ! Bitmap record length
	  INTEGER bp(129)                       ! Enough room for 4096 bits
						! --1 picture row for output
	  INTEGER Nbx, Nby                      ! Size of bitmap, bits
	  COMMON /half/ brecl,bp,Nbx,Nby

	  INTEGER bits (0:31)                   ! Powers of 2
	  INTEGER bitrev5 (0:31)                ! Table for bit-reversing 5-bit
						! characters
	  INTEGER bitrev8 (-128:127)            ! Table for bit-reversing 8-bit
						! characters
	  COMMON /halftbls/ bits, bitrev5, bitrev8

	  INTEGER x,y,s,i,k,ibit,lh
	  BYTE bytebuf(528)                     ! Buffer for raster
	  INTEGER ixpos, iypos                  ! Screen coord. (left marg)
	  INTEGER ixbyte, ixword, iwoff         ! Byte # in output buffer,
						! word # in input
						! buffer, bit offset.
	  INTEGER in, out                       ! I/O channels
	  INTEGER plus_shift,minus_shift        ! number of bits to shift image
	  INTEGER left,right,bp_tmp             ! bit-shifted words
	  CHARACTER*15 homedir                  ! User's login directory name
	  CHARACTER*40 tempfile                 ! Name of file generated by
						! HalfLV
!------------------------------------------------------------------------------
	  INCLUDE 'MONGOPAR.inc'               ! Mongo declarations,
						! such as LX1..LY2, GX1..GX2

!------------------------------------------------------------------------------
! Open the bitmap files

	  IOOK = .FALSE.
	  CALL LIB$GET_LUN (in)
	  OPEN (unit=in,file='BITMAP.TMP',err=999,iostat=s,recl=brecl,
	1       form='unformatted',status='old')
	  tempfile = 'SYS$LOGIN:BITMAP1.TMP'
	  CALL LIB$GET_LUN (out)
	  OPEN (unit=out,file=tempfile,err=997,iostat=s,recl=66,
	1       form='unformatted',status='new')

!------------------------------------------------------------------------------
! Write co-ords of corner of image (GX1,GY1), number of words per line, and
! number of lines. Because Imagen operates  on full words, NUMLINES must be
! a multiple of 32.
	  NUMWORDS = brecl      ! = 1 + (Nbx-1)/32
	  NUMLINES = Nby
	  iGX1 = IFIX(GX1)
	  iGY1 = IFIX(GY1)

! Read lines from bitmap file. If GX1 is not a multiple of 32, pad each line.
! Plus_shift is the number of blank bits to pad with on the left.
! IPADTOP is the number of blank lines to pad with at top of file (which
! actually becomes the bottom of the image, since the image is displayed
! bottom up from lower left-hand corner).

	  plus_shift = MOD(iGX1,32) !Possibly iGX1 - 4 for XYSWAPPED
	  minus_shift = plus_shift - 32
	  IPADTOP = MOD(IGY1,32)
	  IF (XYSWAPPED) IPADTOP = IPADTOP + 16  ! Imagen weirdness fudge
	  IPADBOTTOM = 32 - (MOD((IPADTOP+NUMLINES),32))

	  JGX1 = IGX1
	  IF (plus_shift.GT.0) THEN
	    JGX1 = (IGX1/32) * 32
	    NUMWORDS = NUMWORDS + 1
	  ENDIF
	  JGY1 = IGY1
	  IF (IPADTOP.NE.0) THEN
	    JGY1 = (IGY1/32) * 32
	    NUMLINES = NUMLINES + IPADTOP + IPADBOTTOM
	  ENDIF

	  WRITE (out, err=996) JGX1, JGY1,NUMWORDS,NUMLINES,XYSWAPPED

!         ixpos = MIN (IFIX(GX1), 2000)
!         ixword = ixpos / 32
!         iwoff = ixpos - 32*ixword
!         ixbyte = (ixpos-iwoff) / 8
!         DO i=1, 264
!           bytebuf(i) = 0
!         ENDDO
						! Clear output buffer.
	  DO i=1, 129
	    bp(i) = 0
	  ENDDO

C  If GY1 is not a multiple of 32, pad with blank lines.
	  DO J=1,IPADTOP
	    WRITE (out, err=996) (bp(i),i=1,NUMWORDS)
	  ENDDO

						! Clear input buffer.
	  DO y=Nby,1,-1
	    READ (in, err=998) (bp(x),x=1,brecl)
					! Read row from file.

C  It appears necessary to reverse each word (byte by byte).
!           DO M=1,BRECL
!             BP_TMP = BP(I)
!             DO N=1,4
!               BP_TMP1(N


	    IF (plus_shift.GT.0) THEN
	      right = 0
	      DO I=1,NUMWORDS-1
		bp_tmp = bp(I)
		left = JISHFT (bp_tmp,plus_shift)
		bp(I) = left .OR. right
		right = JISHFT (bp_tmp,minus_shift)
	      ENDDO
	      bp(NUMWORDS) = right
	    ENDIF

	    WRITE (out, err=996) (bp(i),i=1,NUMWORDS)
	  ENDDO
						! Clear output buffer.
	  DO i=1, 129
	    bp(i) = 0
	  ENDDO

C  If NUMLINES is not a multiple of 32, pad with blank lines.
	  DO J=1,IPADBOTTOM
	    WRITE (out, err=996) (bp(i),i=1,NUMWORDS)
	  ENDDO

!------------------------------------------------------------------------------
! Close bitmap files

	  CLOSE (in,status='DELETE')
	  CLOSE (out)
	  CALL LIB$FREE_LUN (in)
	  CALL LIB$FREE_LUN (out)
	  IOOK = .TRUE.
	  RETURN


!------------------------------------------------------------------------------
! Error handlers
996       TYPE *, 'HALFTONE--I/O error trying to write BITMAP1 file.'
c         CLOSE (out, status='DELETE')
	  CALL LIB$FREE_LUN (out)
	  RETURN
997       TYPE *, 'HALFTONE--I/O error trying to open BITMAP1 file.'
	  CALL LIB$FREE_LUN (out)
	  RETURN
998       TYPE *, 'HALFTONE--I/O error trying to read BITMAP file.'
c         CLOSE (in, status='DELETE')
	  CALL LIB$FREE_LUN (in)
	  RETURN
999       TYPE *, 'HALFTONE--I/O error trying to open BITMAP file.'
	  CALL LIB$FREE_LUN (in)
	  RETURN

	END
C===============================================================================
C===============================================================================
	subroutine PTUV(vtgwrite)(buf)

C       WRITE CHARACTERS TO THE TERMINAL WITHOUT THE CARRIAGE-RETURN
C       OR LINE-FEED CHARACTERS WHICH FORTRAN NORMALLY SENDS.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	implicit integer*4 (a-z)
	character*(*) buf
	common/vtgunitcom/ vtgunit
	common/vtgcom/ichan
	logical go,check,xerr
C       common/con/ go,check,xerr
	external io$_writevblk,io$m_noformat
	if(ichan.lt.0) call PTUV(vtgopen)
	l=len(buf)
	if(vtgunit.ne.0) then
		if(buf(l:l).eq.char(24)) l=l-1
		if(l.eq.0) return
		write(vtgunit,1000,iostat=status) buf(1:l)
		if(status.ne.0) then
			type *,'VTGWRITE--Write error to external plot file:',
	1                       status
			xerr=.false.
		end if
	else
		func=%loc(io$_writevblk).or.%loc(io$m_noformat)
		status=sys$qiow(,%val(ichan),%val(func),,,,%val(%loc(buf)),
     #          %val(l),,%val(0),,)
		if(.not.status) call lib$signal(status)
	end if
1000    format(1x,a)
	return
	end
C===============================================================================
C===============================================================================
	subroutine PTUV(vtgopen)

C       OPEN A SYSTEM CHANNEL FOR VTGWRITE
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

	implicit integer*4 (a-z)
	character physname*8
	common/vtgcom/ichan
	data ichan/-1/
c       Translate TT to current physical terminal name
	status=sys$trnlog('TT',t1,physname,,,)
c       Assign channel
	status=sys$assign(physname,chan,,)
	ichan=chan
	return
	end
C===============================================================================
C===============================================================================
