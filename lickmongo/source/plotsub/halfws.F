#include "Config.h"
C=======================================================================
C=======================================================================
	SUBROUTINE PMGO(HalfWS)(IOOK)
!------------------------------------------------------------------------------
!               HALFWS
!------------------------------------------------------------------------------
!  Write bitmap halftone picture to WORKSTATION device
!  MONGO-dependent.
!------------------------------------------------------------------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	  LOGICAL IOOK                          ! Whether I/O went OK.
	  INTEGER FOUND,LIB$FFS                 ! 1 if a bit is set in word
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
	  LOGICAL*4 LASTWORDMASK
	  LOGICAL*4 MASK(0:32)/0,1,3,7,'F'X,'1F'X,'3F'X,'7F'X,'FF'X,
	1       '1FF'X,'3FF'X,'7FF'X,'FFF'X,'1FFF'X,'3FFF'X,'7FFF'X,
	2       'FFFF'X,'1FFFF'X,'3FFFF'X,'7FFFF'X,'FFFFF'X,'1FFFFF'X,
	3       '3FFFFF'X,'7FFFFF'X,'FFFFFF'X,'1FFFFFF'X,'3FFFFFF'X,
	4       '7FFFFFF'X,'FFFFFFF'X,'1FFFFFFF'X,'3FFFFFFF'X,
	5       '7FFFFFFF'X,'FFFFFFFF'X/
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
! Read lines from bitmap file; convert to POINTS and plot on screen.

	  GX2 = GX1 + Nbx               ! Re-do MONGO limits
	  IYPOS = GY2                   ! Top left corner
	  LASTWORDBITS=NBX-BRECL*32
	  LASTWORDMASK=MASK(LASTWORDBITS)

	  DO Y=NBY,1,-1
	    READ (chan, err=998) (bp(x),x=1,brecl)
					! Read row from file.
	    IYPOS = IYPOS - 1           ! Step y position down one notch.
	    DO I=1,BRECL
		IXWORD=GX1+(I-1)*32     ! Set bit pointer to start of WORD
		IWORD=BP(I)
		IF(I.EQ.BRECL) IWORD=IWORD.AND.LASTWORDMASK
		IX=0
		FOUND=1
		DO WHILE (FOUND.EQ.1)
		  ILIMIT=MAX(32-IX,0)
		  FOUND=LIB$FFS(IX,ILIMIT,IWORD,INEXT)
		  IF(FOUND.EQ.1) THEN
		    IXPOS=IXWORD+INEXT
		    CALL PTUV(T4010RELOC)(IXPOS,IYPOS)
		    CALL PTUV(T4010DRAW)(IXPOS,IYPOS)
		    IX=INEXT+1
		  END IF
		END DO
	    ENDDO
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
C=======================================================================
C=======================================================================

