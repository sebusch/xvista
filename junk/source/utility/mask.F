#include "Vista.h"
C       Here is a package for handling pixel masking.  If pixel masking
C       is not desired, use the following subroutines.

c       SUBROUTINE MASK(COL,ROW)
c       INTEGER         COL, ROW
c       RETURN
c       END
c       SUBROUTINE UNMASK(COL,ROW)
c       INTEGER          COL,ROW
c       RETURN
c       END
c       SUBROUTINE CLEARMASK
c       RETURN
c       END
c       LOGICAL FUNCTION MASKED(COL,ROW)
c       INTEGER                 COL,ROW
c       MASKED = .FALSE.
c       RETURN
c       END

C       ----------------------------------------------------------------------

        SUBROUTINE MASK(COL,ROW)

C       Mask the pixel (COL,ROW)

        INTEGER         COL, ROW

C       Include the mask.

#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
        INCLUDE 'VINCLUDE:MASK.INC'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/mask.inc'
#endif

        LOGICAL GETBIT, MASKED

C       Get the word and bit for (COL,ROW).

        IF (.NOT. GETBIT(COL,ROW,IWD,IBT)) RETURN

C       Set the bit.
        CALL CCMASKSET(LOCMASK, IWD, IBT, 1, MASKED)
C        PIXMASK(IWD) = IBSET(PIXMASK(IWD),IBT)

        RETURN
        END

C       ----------------------------------------------------------------------

        SUBROUTINE UNMASK(COL,ROW)

C       Unmask the pixel (COL,ROW)

        INTEGER         COL, ROW

C       Include the mask.

#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
        INCLUDE 'VINCLUDE:MASK.INC'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/mask.inc'
#endif

        LOGICAL GETBIT, MASKED

C       Get the word and bit for (COL,ROW).
        IF (.NOT. GETBIT(COL,ROW,IWD,IBT)) RETURN

C       Clear the bit.
        CALL CCMASKSET(LOCMASK, IWD, IBT, 0, MASKED)
C        PIXMASK(IWD) = IBCLR(PIXMASK(IWD),IBT)

        RETURN
        END

C       ---------------------------------------------------------------------

        SUBROUTINE CLEARMASK

C       Clears the mask array.

#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
        INCLUDE 'VINCLUDE:MASK.INC'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/mask.inc'
#endif

	CALL CCFREE(0,LOCMASK)
        LOCMASK = 0
        NRMASK = 0
        NCMASK = 0
        SRMASK = 0
        SCMASK = 0
        MASKSIZE = 0
C        DO 8701 I=1, MASKSIZE
C                PIXMASK(I) = 0
C8701    CONTINUE

        RETURN
        END

C       ---------------------------------------------------------------------
        SUBROUTINE NEWMASK(NR,NC,SR,SC)

	INTEGER NR, NC, SR, SC
#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
        INCLUDE 'VINCLUDE:MASK.INC'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/mask.inc'
#endif

        NRMASK = NR
        NCMASK = NC
        SRMASK = SR
        SCMASK = SC
        MASKSIZE = NRMASK*NCMASK/32
        IF (MASKSIZE*32 .NE. NRMASK*NCMASK) MASKSIZE=MASKSIZE+1
        IF (LOCMASK .NE. 0) CALL CCFREE(MASKSIZE*4,LOCMASK)
        IF (MASKSIZE .GT. 0) THEN
          CALL CCALLOC(MASKSIZE*4,LOCMASK)
	  CALL CCZEROMASK(LOCMASK,MASKSIZE)

	  PRINT 101, SC, SC+NC-1,SR,SR+NR-1
101       FORMAT(
     &'Created new MASK array with dimensions (', I5,':',I5,',',I5,':',I5,')')
        ELSE
          PRINT *, 'Requested MASK has zero size! No MASK created...'
        END IF

	RETURN
	END
C       ---------------------------------------------------------------------

        LOGICAL FUNCTION MASKED(COL,ROW)

C       Returns .TRUE. if the pixel (COL,ROW) is masked.  See the file
C       'MASK.INC' for a description of the mask array.  This routine
C       is called by various VISTA analysis routines which test for masked
C       pixels.

        INTEGER COL, ROW

C       Include the mask.

#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
        INCLUDE 'VINCLUDE:MASK.INC'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/mask.inc'
#endif

        LOGICAL GETBIT, BTEST

C       Get the word and bit for (COL,ROW).

        IF (.NOT. GETBIT(COL,ROW,IWD,IBT)) THEN
	  MASKED = .FALSE.
	  RETURN
	END IF

C       Test the bit.
        CALL CCMASKSET(LOCMASK, IWD, IBT, -1, MASKED)
C        MASKED = BTEST(PIXMASK(IWD),IBT)

        RETURN
        END

C       -----------------------------------------------------------------------

        LOGICAL FUNCTION GETBIT(COL,ROW,IWD,IBT)

C       Finds the bit which corresponds to (COL,ROW).  This bit is bit
C       IBT in MASK(IWD).

#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
        INCLUDE 'VINCLUDE:MASK.INC'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/mask.inc'
#endif

        INTEGER*4       COL, ROW
	DATA NRMASK,NCMASK,SRMASK,SCMASK /0,0,0,0/
        DATA VERBOSEMASK /.FALSE./
        DATA MISSEDMASK /.FALSE./
        DATA LOCMASK /0/

	IF (LOCMASK .EQ. 0 .OR. NRMASK .EQ. 0 .OR. NCMASK .EQ. 0) THEN
          PRINT *, 'No mask currently defined'
	  GETBIT = .FALSE.
          RETURN
        END IF

C       Compute and test the bit number for (COL,ROW).

        IF (ROW .LT. SRMASK .OR. ROW .GT. SRMASK+NRMASK-1  .OR.
     &      COL .LT. SCMASK .OR. COL .GT. SCMASK+NCMASK-1) THEN
          IF (VERBOSEMASK)  THEN
            PRINT *, 'WARNING: Cannot mask pixels with ROW or COL < ',
     &           SRMASK, SCMASK
            PRINT *, '   or with ROW or COL > ', 
     &           SRMASK+NRMASK-1, SCMASK+NCMASK-1
          END IF
          MISSEDMASK = .TRUE.
          GETBIT = .FALSE.
          RETURN
        END IF

        IBIT = (ROW-SRMASK) * NCMASK + (COL-SCMASK)

C       Find the word and bit in that word which corresponds to IBIT.
        IBT = MOD(IBIT,32)
        IWD = MASKSIZE - IBIT/32

C       Return.

        GETBIT = .TRUE.

        RETURN
        END

        SUBROUTINE MASKSET(PIXMASK, IWD, IBT, ISET, MASKED)

	INCLUDE 'vistadisk/source/include/mask.inc'
        LOGICAL MASKED
        INTEGER*4 PIXMASK(IWD)

        IF (ISET .EQ. 1) THEN
          PIXMASK(IWD) = IBSET(PIXMASK(IWD),IBT)
        ELSE IF (ISET .EQ. 0) THEN
          PIXMASK(IWD) = IBCLR(PIXMASK(IWD),IBT)
        ELSE
          MASKED = BTEST(PIXMASK(IWD),IBT)
        END IF

	RETURN
	END

        SUBROUTINE ZEROMASK(MASK,NWORDS)
        INTEGER*4 MASK(NWORDS)

        DO I=1,NWORDS
          MASK(I) = 0
        END DO

        RETURN
        END


