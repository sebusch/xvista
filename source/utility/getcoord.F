#include "Vista.h"

C
C Subroutines for translating FITS header cards
C
C     GETCOORD - extract time/coordinate data in sexigesimal format
C                (i.e., hh:mm:ss)
C
C     GETDATE -  translate FITS date formats (old-style dd/mm/yy and
C                new-style ccyy-mm-dd and ccyy-mm-ddThh:mm:ss.sss)
C
C---------------------------------------------------------------------------

      SUBROUTINE GETCOORD(PARAM,IP1,IP2,FP3,SG)

C
C  GETCOORD - extract the time/position values from a FITS header card
C
C  Input:
C     PARAM = Input character string of the form
C              -xx:xx:xx.x where the leading sign is
C              optional.  The numeric fields are assumed to be
C              separated by colons and the first two fields
C              are assumed to be integers.  The third field
C              can be floating point, but does not have to be.
C
C  Output:
C     IP1 =   Integer value of first numeric field
C             Note that IP1 is positive even if the leading
C             negative sign was present.
C     IP2 =   Integer value of second numeric field
C     FP3 =   Floating value of third numeric field
C     SG  =   -1.0 if leading negative sign was present,
C             and +1.0 otherwise.

      CHARACTER*(*) PARAM
      CHARACTER*80 TEMP,TEMP2
      LOGICAL XERR
      REAL*8 FNUM

C  First strip out the colons or slashes

      L = MIN(80,NUMCHAR(PARAM))
      TEMP = PARAM(1:L)

C  Check for leading sign

      SG = 1.0
      I = 1
 8702 IF (TEMP(I:I) .EQ. ' ' .AND. I .LE. LEN(PARAM)) THEN
         I = I + 1
         GO TO 8702
      END IF

      IF(TEMP(I:I) .EQ. '-') THEN
         SG = -1.0
         TEMP(I:I) = ' '
      ELSE IF(TEMP(I:I) .EQ. '+') THEN
         TEMP(I:I) = ' '
      END IF

C  Replace delimiters with spaces
      
      DO 8701 I=1,L
         IF(TEMP(I:I) .EQ. ':' .OR. TEMP(I:I) .EQ. '/' .OR.
     &        TEMP(I:I) .EQ. '-' .OR. TEMP(I:I) .EQ. 'T')  
     &      TEMP(I:I) = ' '
 8701 CONTINUE

C  Extract the three parameters

      CALL DISSECT(TEMP,1,.FALSE.,NTYPE,IP1,FNUM,TEMP2,NCHAR,XERR)
      CALL DISSECT(TEMP,2,.FALSE.,NTYPE,IP2,FNUM,TEMP2,NCHAR,XERR)
      CALL DISSECT(TEMP,3,.FALSE.,NTYPE,IP3,FNUM,TEMP2,NCHAR,XERR)
      FP3 = FNUM
      IF (NTYPE .EQ. 1) FP3 = FLOAT(IP3)
      IP1 = IABS(IP1)

      RETURN
      END

C---------------------------------------------------------------------------

      INTEGER FUNCTION GETDATE(DATESTR,CCYY,MM,DD,UTH,UTM,UTS)

C
C  GETDATE - read date/time information out of FITS Header cards
C
C  Arguments
C  DATESTR = Input character string with the date/time information.
C
C            Accepted codings:
C
C            DD/MM/YY
C               Old-style (Pre-1999) FITS DATE-OBS dates.  These dates
C               are not Y2K compliant and give ambiguous results starting
C               in 2000.  As of 1999 Jan 1, the IAU FITS Working Group
C               mandates replacement with...
C
C            CCYY-MM-DD 
C               Y2K-compliant FITS DATE-OBS (starting 1999 Jan 1)
C
C            CCYY-MM-DDThh:mm:ss.sss 
C               "Full ISO" Y2K-compliant FITS DATE-OBS with time
C               The literal "T" is the ISO 8601 time designator
C
C          The Y2K-compliant date formats are a restricted subset of ISO-8601.
C
C  CCYY (integer) = Calendar Year
C    MM (integer) = Month in Year (1=Jan, 12=Dec)
C    DD (integer) = Day in month
C   UTH (integer) = UT Hours in the Day (hours since midnight)
C   UTM (integer) = UT Minutes in the Hour
C   UTS (real*4)  = UT Seconds in the minute.
C
C  Overview:
C
C  GETFITS translate dates from both old and newstyle FITS headers.  All
C  times are assumed to be UTC for dates since 1972 and UT before.  The
C  dates are assumed to follow the Gregorian Calendar.
C
C  The new format was approved on 1997 November 10 by a formal vote
C  of the IAU FITS Working Group, making it part of the FITS standard.
C  The ISO-8601 format will be formally required starting 1999 January 1.
C  
C  Return Values:
C     GETDATE = 0: got date but not time
C     GETDATE = 1: got date and time
C     GETDATE = -1: invalid, corrupted or unrecognized date format
C                   meaning that some error occurred in processing
C
C  R. Pogge
C  OSU Astronomy
C  1998 November 12
C
C---------------------------------------------------------------------------

C  External Variables

      CHARACTER*(*) DATESTR
      INTEGER CCYY, MM, DD
      INTEGER UTH, UTM
      REAL*4 UTS

C  Working Variables

      CHARACTER*80 TEMP,TEMP2
      INTEGER DELIMLOC(5)
      LOGICAL XERR
      REAL*8 FNUM
C  Format Codes:

      INTEGER FMTCODE
      INTEGER OLDSTYLE
      PARAMETER (OLDSTYLE = 1)
      INTEGER NEWSTYLE
      PARAMETER (NEWSTYLE = 2)
      INTEGER FULLISO
      PARAMETER (FULLISO = 3)
      INTEGER UNKNOWN
      PARAMETER (UNKNOWN = -1)

C  External Functions
      
      INTEGER NUMCHAR
      INTEGER UPPER

C  Yow!

      FMTCODE = UNKNOWN
      UTH = 0
      UTM = 0
      UTS = 0.0
      CCYY = 0
      MM = 0
      DD = 0

C  Determine the length of the string, make a working copy
C  in TEMP and force to uppercase for the tests

      NSTR = NUMCHAR(DATESTR)
      TEMP = DATESTR(1:NSTR)
      L = UPPER(TEMP)

C  Look to see what kind of DATE-OBS string we have been passed.
C  1) if /, assume old-style DD/MM/YY format
C  2) if - and no T, assume new-style CCYY-MM-DD format
C  3) if - and T and :,  assume full CCYY-MM-DDThh:mm:ss.ss
C  otherwise, invalid or corrupted date string

      if (INDEX(TEMP,'/') .NE. 0) THEN
         FMTCODE = OLDSTYLE
      else if (INDEX(TEMP,'-') .NE. 0) THEN
         FMTCODE = NEWSTYLE
         if (INDEX(TEMP,'T') .NE. 0) FMTCODE = FULLISO
      end if

      IF (FMTCODE .EQ. UNKNOWN) THEN
         GETDATE = -2
         RETURN
      END IF

C  Verify the different formats & locate the delimiters

C  OLDSTYLE:  dd/mm/yy format.  Remove the /s and replace with spaces
C             we should have encountered 2 /s for a valid format

      IF (FMTCODE .EQ. OLDSTYLE) THEN
         NSLASH = 0
         DO 10 I=1,NSTR
            IF (TEMP(I:I) .EQ. '/') THEN
               NSLASH = NSLASH + 1
               DELIMLOC(NSLASH) = I
               TEMP(I:I) = ' '
            END IF
 10      CONTINUE
         IF (NSLASH .NE. 2) THEN
            GETDATE = -1
            RETURN
         END IF

C  NEWSTYLE:  ccyy-mm-dd without time tag, must have 2 -s.  Same as above

      ELSE IF (FMTCODE .EQ. NEWSTYLE) THEN
         NSLASH = 0
         DO 11 I=1,NSTR
            IF (TEMP(I:I) .EQ. '-') THEN
               NSLASH = NSLASH + 1
               DELIMLOC(NSLASH) = I
               TEMP(I:I) = ' '
            END IF
 11      CONTINUE
         IF (NSLASH .NE. 2) THEN
            GETDATE = -1
            RETURN
         END IF

C  FULLISO:  ccyy-mm-ddThh:mm:ss.s.  We now need 2 -'s, 2 :'s and 1 T
         
      ELSE 
         NSLASH = 0
         DO 12 I=1,NSTR
            IF (TEMP(I:I) .EQ. '-') THEN
               NSLASH = NSLASH + 1
               DELIMLOC(NSLASH) = I
               TEMP(I:I) = ' '
            ELSE IF (TEMP(I:I) .EQ. 'T') THEN
               NSLASH = NSLASH + 1
               DELIMLOC(NSLASH) = I
               TEMP(I:I) = ' '
            ELSE IF (TEMP(I:I) .EQ. ':') THEN
               NSLASH = NSLASH + 1
               DELIMLOC(NSLASH) = I
               TEMP(I:I) = ' '
            END IF
 12      CONTINUE
         IF (NSLASH .NE. 5) THEN
            GETDATE = -1
            RETURN
         END IF

      END IF

C  Now take them apart

      if (FMTCODE .EQ. OLDSTYLE) THEN
         call dissect(temp,1,.false.,ntype,dd,fnum,temp2,nch,xerr)
         call dissect(temp,2,.false.,ntype,mm,fnum,temp2,nch,xerr)
         call dissect(temp,3,.false.,ntype,ccyy,fnum,temp2,nch,xerr)
         ccyy = ccyy + 1900
         GETDATE = 0
         if (xerr) GETDATE = -1

      else if (FMTCODE .EQ. NEWSTYLE) THEN
         call dissect(temp,1,.false.,ntype,ccyy,fnum,temp2,nch,xerr)
         call dissect(temp,2,.false.,ntype,mm,fnum,temp2,nch,xerr)
         call dissect(temp,3,.false.,ntype,dd,fnum,temp2,nch,xerr)
         GETDATE = 0
         if (xerr) GETDATE = -1

      else
         call dissect(temp,1,.false.,ntype,ccyy,fnum,temp2,nch,xerr)
         call dissect(temp,2,.false.,ntype,mm,fnum,temp2,nch,xerr)
         call dissect(temp,3,.false.,ntype,dd,fnum,temp2,nch,xerr)
         call dissect(temp,4,.false.,ntype,uth,fnum,temp2,nch,xerr)
         call dissect(temp,5,.false.,ntype,utm,fnum,temp2,nch,xerr)
         call dissect(temp,6,.false.,ntype,itmp,fnum,temp2,nch,xerr)
         uts = fnum
         if (ntype .eq. 1) uts = float(itmp)
         GETDATE = 1
         if (xerr) GETDATE = -1

      end if

      if (mm .gt. 12) getdate = -1
      if (dd .gt. 31) getdate = -1
      if (ccyy .lt. 1900) getdate = -1
      if (ccyy .gt. 2900) getdate = -1

      return
      END
