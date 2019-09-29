#include "Vista.h"
        SUBROUTINE GETDATA(LABEL,DATA)

C       INPUT AND MODIFICATION OF PHOTOMETRY DATA

C -------------------------------------------------------------------------
C
C       This subroutine:
C               1) Displays the label and data for a photometry record
C                  on the VT100 screen.
C               2) Asks the user to enter or change certain records.
C               3) Returns the (changed) records to the main program.
C
C
C       This routine is called from the programs MARKSTAR and MODPHOT
C
C       Language:               Fortran
C       Author:                 Donald Terndrup
C       Date of last revision:  12/07/83
C -------------------------------------------------------------------------

#ifdef VMS
        INCLUDE 'VINCLUDE:PHOTFILES.INC'       ! Sizes of data record
#else
        INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif

        DIMENSION       DATA(NUMDATA)
        CHARACTER*80    LABEL
        CHARACTER*1     OPT
        LOGICAL         TOPOSN,  OK

C       TOPOSN is true if the RA and Dec are being converted from
C       seconds of time/arc to normal notation.
C       OK is true if the above conversion procedure worked.

C       Clear the screen.

  5     CALL COLOR('C')

C       Display those data in the photometry record which are defined.

C                       --- Header and Label ---

        PRINT *,' '
        PRINT *,LABEL

C                          --- Position ---

        PRINT 20,DATA(ROWLOC),DATA(COLLOC)
 20     FORMAT(25X,'Row: ',F6.1,8X,'Col: ',F6.1)

C                    --- Sky position, if known ---


        IF (DATA(CALCRA) .NE. 0.0) THEN
                TOPOSN = .TRUE.
                CALL RADEC(DATA(CALCRA),DATA(CALCDEC),
     .                              IHR,IMIN,RASEC,
     .                                     IDG,IDMN,DECSEC,TOPOSN,OK)
                PRINT 27,DATA(EPOCH),IHR,IMIN,RASEC,IDG,IDMN,DECSEC
  27            FORMAT(/2X,F5.0,' Calculated :  R.A. ',
     .                  I4,1X,I2,1X,F6.3,3X,
     .                  ' DEC. ',I4,1X,I2,1X,F6.3)
        ELSE
                PRINT 28
 28             FORMAT(/)
        END IF

        IF (DATA(TRIALRA) .NE. 0.0) THEN
                TOPOSN = .TRUE.
                CALL RADEC(DATA(TRIALRA),DATA(TRIALDEC),IHR,IMIN,
     &                RASEC,
     .                                   IDG,IDMN,DECSEC,TOPOSN,OK)
                PRINT 25,DATA(EPOCH),IHR,IMIN,RASEC,IDG,IDMN,DECSEC
  25            FORMAT(2X,F5.0,'       Trial:  R.A. ',I4,1X,I2,1X,F6.3,3X,
     &
     .                ' DEC. ',I4,1X,I2,1X,F6.3)

        ELSE
                PRINT *,' '
                PRINT *,'        Coordinates:'
        END IF

C                  --- Scale Factor and Brightness ---

        IF (DATA(SCALERATIO) .EQ. 0.0 .AND.
     .   DATA(ERRSCALERATIO) .EQ. 0.0) THEN
          PRINT 30
  30      FORMAT(/19X,'Scale factor .............. ')
        ELSE
          PRINT 35,DATA(SCALERATIO),DATA(ERRSCALERATIO)
  35      FORMAT(/19X,'Scale factor .............. ',1PE10.3,' +/- ',
     .         1PE9.2)
        END IF

        IF (DATA(SCALERATIO) .EQ. 0.0) THEN
          PRINT 40
  40      FORMAT(19X,'Total brightness ........... ')
        ELSE
          PRINT 45,DATA(TOTALAD),
     &          DATA(TOTALAD)*DATA(ERRSCALERATIO)
     .                                        /DATA(SCALERATIO)
  45      FORMAT(19X,'Calculated magnitude ...... ',1PE10.3,' +/- ',
     .          1PE9.2)
        END IF

C       Type message.

        PRINT *,' '
        PRINT *,'              To change the label,             type L'
        PRINT *,'             To enter or change coordinates,   type P'
        PRINT *,'              To exit,                         type E'
        PRINT *,' '
        PRINT 60
  60    FORMAT(/'                                         Option >> ',$)

C       Get answer, print message, accept numbers.

        READ (*,'(A1)'),OPT

        IF (OPT .EQ. 'E' .OR. OPT .EQ. 'e') THEN
          RETURN

        ELSE IF(OPT .EQ. 'L' .OR. OPT .EQ. 'l') THEN

          PRINT 100
 100      FORMAT(/'New Label >> ',$)
          READ(5,'(A80)')LABEL

        ELSE IF (OPT .EQ. 'P' .OR. OPT .EQ. 'p') THEN

 115      PRINT 120
 120      FORMAT(/'Enter epoch of coordinates (enter 0 to delete)  >> ',$)
          READ(5,*,ERR=115) DATA(EPOCH)

          IF (DATA(EPOCH) .LT. 0.0) THEN
                GOTO 115
          ELSE IF (DATA(EPOCH) .EQ. 0.0) THEN
                DATA(TRIALRA) = 0.0
                DATA(TRIALDEC) = 0.0
          ELSE

 123            PRINT 125
 125            FORMAT('Enter R.A. in format ##,##,##.## >> ',$)
                READ(5,*,ERR=123)IHR,IMIN,RASEC

 128            PRINT 130
 130            FORMAT('Enter Dec. in format ##,##,##.## >> ',$)
                READ(5,*,ERR=128) IDG,IDMN,DECSEC

                TOPOSN = .FALSE.
                CALL RADEC(DATA(TRIALRA),DATA(TRIALDEC),IHR,IMIN,
     &                RASEC,
     .                             IDG,IDMN,DECSEC,TOPOSN,OK)

                IF (.NOT. OK) GOTO 123

            END IF
        END IF

C       Display the numbers again

        GO TO 5

C       End of Program
        END
