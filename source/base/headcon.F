#include "Vista.h"
      SUBROUTINE HEADCON 

C
C   VISTA Version 3.5
C
C   HEADCON Command
C
C     Decodes "foreign" (i.e., non-VISTA) fits headers into the format
C     appropriate to VISTA.   This will not affect the contents of the
C     source tape FITS headers, but the data written - either to disk or
C     tape - from VISTA will have VISTA style header cards.
C
C --------------------------
C
C   Command Syntax:
C
C      HEADCON  imno  [MCD] [LAPALMA] [CTIO] [KPNO] [PAL]
C
C   where:
C
C      imno         is the image buffer with the header to be converted
C
C   one only of each of the following:
C
C      [MCD]        McDonald Observatory CCD Data (ARGUS headers)
C
C      [LAPALMA]    La Palma Observatory (INT and Herschel Telescopes)
C
C      [CTIO]       CTIO FITS Tapes
C
C      [KPNO]       Kitt Peak FITS Tapes
C
C      [PAL]        Palomar FITS Tapes written by OBSERVE
C
C  -----------------------------------
C
C   Author:  Rick Pogge
C            McDonald Observatory
C            The University of Texas at Austin
C            RLM 15.308
C            Austin, TX 78712
C
C   Date Written: 1988 December 22
C
C   Modification History
C
C	Palomar/OBSERVE option added by BF, 7/90.
C
C-----------------------------------------------------------------------------

C   Yow!

C   Communication with VISTA

#ifdef VMS
	INCLUDE 'VINCLUDE:vistalink.inc'		
	INCLUDE 'VINCLUDE:imagelink.inc'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C   Declarations:
	LOGICAL KEYCHECK

C   List of allowed keywords.
	CALL KEYINIT

	CALL KEYDEF('MCD')
        CALL KEYDEF('LAPALMA')
        CALL KEYDEF('CTIO')
        CALL KEYDEF('KPNO') 
        CALL KEYDEF('PALOMAR')

C   Check to see that the user did not give any unknown keywords
	IF (.NOT. KEYCHECK()) THEN
            XERR = .TRUE.
            RETURN
	END IF

C   Check for keywords.
	DO I=1, NCON

C   McDonald Observatory Data

	    IF (WORD(I)(1:3) .EQ. 'MCD') THEN	
                  CALL MCDCONV(IM)
                  RETURN

C   LaPalma Observatory Data

            ELSE IF (WORD(I)(1:7) .EQ. 'LAPALMA') THEN
C                  CALL LAPALMACONV(IM)
                  PRINT *, 'CANNOT YET CONVERT LAPALMA FITS HEADERS'
                  RETURN

C   CTIO Data

            ELSE IF (WORD(I)(1:4) .EQ. 'CTIO') THEN
C                 CALL NOAOCONV(IM)
                  PRINT *, 'CANNOT YET CONVERT CTIO FITS HEADERS'
                  RETURN

C   KPNO Data

            ELSE IF (WORD(I)(1:4) .EQ. 'KPNO') THEN
C                 CALL NOAOCONV(IM)
                  PRINT *, 'CANNOT YET CONVERT KITT PEAK FITS HEADERS'
                  RETURN

C   Palomar Data written with OBSERVE (e.g. DBSP)

            ELSE IF (WORD(I)(1:3) .EQ. 'PAL') THEN
                  CALL PALOMARCONV(IM)
                  RETURN

            END IF

        END DO

        RETURN

        END

C-----------------------------------------------------------------------------

      SUBROUTINE MCDCONV(IMNO)

C
C   Called by HEADCON Command when MCD keyword is invoked
C
C   Converts McDonald FITS headers into VISTA format
C
C  -------------------------------------
C
C   McDonald CCD Data taking systems make FITS headers in the old ARGUS
C   format, which contains some unique ways of encoding data which are useful 
C   to VISTA for spectral reduction and record keeping.
C  
C   The Conversion Table gives different FITS card names, and the
C   respective formats.
C
C   MCDCONV will replace the McDonald cards with those appropriate to VISTA
C
C  -------------------------------------
C
C   Conversion Table:
C
C                          McDonald Card           VISTA Card            
C      Quantity              (format)               (format)     
C    -----------------------------------------------------------
C      Exposure Time        EXTTIME                EXPOSURE
C                           (I4 1/10sec)           (I4 sec)
C    -----------------------------------------------------------
C      Observation Date     DATE_OBS                DATE-OBS
C                           (C mm/dd/yy)            (C mm/dd/yy)
C?????????????????????????????????????????????????????????????
C            Looks like VISTA uses the format dd/mm/yy to me --- BF
C    -----------------------------------------------------------
C      UT Start Time        UT_STRT                 TIME
C                           (C hhmmss__)            (C hh:mm:ss.s)
C    -----------------------------------------------------------
C      Right Ascension      RA_STRT                 RA
C                           (C hhmmsss_)            (C hh:mm:ss.s)
C    -----------------------------------------------------------
C      Declination          DEC_STRT                DEC
C                           (C ddmmsssD)            (C +dd:mm:ss.s)
C                            D = N/S
C    -----------------------------------------------------------
C      Hour Angle           HA_STRT                 HA
C                           (C hhmmsssD)            (C -hh:mm:ss.s)
C                            D = E/W
C    -----------------------------------------------------------
C      Tape Number          no trans                TAPENUM
C    -----------------------------------------------------------
C      Observation Number   FILENAME                OBSNUM
C                           (C iiNNNNN_)            (I4 NNN)
C                           "ii" = initials
C    -----------------------------------------------------------
C      Image Status         DATA_ID                 STATUS
C                           (C)                     (C)     <- APPROX TRANS
C    -----------------------------------------------------------
C
C    
C -------------------------------------------------------------------------
C
C   Author:  Rick Pogge
C            McDonald Observatory
C            The University of Texas at Austin
C            RLM 15.308
C            Austin, TX  78746
C
C   Date Written:  1988 December 22
C
C   Modification History:
C
C
C-------------------------------------------------------------------------------

#ifdef VMS
        include 'VINCLUDE:vistalink.inc'                  
        include 'VINCLUDE:imagelink.inc'                  
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C   Declarations.

        CHARACTER*80  CHEAD, FSTRCAT
        CHARACTER*64 FCHAR1, FCHAR2

        INTEGER   INHEAD
        INTEGER   IVALUE

C   Convert Exposure Time  (note: INT_TIME not used with CCDs in general)

        IVALUE = INHEAD('EXP_TIME',HEADBUF(1,IMNO))
      
        IF (IVALUE .LT. 10) THEN
            PRINT *, 'Exposure Time less than 1 second'
            PRINT '('' EXP_TIME = '',I4)', IVALUE
            PRINT *, 'Set artificially to ZERO internally to VISTA'
            IVALUE = 0
            CALL INHEADSET('EXPOSURE',IVALUE,HEADBUF(1,IMNO))

        ELSE
            IVALUE = IVALUE / 10
            CALL INHEADSET('EXPOSURE',IVALUE,HEADBUF(1,IMNO))
            CALL UNFIT('EXP_TIME',HEADBUF(1,IMNO))
        END IF

C   Convert RA

        FCHAR1 = ' '
        FCHAR2 = ' '

        FCHAR1 = CHEAD('RA_STRT', HEADBUF(1,IMNO))

        IF (FCHAR1 .EQ. ' ') THEN 
           FCHAR2 = '00:00:00.0'
        ELSE
           FCHAR2(1:8) = FSTRCAT(FCHAR1(1:2),
     &                    FSTRCAT(':',
     &                     FSTRCAT(FCHAR1(3:4),
     &                      FSTRCAT(':',FCHAR1(5:6)))))
           FCHAR2(9:10) = FSTRCAT('.',FCHAR1(7:7))
        END IF

        CALL CHEADSET('RA',FCHAR2,HEADBUF(1,IMNO))
        CALL UNFIT('RA_STRT',HEADBUF(1,IMNO))
        CALL UNFIT('RA_STOP',HEADBUF(1,IMNO))
                
C   Convert DEC

        FCHAR1 = ' '
        FCHAR2 = ' '

        FCHAR1 = CHEAD('DEC_STRT', HEADBUF(1,IMNO))

        IF (FCHAR1 .EQ. ' ') THEN
            FCHAR2 = '+00:00:00.0'
        ELSE      
            IF (FCHAR1(8:8) .EQ. 'N') FCHAR2(1:1) = '+'
            IF (FCHAR1(8:8) .EQ. 'S') FCHAR2(1:1) = '-'

            FCHAR2(2:4) = FSTRCAT(FCHAR1(1:2),':')
            FCHAR2(5:7) = FSTRCAT(FCHAR1(3:4),':')
            FCHAR2(8:11) = FSTRCAT(FCHAR1(5:6),
     &                      FSTRCAT('.',FCHAR1(7:7)))
        END IF

        CALL CHEADSET('DEC',FCHAR2,HEADBUF(1,IMNO))
        CALL UNFIT('DEC_STRT',HEADBUF(1,IMNO))
        CALL UNFIT('DEC_STOP',HEADBUF(1,IMNO))

C   Convert Starting HA

        FCHAR1 = ' '
        FCHAR2 = ' ' 
        FCHAR1 = CHEAD('HA_STRT',HEADBUF(1,IMNO)) 
        IF (FCHAR1 .EQ. ' ') THEN 
          FCHAR2 = ' 00:00:00.0' 
        ELSE IF (FCHAR1(8:8) .EQ. 'E') THEN
          FCHAR2(1:1) = '-' 
          IF (FCHAR1(8:8) .EQ. 'W') FCHAR2(1:1) = ' ' 
          FCHAR2(2:4) = FSTRCAT(FCHAR1(1:2),':')
          FCHAR2(5:7) = FSTRCAT(FCHAR1(3:4),':')
          FCHAR2(8:11) = FSTRCAT(FCHAR1(5:6),
     &                      FSTRCAT('.',FCHAR1(7:7)))
        END IF

        CALL CHEADSET('HA',FCHAR2,HEADBUF(1,IMNO))
        CALL UNFIT('HA_STRT',HEADBUF(1,IMNO))
        CALL UNFIT('HA_STOP',HEADBUF(1,IMNO))

C   Convert UT starting time

        FCHAR1 = ' '
        FCHAR2 = ' '

        FCHAR1 = CHEAD('UT_STRT',HEADBUF(1,IMNO))

        IF (FCHAR1 .EQ. ' ') THEN
            FCHAR2 = '00:00:00.0'

        ELSE
            FCHAR2(1:3) = FSTRCAT(FCHAR1(1:2),':')
            FCHAR2(4:6) = FSTRCAT(FCHAR1(3:4),':')
            FCHAR2(7:10) = FSTRCAT(FCHAR1(5:6),'.0')
        END IF

        CALL CHEADSET('TIME',FCHAR2,HEADBUF(1,IMNO))
        CALL UNFIT('UT_STRT',HEADBUF(1,IMNO))
        CALL UNFIT('UT_STOP',HEADBUF(1,IMNO))      

C   Convert UT Date

        FCHAR1 = ' '
        FCHAR2 = ' '

        FCHAR1 = CHEAD('DATE_OBS',HEADBUF(1,IMNO))

        IF (FCHAR1 .EQ. ' ') FCHAR1 = '01/01/01'

        CALL CHEADSET('DATE-OBS',FCHAR1,HEADBUF(1,IMNO))
        CALL UNFIT('DATE_OBS',HEADBUF(1,IMNO))

C   Convert "status" card

        FCHAR1 = ' '

        FCHAR1 = CHEAD('DATA_ID',HEADBUF(1,IMNO))

        IF (FCHAR1 .EQ. ' ') FCHAR1 = 'Unknown'

        CALL CHEADSET('STATUS',FCHAR1,HEADBUF(1,IMNO))

C   Convert Observation Number

        FCHAR1 = ' '

        FCHAR1 = CHEAD('FILENAME',HEADBUF(1,IMNO))

        IF (FCHAR1 .EQ. ' ') THEN
            IVALUE = 0
        ELSE
            READ (FCHAR1(3:7),*,ERR=1) IVALUE
            GO TO 2
    1       IVALUE = 0
            WRITE (44,100)
  100       FORMAT(' Observation number could not be converted.')
    2       CONTINUE
        END IF

        CALL INHEADSET('OBSNUM',IVALUE,HEADBUF(1,IMNO))
        
C   All Done.

        RETURN

        END      

C-----------------------------------------------------------------------------

      SUBROUTINE PALOMARCONV(IMNO)

C
C   Called by HEADCON Command when PAL keyword is invoked
C
C   Converts Palomar FITS headers written by OBSERVE into VISTA format.
C   Note that the OBSERVE cards are written at the END of an exposure,
C   requiring some adjustment to the UT time and hour angle cards.
C
C  -------------------------------------
C
C   The Conversion Table gives different FITS card names, and the
C   respective formats.
C
C  -------------------------------------
C
C   Conversion Table:
C
C                          OBSERVE Card           VISTA Card            
C      Quantity              (format)               (format)     
C    -----------------------------------------------------------
C      Exposure Time        TIME                   EXPOSURE
C                           (I4 sec)               (I4 sec)
C    -----------------------------------------------------------
C      Observation Date     DATE                    DATE-OBS
C                           (C dd-mmm-yyyy)         (C dd/mm/yy)
C    -----------------------------------------------------------
C      UT Time              UT (end time)           TIME (start time)
C                           (F dec. hours)          (C hh:mm:ss.s)
C    -----------------------------------------------------------
C      Right Ascension      RA (end RA)             RA (start RA)
C                           (F dec. hours)          (C hh:mm:ss.s)
C    -----------------------------------------------------------
C      Declination          DEC (end Dec.)          DEC (start Dec.)
C                           (F dec. degree)         (C +dd:mm:ss.s)
C                           DECS
C                           (C sign of Dec.)
C    -----------------------------------------------------------
C      Hour Angle           HA (end HA)             HA (start HA)
C                           (F dec. hours)          (C -hh:mm:ss.s)
C                           HAS
C                           (C sign of Dec.)
C    -----------------------------------------------------------
C      Tape Number          no trans                TAPENUM
C    -----------------------------------------------------------
C      Observation Number   FRAME                   OBSNUM
C                           (I NNN)                 (I4 NNN)
C    -----------------------------------------------------------
C    
C -------------------------------------------------------------------------
C
C   Author:  BF
C            Caltech 105-24
C
C   Date Written:  1990 July 15
C
C   Modification History:
C
C
C-------------------------------------------------------------------------------

#ifdef VMS
        include 'VINCLUDE:vistalink.inc'                  
        include 'VINCLUDE:imagelink.inc'                  
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C   Declarations.

        CHARACTER*80 CHEAD, FSTRCAT
        CHARACTER*64 FCHAR1, FCHAR2, DECSIGN, HASIGN

        INTEGER   INHEAD
        INTEGER   IVALUE

C   Convert Exposure Time  (note: TIME is the actual integration time, while
C   ETIME is what was typed in at the beginning of the exposure.

        IVALUE = INHEAD('TIME',HEADBUF(1,IMNO))
      
        IF (IVALUE .LT. 1) THEN
            PRINT *, 'Exposure Time less than 1 second'
            PRINT '('' EXP_TIME = '',I4)', IVALUE
            PRINT *, 'Set artificially to ZERO internally to VISTA'
            IVALUE = 0
            CALL INHEADSET('EXPOSURE',IVALUE,HEADBUF(1,IMNO))

        ELSE
            CALL INHEADSET('EXPOSURE',IVALUE,HEADBUF(1,IMNO))
            CALL UNFIT('TIME',HEADBUF(1,IMNO))
            CALL UNFIT('ETIME',HEADBUF(1,IMNO))
        END IF

C   Convert RA.  Note that in OBSERVE all header information is written
C   for the END of the exposure, not the start as with Lick data-taking
C   routines.  Hence we need to correct the hour angle for the exposure time.

	FTIME = FLOAT(IVALUE)

        RA = FHEAD('RA', HEADBUF(1,IMNO))

	IRAHRS = IFIX(RA)
	RA = 60.*(RA - IRAHRS)

	IRAMINS = IFIX(RA)
	RASECS = 60.*(RA - IRAMINS)

C   Parse the hours, minutes, and seconds into a string.

	FCHAR2 = '00:00:00.0'
	WRITE (FCHAR2(1:2),100) IRAHRS
	WRITE (FCHAR2(4:5),100) IRAMINS
	WRITE (FCHAR2(7:10),101) RASECS

  100   FORMAT(I2)
  101   FORMAT(F4.1)
  102   FORMAT(A1)
  103   FORMAT(A2)

        CALL UNFIT('RA',HEADBUF(1,IMNO))
        CALL CHEADSET('RA',FCHAR2,HEADBUF(1,IMNO))
                
C   Convert DEC

        DEC = FHEAD('DEC', HEADBUF(1,IMNO))
	DECSIGN = CHEAD('DECS',HEADBUF(1,IMNO))

	IDECDEGS = IFIX(DEC)
	DEC = 60.*(DEC - IDECDEGS)

	IDECMINS = IFIX(DEC)
	DECSECS = 60.*(DEC - IDECMINS)

C   Parse the degrees, minutes, and seconds into a string.

	FCHAR2 = '+00:00:00.0'
	WRITE (FCHAR2(1:1),102) DECSIGN(1:1)
	WRITE (FCHAR2(2:3),100) IDECDEGS
	WRITE (FCHAR2(5:6),100) IDECMINS
	WRITE (FCHAR2(8:11),101) DECSECS

        CALL UNFIT('DEC',HEADBUF(1,IMNO))
        CALL CHEADSET('DEC',FCHAR2,HEADBUF(1,IMNO))

C	Convert Starting HA.  Remember that this must be converted into the 
C	starting hour angle, since OBSERVE writes the ending hour angle.
C	N.B.  AN IMPLICIT ASSUMPTION IS MADE HERE THAT EITHER THE TELESCOPE
C	WAS TRACKING, OR ELSE THAT IT DOESN''T MATTER WHETHER YOU USE THE
C	START OR END H.A.

        HA = FHEAD('HA', HEADBUF(1,IMNO))
	HASIGN = CHEAD('HAS',HEADBUF(1,IMNO))

	IF (HASIGN(1:1) .EQ. '+') THEN
		HA = ABS(HA) - FTIME/3600.
	ELSE
		HA = -ABS(HA) - FTIME/3600.
	END IF

        IF (HA .LT. 0.) THEN
            HASIGN(1:1) = '-'
        ELSE
            HASIGN(1:1) = '+'
        END IF
        HA = ABS(HA)

	IHAHRS = IFIX(HA)
	HA = 60.*(HA - IHAHRS)

	IHAMINS = IFIX(HA)
	HASECS = 60.*(HA - IHAMINS)

C   Parse the degrees, minutes, and seconds into a string.

	FCHAR2 = '+00:00:00.0'
	WRITE (FCHAR2(1:1),102) HASIGN(1:1)
	WRITE (FCHAR2(2:3),100) IHAHRS
	WRITE (FCHAR2(5:6),100) IHAMINS
	WRITE (FCHAR2(8:11),101) HASECS

        CALL UNFIT('HA',HEADBUF(1,IMNO))
	CALL UNFIT('HAS',HEADBUF(1,IMNO))
        CALL CHEADSET('HA',FCHAR2,HEADBUF(1,IMNO))

C	Convert UT starting time.  Again, note that the OBSERVE FITS card must
C	be converted from ending time to starting time.

        UT = FHEAD('UT', HEADBUF(1,IMNO))

	UT = UT - FTIME/3600.
	IF (UT .LT. 0.) UT = UT + 24.

	IUTHRS = IFIX(UT)
	UT = 60.*(UT - IUTHRS)

	IUTMINS = IFIX(UT)
	UTSECS = 60.*(UT - IUTMINS)

C   Parse the degrees, minutes, and seconds into a string.

	FCHAR2 = '00:00:00.0'
	WRITE (FCHAR2(1:2),100) IUTHRS
	WRITE (FCHAR2(4:5),100) IUTMINS
	WRITE (FCHAR2(7:10),101) UTSECS

        CALL UNFIT('UT',HEADBUF(1,IMNO))
        CALL CHEADSET('TIME',FCHAR2,HEADBUF(1,IMNO))

C   Convert UT Date

        FCHAR1 = ' '
        FCHAR2 = ' '

        FCHAR1 = CHEAD('DATE',HEADBUF(1,IMNO))

        IF (FCHAR1 .EQ. ' ') FCHAR1 = '00-JAN-1900'

C      Find the position of the first hyphen (since numbers below ten do
C      not have a zero in front of them in OBSERVE.

      I = INDEX(FCHAR1,'-')
      IF (I .LE. 2) THEN
            FCHAR1 = FSTRCAT('0',FCHAR1)
      END IF

	IF (FCHAR1(4:6) .EQ. 'JAN') THEN
		MONTH = 1
	ELSE IF (FCHAR1(4:6) .EQ. 'FEB') THEN
		MONTH = 2
	ELSE IF (FCHAR1(4:6) .EQ. 'MAR') THEN
		MONTH = 3
	ELSE IF (FCHAR1(4:6) .EQ. 'APR') THEN
		MONTH = 4
	ELSE IF (FCHAR1(4:6) .EQ. 'MAY') THEN
		MONTH = 5
	ELSE IF (FCHAR1(4:6) .EQ. 'JUN') THEN
		MONTH = 6
	ELSE IF (FCHAR1(4:6) .EQ. 'JUL') THEN
		MONTH = 7
	ELSE IF (FCHAR1(4:6) .EQ. 'AUG') THEN
		MONTH = 8
	ELSE IF (FCHAR1(4:6) .EQ. 'SEP') THEN
		MONTH = 9
	ELSE IF (FCHAR1(4:6) .EQ. 'OCT') THEN
		MONTH = 10
	ELSE IF (FCHAR1(4:6) .EQ. 'NOV') THEN
		MONTH = 11
	ELSE IF (FCHAR1(4:6) .EQ. 'DEC') THEN
		MONTH = 12
	END IF

	FCHAR2 = '00/00/00'
	WRITE (FCHAR2(4:5),104) MONTH
  104   FORMAT(I2.2)
	FCHAR2(1:2) = FCHAR1(1:2)
	FCHAR2(7:8) = FCHAR1(10:11)

        CALL UNFIT('DATE',HEADBUF(1,IMNO))
        CALL CHEADSET('DATE-OBS',FCHAR2,HEADBUF(1,IMNO))

C   Convert Observation Number

	IVALUE = INHEAD('FRAME',HEADBUF(1,IMNO))

        CALL INHEADSET('OBSNUM',IVALUE,HEADBUF(1,IMNO))
        
C   All Done.

        RETURN

        END      

C-----------------------------------------------------------------------------


