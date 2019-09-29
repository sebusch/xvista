#include "Vista.h"
	SUBROUTINE AUTOMARK(A,ISROW,IEROW,ISCOL,IECOL)

C       Locate stars in an image.

C       The image:
	REAL*4  A(ISCOL:IECOL,ISROW:IEROW)

C       This routine finds stars by first locating the peak pixels in
C       an image.  The centroid is computed around each of these peaks.
C
C               AUTOMARK imno [RANGE=low,high] [RADIUS=rad] [REJECT=rej]
C                             [BOX=b] [NEW] [AUTO]
C
C       where
C               imno            is the number of the image being processed
C               RANGE           specifies the range of values at the peak
C                               which are used.  Peaks higher or lower than
C                               these are ignored.  If no range is given,
C                               the program searches for peaks in the
C                               range 0 to 1.0E10.  If only one value is
C                               given, the program searches from that
C                               value upwards to 1.0E10.
C               RADIUS=rad      size of the image which is used in the
C                               computation of the centroid.  If not
C                               given, it defaults to 2 pixels.
C               REJECT=rej      ignore all peaks that are within a
C                               specified radius 'rej' of ANY masked pixel.
C               BOX=b           find stars in box 'b' only
C               NEW             make a new list.  Otherwise, append to the
C                               old list.
C               AUTO            uses position from old photometry file 
C                               as starting guess for new positions.
C               SILENT          suppresses terminal output
C
C
C       Donald Terndrup
C       Lick Observatory
C       University of California
C       Santa Cruz, CA  95064
C       June 1985

C       Modified by:    April Atwood    1/87
C       Modified by:    J. Holtzman    12/91 for AUTO option

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
	INCLUDE 'VINCLUDE:PHOTFILES.INC'       ! Photometry files
	INCLUDE 'VINCLUDE:TIMEDATA.INC'        ! Observation parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
	INCLUDE 'vistadisk/source/include/timedata.inc'
#endif
C       Other declarations.

	CHARACTER*80    LABEL
	CHARACTER*8     PARM

	INTEGER         SR, SC, ER, EC
	REAL*4          RANGE(2)

	LOGICAL         HAVERANGE, HAVEBOX, HAVEREJECT, HAVERADIUS, COMPLETE
	LOGICAL         OK, OBSNUM, SILENT, NOQUESTION, AUTO, PMASKED
	LOGICAL         KEYCHECK, ISONLIST, MASKNEAR, NEWLIST, FINDCENT, MASK
	LOGICAL         WASFOUND(MAXNUMSTAR), HAVERSHIFT, HAVECSHIFT
	COMMON /WORK/   WASFOUND

	INTEGER DAONUMS
	COMMON /LISTOFPOSITIONS/ ROWS(MAXNUMSTAR),
     &        COLS(MAXNUMSTAR), DAONUMS(MAXNUMSTAR)

C      Photometry record
	REAL*4          PHOT(NUMDATA)
	REAL*8          DVAL
C       Conversion of radians to seconds of time.

	PARAMETER (CONVERSN = 180.0 / 3.141592654 * 3600.0 / 15.0)

	COMMON /MOMENT/ FMOMENT, SILENT

	DATA            LABEL /' '/
	DATA            PHOT  /NUMDATA * 0.0/

C       List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('RANGE=')
	CALL KEYDEF('RADIUS=')
	CALL KEYDEF('REJECT=')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('NEW')
	CALL KEYDEF('DMIN=')
	CALL KEYDEF('MOMENT=')
	CALL KEYDEF('EDGE=')
        CALL KEYDEF('ID=')
	CALL KEYDEF('OBSNUM')
	CALL KEYDEF('SILENT')
	CALL KEYDEF('FORCE')
	CALL KEYDEF('NITER=')
	CALL KEYDEF('AUTO')
	CALL KEYDEF('DR=')
	CALL KEYDEF('DC=')
	CALL KEYDEF('RSHIFT=')
	CALL KEYDEF('CSHIFT=')
	CALL KEYDEF('MASK')
	CALL KEYDEF('COMPLETE')

C       Exit if we have a keyword that we won''t understand.
	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       If the user wanted to search a spectrum, we must exit.
	IF (ISROW .EQ. IEROW) THEN
		PRINT *,'You cannot run AUTOMARK on a spectrum.'
		XERR = .TRUE.
		RETURN
	END IF

C       Look for the NEW word.
	NEWLIST = .FALSE.
	DO 8701 I=1,NCON
		IF (WORD(I) .EQ. 'NEW') NEWLIST = .TRUE.
8701    CONTINUE

C       Begin a new list if there is one already.
	IF (NEWLIST) THEN
		IF (NSTARS .GT. 0) CALL CLOSEPHOT
		CALL OPENPHOT
		NSTARS = 0
	ELSE
		IF (NSTARS .EQ. 0) CALL OPENPHOT
	END IF

C       Initializations.
	HAVERANGE  = .FALSE.
	HAVEBOX    = .FALSE.
	HAVEREJECT = .FALSE.
	HAVERADIUS = .FALSE.
	NOQUESTION = .FALSE.
	AUTO = .FALSE.
        HAVERSHIFT = .FALSE.
        HAVECSHIFT = .FALSE.
        MASK = .FALSE.
        COMPLETE = .FALSE.

	REJECT   = 0.0
	FMOMENT = 0.0
	DMIN = 1.
	EDGE = 0.0
        ID = 1
	OBSNUM = .FALSE.
	SILENT = .FALSE.
	MAXITER = 6

	DR = 0.0
	DC = 0.0

C       Get the options for the program.
	DO 8702 I=1, NCON
		IF (WORD(I)(1:6) .EQ. 'RANGE=') THEN
			CALL ASSIGNV(WORD(I),2,RANGE,NF,PARM)
			IF (XERR) RETURN
			IF (NF .LT. 1) THEN
		         PRINT *,'Error:  Must give argument in RANGE.'
	 		 XERR = .TRUE.
			 RETURN
			ELSE IF (NF .EQ. 1) THEN
			 RANGE(2) = 1.0E10
			END IF
			HAVERANGE = .TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			NBOX    = INT(TEMP)
			HAVEBOX = .TRUE.

		ELSE IF (WORD(I)(1:7) .EQ. 'RADIUS=') THEN
			CALL ASSIGN(WORD(I),RADIUS,PARM)
			IF (XERR) RETURN
			HAVERADIUS = .TRUE.

		ELSE IF (WORD(I)(1:7) .EQ. 'REJECT=') THEN
			CALL ASSIGN(WORD(I),REJECT,PARM)
			IF (XERR) RETURN
			HAVEREJECT = .TRUE.
                        MASK = .TRUE.

		ELSE IF (WORD(I)(1:5) .EQ. 'EDGE=') THEN
			CALL ASSIGN(WORD(I),EDGE,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:7) .EQ. 'MOMENT=') THEN
			CALL ASSIGN(WORD(I),FMOMENT,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:5) .EQ. 'DMIN=') THEN
			CALL ASSIGN(WORD(I),DMIN,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:3) .EQ. 'DR=') THEN
			CALL ASSIGN(WORD(I),DR,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:3) .EQ. 'DC=') THEN
			CALL ASSIGN(WORD(I),DC,PARM)
			IF (XERR) RETURN

                ELSE IF (WORD(I)(1:3) .EQ. 'ID=') THEN
                        CALL ASSIGN(WORD(I),TEMP,PARM)
                        IF (XERR) RETURN
                        ID = NINT(TEMP)

		ELSE IF (WORD(I) .EQ. 'OBSNUM') THEN
			OBSNUM = .TRUE.

		ELSE IF (WORD(I) .EQ. 'AUTO') THEN
			AUTO = .TRUE.

                ELSE IF (WORD(I)(1:7) .EQ. 'RSHIFT=') THEN
                        CALL ASSIGN(WORD(I),RSHIFT,PARM)
                        IF (XERR) RETURN
                        HAVERSHIFT = .TRUE.

                ELSE IF (WORD(I)(1:7) .EQ. 'CSHIFT=') THEN
                        CALL ASSIGN(WORD(I),CSHIFT,PARM)
                        IF (XERR) RETURN
                        HAVECSHIFT = .TRUE.



		ELSE IF (WORD(I) .EQ. 'SILENT') THEN
			SILENT = .TRUE.

		ELSE IF (WORD(I) .EQ. 'FORCE') THEN
			NOQUESTION = .TRUE.

		ELSE IF (WORD(I)(1:6) .EQ. 'NITER=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			MAXITER = NINT(TEMP)

		ELSE IF (WORD(I) .EQ. 'MASK') THEN
			MASK = .TRUE.

		ELSE IF (WORD(I) .EQ. 'COMPLETE') THEN
			COMPLETE = .TRUE.

		END IF
8702    CONTINUE

C       Check the range.
	IF (HAVERANGE) THEN
		IF (RANGE(2) .LT. RANGE(1)) THEN
			TEMP     = RANGE(1)
			RANGE(1) = RANGE(2)
			RANGE(2) = TEMP
		END IF
	ELSE
		RANGE(1) =  0.0
		RANGE(2) =  1.0E10
	END IF

C       Check the rejection radius.
	IF (HAVEREJECT) THEN
		IF (REJECT .LE. 0.0) THEN
			PRINT *,'Rejection radius must be > 0.0'
			XERR = .TRUE.
			RETURN
		END IF
	END IF

C       If we have a box get the box parameters.  If the box includes the
C       pixels at the edge of the frame, we must reduce the box size to
C       eliminate these pixels.  If we do not have a box, search the entire
C       image, less the edge pixels.

	IF (HAVEBOX) THEN
		CALL GETBOX(NBOX,ICOORD(1,IM),SR,ER,SC,EC)
		IF (XERR) RETURN

		IF (SR .EQ. ISROW) SR = ISROW + 1
		IF (SC .EQ. ISCOL) SC = ISCOL + 1
		IF (ER .EQ. IEROW) ER = IEROW - 1
		IF (EC .EQ. IECOL) EC = IECOL - 1
	ELSE
		SR = ISROW + 1
		ER = IEROW - 1
		SC = ISCOL + 1
		EC = IECOL - 1
	END IF

C       Note the pixels which will be tested.  We do not check the pixels at
C       the end of the frame.
	IF (SR .GE. ER .OR. SC .GE. EC) THEN
		PRINT *,'The image is not large enough for AUTOMARK.'
		XERR = .TRUE.
		RETURN
	END IF

C       Check the radius.  If there is no radius, set the radius for
C       centroiding to 2 pixels.
	IF (HAVERADIUS) THEN
		IF (RADIUS .LE. 0) THEN
			PRINT *,'Radius must be > 0'
			XERR = .TRUE.
			RETURN
		END IF
	ELSE
		RADIUS = 3.0
	END IF

C       Get the observation parameters for this image, and load them into
C       the common blocks in TIMEDATA.INC.
	CALL CCGETHJD(HEADBUF(1,IM),XERR)
        IF (XERR) RETURN

C       AUTO option
	IF (AUTO) THEN


		IF (NSTARS .EQ. 0) CALL OPENPHOT

C               Initialize variables for computing average coordinate
C               shift.

127             SUMDR = 0.0
		SUMDC = 0.0
		NDELT = 0

C               Make sure there is a photometry file connected to the
C               program.
		IF (NSTARS .EQ. 0) THEN
			PRINT *,'There must be a photometry file ',
     &                          'connected'
			PRINT *,'for the AUTO option.'
			XERR = .TRUE.
			RETURN
		END IF

C               Initialize the array WASFOUND.
		DO 8707 I=1,NSTARS
			WASFOUND(I) = .FALSE.
8707            CONTINUE

C               For each star on the photometry file ...

		DO 8708 K=1, NSTARS

C                       Exit when control-C is typed.
			IF (NOGO) RETURN

C                       Find where the star is supposed to be.
C                       Recall that ROWS and COLS are in common.

			ROWSTART = ROWS(K) + DR
			COLSTART = COLS(K) + DC

			XCAX     = COLSTART
			YCAX     = ROWSTART

			ROW       = NINT(ROWSTART)
			COL       = NINT(COLSTART)

C                       Clear out the record for the star, but do not
C                       touch the star''s name.
			DO 8709 I=1,NUMDATA
				PHOT(I) = 0.0
8709                    CONTINUE

C                       If the star is not too close to the
C                       boundary, look for the star.

			IF (ROW .LT. (SR + RADIUS) .OR.
     &                      ROW .GT. (ER - RADIUS) .OR.
     &                      COL .LT. (SC + RADIUS) .OR.
     &                      COL .GT. (EC - RADIUS)) THEN

				PRINT *,'Star ',K,' is outside ',
     &                      'the image or too close to the edge.'
				GOTO 90
			ELSE
				MAXITER = 6
				OK = FINDCENT(A,ISROW,IEROW,
     &                                   ISCOL,IECOL,
     &                                   XCAX,YCAX,NINT(RADIUS),MAXITER,MASK)
			END IF

C                       If the star could not be found, do not include
C                       it in the new list.

91                      IF (.NOT. OK) GOTO 90

C                       The position of the star has been found.  Check
C                       that the position did not converge to that of another
C                       star on the list.

			IF (ISONLIST(XCAX,YCAX,K,DMIN)) THEN
			   PRINT *,'Position for star ',K
			   PRINT *,'converged to that of ',
     &                                 'another star.'
			   GO TO 90
			END IF

C                       Check that the star did not stray too far from
C                       the original position.

			DELTAR = YCAX - ROWSTART
			DELTAC = XCAX - COLSTART

                        IF (HAVERSHIFT) THEN
                            IF (ABS(DELTAR) .GT. RSHIFT) THEN
                               WRITE(olu,*,ERR=9999)
     &                                   'Star ',K,' can''t be found.'
                               GOTO 90
                            END IF
                        END IF

                        IF (HAVECSHIFT) THEN
                            IF (ABS(DELTAC) .GT. CSHIFT) THEN
                               WRITE(olu,*,ERR=9999)
     &                                   'Star ',K,' can''t be found.'
                               GOTO 90
                            END IF
                        END IF

C                       Masked pixel nearby?
                        IF (MASK) THEN
                          IF (MASKNEAR(XCAX,YCAX,REJECT)) GOTO 90
                        END IF

C                       The star has been found.  Load the information
C                       about it to the current record.

444                     CALL GETPHOTREC(LABEL,PHOT,K)
			IF (XERR) RETURN

			PHOT(ROWLOC)        = YCAX
			PHOT(COLLOC)        = XCAX
			CALL CCFHEAD('EXPOSURE',HEADBUF(1,IM),DVAL)
			PHOT(EXPOSURETIME) = SNGL(DVAL)
			CALL CCFHEAD('FOCUS',HEADBUF(1,IM),DVAL)
			PHOT(FOCUS) = SNGL(DVAL)
			CALL CCFHEAD('GAIN',HEADBUF(1,IM),DVAL)
			PHOT(PHOTGAIN) = SNGL(DVAL)
			IF (DVAL .EQ. 0) PHOT(PHOTGAIN) = 1.
			CALL CCFHEAD('RONOISE',HEADBUF(1,IM),DVAL)
			PHOT(PHOTRN) = SNGL(DVAL)
			PHOT(FRAMERA) = RA
			PHOT(FRAMEDEC) = DEC
			PHOT(HOURANGLE) = HA
			IF (HJD .GT. 0.5) THEN
			  PHOT(JULIAN) = SNGL(HJD - 2444000.D0)
			  PHOT(AIRMASS) = SNGL(AM)
			ELSE
			  PHOT(JULIAN) = 0.
			  PHOT(AIRMASS) = 0.
			END IF
			PHOT(UTMONTH)        = MONTH
			PHOT(UTDAY)          = DAY
			PHOT(UTYEAR)         = YEAR
			PHOT(UTEXPOS)        = UT * 3600.
			IF (OBSNUM) THEN
			 CALL CCINHEAD('OBSNUM',HEADBUF(1,IM),JOBS)
			 IOLD = PHOT(NALTER)
			 IF (IOLD .GT. 1000) IOLD = IOLD/1000
			 PHOT(NALTER)       = IOLD*1000 + MOD(JOBS,1000)
			END IF

			CALL PUTPHOTREC(LABEL,PHOT,K)
			IF (XERR) RETURN

			WASFOUND(K) = .TRUE.

			WRITE(olu,901,ERR=9999) K, DELTAR, DELTAC
 90                     CONTINUE
 901                    FORMAT (' Star ',I6,' DR= ',F9.2,'    DC= ',
     &                            F9.2)

C                       Accumulate information for average shift.

27                      SUMDR  = SUMDR + DELTAR
			SUMDC  = SUMDC + DELTAC
			NDELT  = NDELT + 1
8708            CONTINUE

C               Print average shift.

		IF (NDELT .GT. 0) THEN
			SUMDR = SUMDR / FLOAT(NDELT)
			SUMDC = SUMDC / FLOAT(NDELT)

			WRITE(olu,*,ERR=9999) ' '
			WRITE(olu,902,ERR=9999) DR+SUMDR,DC+SUMDC
 902                    FORMAT(3X,'Average DR = ',F9.2,4X,
     &                            'Average DC = ',F9.2)
			PRINT *, 'Loaded into variables DR, DC '
			CALL VARIABLE('DR',DR+SUMDR,.TRUE.)
			CALL VARIABLE('DC',DC+SUMDC,.TRUE.)
		END IF

C               Now go through the list, compressing the list to include
C               only those stars that were found.

		NFOUND = 0
		DO 8710 I=1,NSTARS
			IF (COMPLETE .OR. WASFOUND(I)) THEN
				NFOUND = NFOUND + 1
				CALL GETPHOTREC(LABEL,PHOT,I)
				IF (XERR) RETURN
				CALL PUTPHOTREC(LABEL,PHOT,NFOUND)
				IF (XERR) RETURN
			END IF
8710            CONTINUE
		IF (NSTARS .NE. NFOUND) THEN
	         PRINT *, 'Missed at least one star for some reason.'
		 PRINT *, 'If completeness is desired, rerun with '
		 PRINT *, 'COMPLETE or check edges. Remember to reread '
		 PRINT *, 'in original photometry file '
		 PRINT *
		END IF
		NSTARS = NFOUND
                CALL VARIABLE('NSTARS',FLOAT(NSTARS),.TRUE.)
		RETURN
	END IF

C       For each pixel that we are finding ...

	IDR = INT(RADIUS)
	IF (.NOT. SILENT) 
     &   WRITE(olu,*,ERR=9999) ' Number     Row        Column'
	DO 8703 J=SR+INT(EDGE),ER-INT(EDGE),1
		DO 8704 I=SC+INT(EDGE),EC-INT(EDGE)

C                       If the pixel is a peak pixel and within the proper
C                       range...

			PIXEL = A(I,J)
			  IF (PIXEL .GE. RANGE(1)   .AND.
     .                      PIXEL .LE. RANGE(2)   .AND.
     .                      PIXEL .GE. A(I-1,J-1) .AND.
     .                      PIXEL .GE. A(I  ,J-1) .AND.
     .                      PIXEL .GE. A(I+1,J-1) .AND.
     .                      PIXEL .GE. A(I-1,J  ) .AND.
     .                      PIXEL .GE. A(I+1,J  ) .AND.
     .                      PIXEL .GE. A(I-1,J+1) .AND.
     .                      PIXEL .GE. A(I  ,J+1) .AND.
     .                      PIXEL .GE. A(I+1,J+1)) THEN

C                           Return if control-C
			    IF (NOGO) THEN
				PRINT *,'Marking stopped by control-C'
				RETURN
			    END IF

C                           Find the centroid here.  OK is .TRUE. if the
C                           centroid could be found.  ISONLIST is .FALSE.
C                           if the star has already been marked.  MASKNEAR
C                           is .TRUE. if there is a masked pixel nearby.
C                       NOTE:  FINDCENT ignores masked pixels, so MASKED
C                           pixels are ignored even without HAVEREJECT.

			    COL = FLOAT(I)
			    ROW = FLOAT(J)
			    NITER = MAXITER
			    OK = FINDCENT(A,ISROW,IEROW,ISCOL,IECOL,
     &                             COL,ROW,IDR,NITER,MASK)

                            IF (HAVEREJECT) PMASKED = MASKNEAR(COL,ROW,REJECT)
                            IF ((OK .OR. NOQUESTION) .AND.
     &                         .NOT. ISONLIST(COL,ROW,0,DMIN) .AND.
     &                         .NOT. (HAVEREJECT .AND. PMASKED)) THEN

			      NSTARS           = NSTARS + 1
			      PHOT(ROWLOC)     = ROW
			      PHOT(COLLOC)     = COL
			      PHOT(FRAMERA)    = RA
			      PHOT(FRAMEDEC)   = DEC
			      PHOT(HOURANGLE)  = HA
			      IF (HJD .GT. 0.5) THEN
			        PHOT(AIRMASS)  = SNGL(AM)
                                PHOT(JULIAN)   = SNGL(HJD - 2444000.D0)
			      ELSE
			        PHOT(AIRMASS)  = 0.
                                PHOT(JULIAN)   = 0.
			      END IF
			      CALL CCFHEAD('EXPOSURE',HEADBUF(1,IM),
     &                               DVAL)
			      PHOT(EXPOSURETIME) = SNGL(DVAL)
			      CALL CCFHEAD('FOCUS',HEADBUF(1,IM),DVAL)
			      PHOT(FOCUS) = SNGL(DVAL)
			      CALL CCFHEAD('GAIN',HEADBUF(1,IM),DVAL)
			      PHOT(PHOTGAIN) = SNGL(DVAL)
			      IF (DVAL .EQ. 0) PHOT(PHOTGAIN) = 1.
			    CALL CCFHEAD('RONOISE',HEADBUF(1,IM),DVAL)
			      PHOT(PHOTRN) = SNGL(DVAL)
			      PHOT(PEAKAD)        = PIXEL
			      PHOT(UTMONTH)   = MONTH
			      PHOT(UTDAY)     = DAY
			      PHOT(UTYEAR)    = YEAR
			      PHOT(UTEXPOS)   = UT * 3600.0
			      JOBS = 0
			      IF (OBSNUM) THEN
C                           CALL CCINHEAD('OBSNUM',HEADBUF(1,IM),MOD(JOBS,1000))
			        CALL CCINHEAD('OBSNUM',HEADBUF(1,IM),JOBS)
                                PHOT(NALTER)    = ID*1000 + MOD(JOBS,1000)
			      ELSE
				PHOT(NALTER) = ID
			      END IF
                              ID = ID + 1

			      CALL PUTPHOTREC(LABEL,PHOT,NSTARS)
			      IF (XERR) RETURN

			      IF (.NOT. SILENT) 
     &			        WRITE(olu,*,ERR=9999) NSTARS,ROW,COL
			   END IF
			END IF
8704            CONTINUE
8703    CONTINUE
#ifdef __DECSTA
	CALL FLUSH(olu)
#endif

	SILENT = .FALSE.
        CALL VARIABLE('NSTARS',FLOAT(NSTARS),.TRUE.)
	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
	SILENT = .FALSE.
	XERR = .TRUE.
	RETURN

	END

C       ---------------------------------------------------------------------

	LOGICAL FUNCTION MASKNEAR(COL,ROW,RADIUS)

C       Returns .TRUE. if there is a masked pixel within RADIUS of the
C       position (COL,ROW).
#ifdef VMS
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
	INCLUDE 'VINCLUDE:MASK.INC'
#else
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/mask.inc'
#endif
	LOGICAL  MASKED

        MASKNEAR = .FALSE.

        IF (LOCMASK .EQ. 0) RETURN

	IMIN = NINT(COL - RADIUS)
	IMAX = NINT(COL + RADIUS)
	JMIN = NINT(ROW - RADIUS)
	JMAX = NINT(ROW + RADIUS)

	IF (IMIN .LT. SCMASK)         IMIN = SCMASK
	IF (JMIN .LT. SRMASK)         JMIN = SRMASK
	IF (IMAX .GT. SCMASK+NCMASK-1) IMAX = SCMASK+NCMASK-1
	IF (JMAX .GT. SRMASK+NRMASK-1) JMAX = SRMASK+NRMASK-1

        RADIUS2 = RADIUS**2
	DO 8705 J=JMIN,JMAX
		DJ2 = ROW - FLOAT(J)
		DO 8706 I=IMIN,IMAX
                  IF (MASKED(I,J)) THEN
			DI2 = COL - FLOAT(I)
			DIS = (DJ2 ** 2 + DI2** 2)
			IF (DIS .LE. RADIUS2) THEN
				MASKNEAR = .TRUE.
				RETURN
			END IF
                  END IF
8706            CONTINUE
8705    CONTINUE

	MASKNEAR = .FALSE.
	RETURN
	END
