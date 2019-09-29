#include "Vista.h"
	SUBROUTINE SAVE

C*******************************************************************************
C                                                                              
C                         VISTA GET and SAVE COMMANDS                          
C                                                                              
C   This routine allows you to save away the contents of selected data common  
C   blocks in VISTA, and then retrieve them later on.                          
C                                                                              
C   Command Syntax:                                                            
C                                                                              
C       GET   data_keyword=filename                                            
C                                                                              
C       SAVE  data_keyword=filename                                            
C                                                                              
C   where the allowed data_keywords are:                                       
C                                                                              
C       MASK=file               Save or get photometry mask                    
C       PHOT=file               Save or get photometry results                 
C       PROFILE=file            Save or get surface photometry                 
C       APER=file               Save or get aperture photometry                
C       COO=file                Save or get DAOPHOT .COO file                  
C       LINEID=file             Save or get lineid results (.LID)              
C       MULT=file               Save or get multiple galaxy photometry.
C                                                                              
C    By default, the data files will be written to and read in from the data  
C    directory defined in the CUSTOMIZE.INC file.  The default file extensions 
C    are:                                                                      
C                                                                              
C               Photometry              .PHO                                   
C               Mask                    .MSK                                   
C               Surface photometry      .PRF                                   
C               Aperture photometry     .APR                                   
C               Lineid file             .LID                                   
C               Multiple galaxy         .MLT
C                                                                              
C ---------------------------------------------------------------------------- 
C                                                                              
C       Authors:        Tod R. Lauer and Donald Terndrup        4/26/83        
C                       Lick Observatory                                       
C                       University of California                               
C                       Santa Cruz, CA 95064                                   
C                       Version 3                               April, 1985    
C                                                                              
C       Modification History:                                                  
C                                                                              
C               April Atwood    10/86                                          
C               Rick Pogge       3/87   Restored PROFILE= keyword              
C                                                                              
C*******************************************************************************
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'   
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	CHARACTER       PARM*8, TWORD*80
	LOGICAL         KEYCHECK, NEW, APPEND
	LOGICAL         HAVESR, HAVESC
	INTEGER         SR, SC, UPPER

	COMMON /COO/ SR,SC,HAVESR,HAVESC,HIGH,ALOW,RMIN,CMIN,RMAX,CMAX
	LOGICAL HAVESCALE, FITS
	COMMON /PRFSCALE/ HAVESCALE, SCALE, FITS
	DATA HIGH, ALOW /32767., -50./

C   List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('PHOT=')
	CALL KEYDEF('MASK=')
	CALL KEYDEF('PROF=')
	CALL KEYDEF('COL=')
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('APER=')
	CALL KEYDEF('COO=')
	CALL KEYDEF('DAO=')
	CALL KEYDEF('SR=')
	CALL KEYDEF('SC=')
	CALL KEYDEF('HIGH=')
	CALL KEYDEF('LOW=')
        CALL KEYDEF('RMIN=')
        CALL KEYDEF('CMIN=')
        CALL KEYDEF('RMAX=')
        CALL KEYDEF('CMAX=')
	CALL KEYDEF('LINEID=')
	CALL KEYDEF('MULT=')
	CALL KEYDEF('APPEND')
	CALL KEYDEF('FITS')

C   Do we save or get files ?

	IF (COM .EQ. 'GET') THEN
		NEW = .TRUE.
	ELSE
		NEW = .FALSE.
	END IF

	HAVESR = .FALSE.
	HAVESC = .FALSE.
	SR = 0
	SC = 0
        IPRFCOL = 1
	HAVESCALE = .FALSE.
	FITS = .FALSE.
        RMIN = -1E10
        CMIN = -1E10
        RMAX = 1E10
        CMAX = 1E10
	APPEND = .FALSE.
	DO 8701 I=1, NCON
		TWORD = WORD(I)
		L = UPPER(TWORD)
		IF (TWORD(1:3) .EQ. 'SR=' ) THEN
			CALL ASSIGN(TWORD,F,PARM)
			SR = NINT(F)
			HAVESR = .TRUE.
			IF (XERR) RETURN
		ELSE IF (TWORD(1:3) .EQ. 'SC=' ) THEN
			CALL ASSIGN(TWORD,F,PARM)
			SC = NINT(F)
			HAVESC = .TRUE.
			IF (XERR) RETURN
		ELSE IF (TWORD(1:5) .EQ. 'HIGH=') THEN
			CALL ASSIGN(TWORD,HIGH,PARM)
			IF (XERR) RETURN
		ELSE IF (TWORD(1:4) .EQ. 'LOW=') THEN
			CALL ASSIGN(TWORD,ALOW,PARM)
			IF (XERR) RETURN
		ELSE IF (TWORD(1:5) .EQ. 'RMIN=') THEN
			CALL ASSIGN(TWORD,RMIN,PARM)
			IF (XERR) RETURN
		ELSE IF (TWORD(1:5) .EQ. 'CMIN=') THEN
			CALL ASSIGN(TWORD,CMIN,PARM)
			IF (XERR) RETURN
		ELSE IF (TWORD(1:5) .EQ. 'RMAX=') THEN
			CALL ASSIGN(TWORD,RMAX,PARM)
			IF (XERR) RETURN
		ELSE IF (TWORD(1:5) .EQ. 'CMAX=') THEN
			CALL ASSIGN(TWORD,CMAX,PARM)
			IF (XERR) RETURN
		ELSE IF (TWORD(1:4) .EQ. 'COL=') THEN
			CALL ASSIGN(TWORD,F,PARM)
			IF (XERR) RETURN
                        IPRFCOL = NINT(F)
		ELSE IF (TWORD(1:6) .EQ. 'SCALE=') THEN
			CALL ASSIGN(TWORD,SCALE,PARM)
			IF (XERR) RETURN
			HAVESCALE = .TRUE.
		ELSE IF (TWORD .EQ. 'FITS') THEN
			FITS = .TRUE.
                ELSE IF (TWORD .EQ. 'APPEND') THEN
			APPEND = .TRUE.
		END IF
8701    CONTINUE

	DO 8702 I=1, NCON

		TWORD = WORD(I)
		L = UPPER(TWORD)

C       GET or SAVE photometry mask

		IF (TWORD(1:5) .EQ. 'MASK=' ) THEN

			IF (NEW) THEN
				CALL GETMASK(WORD(I)(6: ))
				IF (XERR) RETURN
			ELSE
				CALL SAVEMASK(WORD(I)(6: ))
				IF (XERR) RETURN
			END IF

C       GET or SAVE photometry data

		ELSE IF (TWORD(1:5) .EQ. 'PHOT=' ) THEN

			IF (NEW) THEN
				CALL GETPHOT(WORD(I)(6: ),APPEND)
				IF (XERR) RETURN
			ELSE
				CALL SAVEPHOT(WORD(I)(6: ))
				IF (XERR) RETURN
			END IF

		ELSE IF (TWORD(1:4) .EQ. 'COO=' .OR.
     &                   TWORD(1:4) .EQ. 'DAO=' ) THEN
			IF (NEW) THEN
				CALL GETDAOPHOT(WORD(I)(5: ))
				IF (XERR) RETURN
			ELSE
				CALL SAVEDAOPHOT(WORD(I)(5: ))
				IF (XERR) RETURN
			END IF

C       GET or SAVE surface photometry

		ELSE IF (TWORD(1:5) .EQ. 'PROF=' ) THEN

			IF (NEW) THEN
				CALL GETPROF(WORD(I)(6: ),IPRFCOL)
				IF (XERR) RETURN
			ELSE
				CALL SAVEPROF(WORD(I)(6: ),IPRFCOL)
				IF (XERR) RETURN
			END IF

C       GET or SAVE aperture photometry

		ELSE IF (TWORD(1:5) .EQ. 'APER=' ) THEN

			IF (NEW) THEN
				CALL GETAPER(WORD(I)(6: ))
				IF (XERR) RETURN
			ELSE
				CALL SAVEAPER(WORD(I)(6: ))
				IF (XERR) RETURN
			END IF

C       GET or SAVE lineid data

		ELSE IF (TWORD(1:7) .EQ. 'LINEID=' ) THEN

			IF (NEW) THEN
				CALL GETLINEID(WORD(I)(8: ))
				IF (XERR) RETURN
			ELSE
				CALL SAVELINEID(WORD(I)(8: ))
				IF (XERR) RETURN
			END IF

C       GET or SAVE multiple galaxy photometry data

		ELSE IF (TWORD(1:5) .EQ. 'MULT=') THEN

		        IF (NEW) THEN
				CALL GETMULT(WORD(I)(6:))
				IF (XERR) RETURN
			ELSE
			       CALL SAVEMULT(WORD(I)(6:))
			       IF (XERR) RETURN
			END IF
		END IF

8702    CONTINUE

C   Exit if the user gave a keyword we do not understand.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

	RETURN

	END

C   ----------------------------------------------------------------------

	SUBROUTINE GETPHOT(FILENAME,APPEND)

C   Get photometry record.

	CHARACTER*(*)   FILENAME
	CHARACTER*132    FULLNAME
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Commmunication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Files / directories
	INCLUDE 'VINCLUDE:PHOTFILES.INC'               ! Photometry files
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
C   The components of a photometry record:

	CHARACTER*80    LABEL
	REAL*4          PHOT(NUMDATA)
	LOGICAL 	APPEND

C   Get the filename.

	IF (FILENAME .EQ. ' ') THEN
		PRINT 101
101             FORMAT (' Enter photometry filename: ',$)
		READ 102, FILENAME
102             FORMAT (A)
	END IF

C   In the case where the filename is RECOVER, we will attempt
C   to recover an INTERNAL.PHO file.

	IF (FILENAME .EQ. 'RECOVER') THEN
		CALL GETPHOTRECOVER
		RETURN
	END IF

C   Add directories, etc.

	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.pho')

C   Open the photometry file.

	OPEN (4, FILE=FULLNAME, FORM='UNFORMATTED', STATUS='OLD',
#ifdef __READONLY
     &                  READONLY,
#endif
     &                  IOSTAT=IERR)

	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:132)
		XERR = .TRUE.
		RETURN
	END IF

C   Create a new internal photometry file if not appending
	IF (NSTARS .EQ. 0 .OR. .NOT. APPEND) THEN
	  IF (NSTARS .NE. 0) CALL CLOSEPHOT
	  CALL OPENPHOT
	  NSTARS = 0
	END IF

C   For each record of the photometry file, read the label and the
C   data, and transfer the record to the internal file.

 35     NSTARS = NSTARS + 1
	READ (4,END=40,ERR=37)LABEL,PHOT

	CALL PUTPHOTREC(LABEL,PHOT,NSTARS)
	IF (XERR) THEN
		RETURN
	ELSE
		GO TO 35
	END IF

C   Come here at EOF.

  40    NSTARS = NSTARS - 1
        CALL VARIABLE('NSTARS',FLOAT(NSTARS),.TRUE.)
	CLOSE (4)

C   Return.

	RETURN

C   Come here for read error.

 37     CLOSE(4)
	PRINT *,'Error reading photometry file.'
	NSTARS = 0
	XERR = .TRUE.
	RETURN

	END

C   -----------------------------------------------------------------------

	SUBROUTINE SAVEPHOT(FILENAME)

C   Saves photometry files.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Files / directories
	INCLUDE 'VINCLUDE:PHOTFILES.INC'               ! Photometry files.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
	CHARACTER*(*)   FILENAME
	CHARACTER*132    FULLNAME

C   The components of a photometry file:

	CHARACTER*80    LABEL
	REAL*4          PHOT(NUMDATA)

C   Get the filename.

	IF (FILENAME .EQ. ' ') THEN
		PRINT 101
101             FORMAT (' Enter photometry filename: ',$)
		READ 102, FILENAME
102             FORMAT (A)
	END IF

C   Add directories, etc.

	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.pho')

C   Exit if we do not have a photometry file.

	IF (NSTARS .LE. 0) THEN
		PRINT *,'There is no photometry record that is',
     .                             ' connected to the program.'
		XERR = .TRUE.
		RETURN
	END IF

C   Open the file which will hold the photometry data.

#ifdef VMS
	OPEN (4, FILE=FULLNAME, FORM='UNFORMATTED',STATUS='NEW',
     .   IOSTAT=IERR)
#else
	OPEN (4, FILE=FULLNAME, FORM='UNFORMATTED',
     .   STATUS='UNKNOWN',IOSTAT=IERR)
#endif
	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:132)
		CALL SYSERRPRINT(0,'While doing open')
		XERR = .TRUE.
		RETURN
	END IF

C   Copy the information from the internal photometry file to the
C   designated file.

	DO 8703 I=1,NSTARS
		CALL GETPHOTREC(LABEL,PHOT,I)
		IF (XERR) RETURN

		WRITE (4,ERR=9999) LABEL, PHOT
8703    CONTINUE

	CLOSE (4,IOSTAT=IERR)

	RETURN

C   Error during write - issue message

9999    CALL SYSERRPRINT(0,'WHILE WRITING PHOTOMETRY')
	XERR = .TRUE.
	CLOSE (4,IOSTAT=IERR)
	RETURN

	END

C       -----------------------------------------------------------------------
	SUBROUTINE GETDAOPHOT(FILENAME)

C       Get photmetry record.

	CHARACTER*(*)   FILENAME
	CHARACTER*132    FULLNAME

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
	INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
C       The components of a photometry record:

	CHARACTER*80    LABEL, LINE
        CHARACTER*132   LINE3
	REAL*4          PHOT(NUMDATA)

	LOGICAL HAVESR, HAVESC
	INTEGER SR, SC, IDATA(2)
	COMMON /COO/ SR,SC,HAVESR,HAVESC,HIGH,ALOW,RMIN,CMIN,RMAX,CMAX

C       Get the filename.

	IF (FILENAME .EQ. ' ') THEN
		PRINT 101
101             FORMAT (' Enter photometry filename: ',$)
		READ 102, FILENAME
102             FORMAT (A)
	END IF

C       Add directories, etc.

	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.coo')

C       Open the photometry file.

	OPEN (4, FILE=FULLNAME, STATUS='OLD',
#ifdef __READONLY
     &                  READONLY,
#endif
     &                  IOSTAT=IERR)

	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:132)
		XERR = .TRUE.
		RETURN
	END IF

C       Create a new internal photometry file.

	IF (NSTARS .NE. 0) CALL CLOSEPHOT
	CALL OPENPHOT
	NSTARS = 0

C       For each record of the photometry file, read the label and the
C       data, and transfer the record to the internal file.

	IF (.NOT. HAVESR .OR. .NOT. HAVESC) THEN
	  CALL ASKINT(
     &     'Enter amount to add to DAOPHOT row and col posns.:',IDATA,2)
	  ROW1 = IDATA(1)
	  COL1 = IDATA(2)
	ELSE
	 ROW1 = SR
	 COL1 = SC
	END IF

C       Read header info.

	AIR = 0.
	EXP = 0.
	HJD = 0.
	AP1 = 0.
	GAIN = 1.
	RONOISE = 0.
	FRAD = 0.
	CALL RDHEAD(4,NL,NX,NY,ALOW,HIGH,THRESHT,AIR,EXP,HJD,AP1,GAIN,
     &         RONOISE,FRAD,LINE3)
C	READ(4,'(A80)') LINE
C	IF (INDEX(LINE,'AIR') .GT. 0) THEN
C	  READ(4,*) NL,NX,NY,ALOW,HIGH,THRESH,AIR,EXP,HJD,AP1,GAIN,
C     &        RONOISE,FRAD
C	ELSE IF (INDEX(LINE,'FRAD') .GT. 0) THEN
C	  READ(4,*) NL,NX,NY,ALOW,HIGH,THRESH,AP1,GAIN,
C     &        RONOISE,FRAD
C	  AIR = 0.
C	  EXP = 0.
C	  HJD = 0.
C	ELSE
C	  READ(4,*) NL,NX,NY,ALOW,HIGH,THRESH
C	END IF
C	READ(4,*)

 35     NSTARS = NSTARS + 1
 36     READ (4,'(A)',END=40,ERR=37) LINE
	IF (LINE .EQ. ' ') GOTO 36
#ifdef __SUN
	READ(LINE(1:)//CHAR(4),*,ERR=36) I,COL,ROW
#else
	READ(LINE(1:),*,ERR=36) I,COL,ROW
#endif
C 36     FORMAT(1X,I5,2F9.2)

	PHOT(ROWLOC) = ROW + ROW1
	PHOT(COLLOC) = COL + COL1

        IF (PHOT(ROWLOC) .LT. RMIN .OR. PHOT(ROWLOC) .GT. RMAX .OR.
     &      PHOT(COLLOC) .LT. CMIN .OR. PHOT(COLLOC) .GT. CMAX) GOTO 36

	PHOT(NALTER) = I
	PHOT(PHOTGAIN) = GAIN
	PHOT(PHOTRN) = RONOISE
	PHOT(PHOTAPER) = AP1
	PHOT(PHOTFRAD) = FRAD
	PHOT(AIRMASS) = AIR
	PHOT(EXPOSURETIME) = EXP
	PHOT(JULIAN) = HJD

	CALL PUTPHOTREC(LABEL,PHOT,NSTARS)
	IF (XERR) THEN
		RETURN
	ELSE
		GO TO 35
	END IF

C       Come here at EOF.

  40    NSTARS = NSTARS - 1
        CALL VARIABLE('NSTARS',FLOAT(NSTARS),.TRUE.)
	CLOSE (4)

C       Return.

	RETURN

C       Come here for read error.

 37     CLOSE(4)
	PRINT *,'Error reading photometry file.'
	NSTARS = 0
	XERR = .TRUE.
	RETURN

	END

	SUBROUTINE SAVEDAOPHOT(FILENAME)

C       Saves photometry files.

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
	INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
	CHARACTER*(*)   FILENAME
	CHARACTER*132    FULLNAME

C       The components of a photometry file:

	CHARACTER*80    LABEL
	REAL*4          PHOT(NUMDATA)

	LOGICAL HAVESR, HAVESC
	INTEGER SR, SC, IDATA(2)
	COMMON /COO/ SR,SC,HAVESR,HAVESC,HIGH,ALOW,RMIN,CMIN,RMAX,CMAX

C       Get the filename.

	IF (FILENAME .EQ. ' ') THEN
		PRINT 101
101             FORMAT (' Enter photometry filename: ',$)
		READ 102, FILENAME
102             FORMAT (A)
	END IF

C       Add directories, etc.

	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.coo')

C       Exit if we do not have a photometry file.

	IF (NSTARS .LE. 0) THEN
		PRINT *,'There is no photometry record that is',
     .                             ' connected to the program.'
		XERR = .TRUE.
		RETURN
	END IF

C       Open the file which will hold the photometry data.

#ifdef VMS
	OPEN (4, FILE=FULLNAME, STATUS='NEW',IOSTAT=IERR)
#else
	OPEN (4, FILE=FULLNAME, STATUS='UNKNOWN', IOSTAT=IERR)
#endif
	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:132)
		CALL SYSERRPRINT(0,'While doing open')
		XERR = .TRUE.
		RETURN
	END IF

C       Copy the information from the internal photometry file to the
C       designated file.

	IF (.NOT. HAVESR .OR. .NOT. HAVESC) THEN
	  CALL ASKINT(
     &     'Enter amount to add to DAOPHOT row and col posns.:',IDATA,2)
	  ROW1 = IDATA(1)
	  COL1 = IDATA(2)
	ELSE
	 ROW1 = SR
	 COL1 = SC
	END IF

c       Write header info.

	CALL GETPHOTREC(LABEL,PHOT,1)
	GAIN = PHOT(PHOTGAIN)
	RONOISE = PHOT(PHOTRN)
	AP1 = PHOT(PHOTAPER)
	FRAD = PHOT(PHOTFRAD)
	AIR = PHOT(AIRMASS)
	AIR = PHOT(AIRMASS)
	EXP = PHOT(EXPOSURETIME)
	HJD = PHOT(JULIAN)
	IF (HJD .GT. 2444000) HJD = HJD - 2444000

	WRITE (4,5)
5       FORMAT(' NL  NX  NY  LOWBAD HIGHBAD  THRESH     AIR   ITIME',
     &         '     HJD     AP1  PH/ADU  RNOISE    FRAD')
	WRITE (4,6) 1, 800, 800, ALOW, HIGH, 0., AIR,EXP,HJD,AP1,
     &         GAIN,RONOISE,FRAD
6       FORMAT(I3, 2I4, 3F8.1, 3F8.2, F8.1, 2F8.2, F8.1)
	WRITE (4,*)

	DO 8705 I=1,NSTARS
		CALL GETPHOTREC(LABEL,PHOT,I)
		IF (XERR) RETURN

		IF (PHOT(TOTALAD) .GT. 0.) THEN
		  AMAG = 25 - 1.085736205*ALOG(PHOT(TOTALAD))
		ELSE IF (PHOT(ARTCOUNT) .GT. 0.) THEN
		  AMAG = 25 - 1.085736205*ALOG(PHOT(ARTCOUNT))
		ELSE
		  AMAG = 0.
		END IF
		ERR =  1.085736205*(PHOT(ERRSCALERATIO))
		SKY = PHOT(BACKND)
		AITER = 0
		SHARP = 1.
		CHI = 0.
		ID = I
		IF (PHOT(NALTER) .NE. 0) ID = PHOT(NALTER)
		IF (ID .LT. 0) GOTO 8705
		WRITE (4,15,ERR=9999) ID,PHOT(COLLOC)-COL1,
     &            PHOT(ROWLOC)-ROW1,AMAG,ERR,SKY,AITER,CHI,SHARP
  15            FORMAT (I6, 2F9.2, 3F9.3, F9.0, F9.2, F9.3)
8705    CONTINUE

	CLOSE (4,IOSTAT=IERR)

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'WHILE WRITING PHOTOMETRY')
	XERR = .TRUE.
	CLOSE (4,IOSTAT=IERR)
	RETURN

	END


C   -----------------------------------------------------------------------

	SUBROUTINE GETPHOTRECOVER

C   Recovers a file INTERNAL.PHO, connecting it to the program.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif

	CHARACTER*80    LABEL
	REAL*4          DATA(NUMDATA)

	IF (NSTARS .NE. 0) THEN
		PRINT *,'There already is a photometry file connected.'
		RETURN
	END IF

C   Open the file.  The file is always in the current directory.
	CALL OPENPHOT

	I = 1
 100    CONTINUE
	CALL GETPHOTREC(LABEL,DATA,I)
	IF (XERR) THEN
		PRINT *,'Number of records : ',I - 1
		XERR = .FALSE.
		NSTARS = I - 1
		RETURN
	END IF

	I = I + 1
	GO TO 100

	END

C   -----------------------------------------------------------------------

	SUBROUTINE GETMASK(FILENAME)

C   Get mask file.

	CHARACTER*(*)   FILENAME
	CHARACTER*80    FULLNAME
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:IMAGELINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Files / directories
	INCLUDE 'VINCLUDE:MASK.INC'                    ! Mask file
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/mask.inc'
#endif
	INTEGER CLOSEC, NR, NC, SR, SC

C   Get the filename.

	IF (FILENAME .EQ. ' ') THEN
		PRINT 103
103             FORMAT (' Enter mask filename: ',$)
		READ 104, FILENAME
104             FORMAT(A)
	END IF

C   Add on directories, etc.
	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.msk')

C   See if we have new FITS style mask file
        IFILE = -1
        CALL RDFITSHEAD(TEMPHEAD,FULLNAME,IFILE,IERR,.TRUE.)

        IF (IERR .EQ. 0) THEN
          PRINT *, 'MASK file appears to be FITS....'
C         Allocate memory for MASK
          NR= INHEAD('NAXIS2',TEMPHEAD)
          NC= INHEAD('NAXIS1',TEMPHEAD)
          SR= INHEAD('CRVAL2',TEMPHEAD)
          SC= INHEAD('CRVAL1',TEMPHEAD)
          CALL NEWMASK(NR,NC,SR,SC)

          CALL CCLOADMASK(IFILE,LOCMASK,MASKSIZE,0)
          ISTAT = CLOSEC(IFILE)
          RETURN
        ELSE
          ISTAT = CLOSEC(IFILE)
	  PRINT *, 'Reading old style mask'
C   Open the file.
	  OPEN (4, FILE=FULLNAME, FORM='UNFORMATTED', STATUS='OLD',
#ifdef __READONLY
     &           READONLY,
#endif
     &           IOSTAT=IERR)
	  IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:55)
		XERR = .TRUE.
		RETURN
	  END IF

C   Read the number of words in the mask buffer.
	  READ (4,ERR=150,END=150) MASKSIZE
          NRMASK=2048
          NCMASK=2048
          SRMASK=0
          SCMASK=0

C   Allocate memory
          IF (LOCMASK .NE. 0) CALL CCFREE(MASKSIZE*4,LOCMASK)
          CALL CCALLOC(MASKSIZE*4,LOCMASK)
          CALL CCZEROMASK(LOCMASK,MASKSIZE)
          CALL CCLOADMASK(4,LOCMASK,MASKSIZE,1)
#ifdef OLD
	  IF (NWORDS .GT. MASKSIZE) THEN
		CLOSE (4)
		XERR = .TRUE.
		PRINT *,'There are ',NWORDS,' in the mask.  The'
		PRINT *,'most that you can fit is ',MASKSIZE
		RETURN
	  END IF

C   If everything is OK, clean out the mask buffer.

	  DO 8706 I=1,MASKSIZE
		PIXMASK(I) = 0
8706      CONTINUE

C   Read in the mask buffer so that the last entry in the file
C   fits into the last entry in the array.
	  READ(4,ERR=150,END=150)(PIXMASK(J),J=MASKSIZE-NWORDS+1,
     &        MASKSIZE)
#endif
	  CLOSE (4)
        END IF

	RETURN

C   Come here when read error.

 150    PRINT *,'Read error: cannot fetch mask'
	XERR = .TRUE.
	CLOSE(4)
	RETURN

	END

C   -----------------------------------------------------------------------

	SUBROUTINE SAVEMASK(FILENAME)

C   Get mask file.

	CHARACTER*(*)   FILENAME
	CHARACTER*80    FULLNAME
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:IMAGELINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Files / directories
	INCLUDE 'VINCLUDE:MASK.INC'                    ! Mask file
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/mask.inc'
#endif	
	INTEGER WRITEINT, OPENC, CLOSEC

C   Get the filename.

	IF (FILENAME .EQ. ' ') THEN
		PRINT 103
103             FORMAT (' Enter mask filename: ',$)
		READ 104, FILENAME
104             FORMAT(A)
	END IF

C   Add on directories, etc.

	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.msk')

#define FITSMASK
#ifdef FITSMASK
        IF (NRMASK*NCMASK/32 .NE. MASKSIZE) THEN
          PRINT *, 'ERROR: you cannot save old-style mask in new-style format'
          PRINT *, '  Convert your mask to new-style first but making image'
          PRINT *, '   with MASKTOIM, then using UNMASK and CLIP to make new mask'
	  PRINT *, MASKSIZE, NRMASK, NCMASK
          XERR =.TRUE.
          RETURN
        END IF
        L = NUMCHAR(FULLNAME)
        FULLNAME(L+1:L+1) = CHAR(0)
        IERR = OPENC(IFILE,FULLNAME,1)
        IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:L)
		CALL SYSERRPRINT(0,'WHILE DOING OPEN')
		XERR = .TRUE.
		RETURN
	END IF
        TEMPHEAD(1:80) = 'SIMPLE  =                    T'
        TEMPHEAD(81: ) = 'END '
        CALL INHEADSET('BITPIX',1,TEMPHEAD)
        CALL INHEADSET('NAXIS',2,TEMPHEAD)
        CALL INHEADSET('NAXIS1',NCMASK,TEMPHEAD)
        CALL INHEADSET('NAXIS2',NRMASK,TEMPHEAD)
        CALL INHEADSET('CRVAL1',SCMASK,TEMPHEAD)
        CALL INHEADSET('CRVAL2',SRMASK,TEMPHEAD)
        CALL INHEADSET('CDELT1',1,TEMPHEAD)
        CALL INHEADSET('CDELT2',1,TEMPHEAD)
        CALL CHEADSET('OBJECT','PIXEL MASK',TEMPHEAD)

	IERR = WRITEINT(IFILE,TEMPHEAD,2880)
	IERR = CCWRITEMASK(IFILE,LOCMASK,4*MASKSIZE)
	IERR = CLOSEC(IFILE)
#else
C   Open the file.
#ifdef VMS
	OPEN (4, FILE=FULLNAME, FORM='UNFORMATTED', STATUS='NEW',
     .           IOSTAT=IERR)
#else
	OPEN (4, FILE=FULLNAME, FORM='UNFORMATTED',
     .           STATUS='UNKNOWN', IOSTAT=IERR)
#endif
	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:55)
		CALL SYSERRPRINT(0,'WHILE DOING OPEN')
		XERR = .TRUE.
		RETURN
	END IF
	WRITE(4,ERR=9999) MASKSIZE
	WRITE(4,ERR=9999) PIXMASK
	CLOSE(4)
#endif
	RETURN

C   Error during write - issue message

9999    CALL SYSERRPRINT(0,'WHILE WRITING MASK')
	XERR = .TRUE.
	RETURN

	END

C   -------------------------------------------------------------------

	SUBROUTINE SAVEAPER(APERNAME)

	CHARACTER*(*) APERNAME

C   Save aperture photometry results
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Files and directories
	INCLUDE 'VINCLUDE:APERINC.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/aperinc.inc'
#endif
	CHARACTER*80    FILENAME

	IF (APERNAME .EQ. ' ') THEN
		PRINT 104
104             FORMAT (' Enter aperture photometry filename: ',$)
		READ 106, APERNAME
106             FORMAT(A)
	END IF

	CALL FILEDEF(APERNAME,FILENAME,DATADIR,'.apr')

#ifdef VMS
	OPEN (4,FILE=FILENAME,FORM='UNFORMATTED',STATUS='NEW',ERR=51)
#else
	OPEN (4,FILE=FILENAME,FORM='UNFORMATTED',
     &     STATUS='UNKNOWN',ERR=51)
#endif

	WRITE (4,ERR=9999) APHEAD, NAP, XAP, YAP, SCALEAP
	DO 8707 I=1, NAP
		WRITE (4,ERR=9999) (APER(J,I),J=1,8)
8707    CONTINUE

	CLOSE(4)
	RETURN

 51     XERR = .TRUE.
	PRINT *,'Can not open file: ',FILENAME
	CALL SYSERRPRINT(0,'While doing OPEN')
	RETURN

C   Error during write - issue message

9999    CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
	XERR = .TRUE.
	RETURN

	END

C   -------------------------------------------------------------------

	SUBROUTINE GETAPER(APERNAME)

	CHARACTER*(*) APERNAME

C   Get aperture photometry results
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Files and directories
	INCLUDE 'VINCLUDE:APERINC.INC'         ! Photometry.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/aperinc.inc'
#endif
	CHARACTER*80    FILENAME

	IF (APERNAME .EQ. ' ') THEN
		PRINT 104
104             FORMAT (' Enter aperture photometry filename: ',$)
		READ 106, APERNAME
106             FORMAT(A)
	END IF

	CALL FILEDEF(APERNAME,FILENAME,DATADIR,'.apr')

	OPEN (4,FILE=FILENAME,FORM='UNFORMATTED',STATUS='OLD',
#ifdef __READONLY
     &                  READONLY,
#endif
     &                  ERR=51)

	READ (4,ERR=37), APHEAD, NAP, XAP, YAP, SCALEAP
	DO 8708 I=1, NAP
		READ (4,ERR=37) (APER(J,I),J=1,8)
8708    CONTINUE

	CLOSE (4)

	RETURN

51      XERR = .TRUE.
C                               ! File error
	PRINT *,'Cannot open ',FILENAME
	CALL SYSERRPRINT(0,'While doing OPEN')
	RETURN

37      XERR = .TRUE.
C                               ! Read error
	PRINT *,'Read error in ',FILENAME
	CALL SYSERRPRINT(0,'Error while reading')
	CLOSE(4)
	RETURN

	END

C   -------------------------------------------------------------------

	SUBROUTINE SAVEPROF(PROFNAME,IC)

	CHARACTER*(*) PROFNAME

C   Save surface photometry (PROFILE) results
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Files and directories
	INCLUDE 'VINCLUDE:PROFILE.INC'         ! Profile parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'
	INCLUDE 'vistadisk/source/include/work.inc'
#endif
	CHARACTER*80    FILENAME
#ifndef MSBFirst
        INTEGER OPENC, CLOSEC, WRITEINT, PACKFIT4, RECL, RECW
        INTEGER JUNK(WORKSIZE)
        COMMON /WORK/ JUNK
#endif
	LOGICAL HAVESCALE, FITS
	COMMON /PRFSCALE/ HAVESCALE, SCALE, FITS

	IF (HAVESCALE) PRFSC(IC) = SCALE

	IF (PROFNAME .EQ. ' ') THEN
		PRINT '(1x,''Surface photometry filename ? : '',$)'
		READ '(A)', PROFNAME
	END IF

	CALL FILEDEF(PROFNAME,FILENAME,DATADIR,'.prf')

	IF (FITS) THEN
          CALL FHEADSET('PRFSC',DBLE(PRFSC(IC)),PRFHEAD(IC))
          CALL INHEADSET('NPRF',NPRF(IC),PRFHEAD(IC))
          CALL INHEADSET('NAXIS',2,PRFHEAD(IC))
          CALL INHEADSET('NAXIS1',NPRFPARAM,PRFHEAD(IC))
          CALL INHEADSET('NAXIS2',NPRF(IC),PRFHEAD(IC))
          CALL FHEADSET('CRVAL1',1.d0,PRFHEAD(IC))
          CALL FHEADSET('CRVAL2',1.d0,PRFHEAD(IC))
          CALL FHEADSET('CDELT1',1.d0,PRFHEAD(IC))
          CALL FHEADSET('CDELT2',DBLE(PRFSC(IC)),PRFHEAD(IC))
          CALL INHEADSET('CRPIX1',1,PRFHEAD(IC))
          CALL INHEADSET('CRPIX2',1,PRFHEAD(IC))

          CALL INHEADSET('BITPIX',-32,PRFHEAD(IC))

          CALL WRFITS(PRFHEAD(IC),PARAMPRF(1,1,IC),NPRF(IC),NPRFPARAM,
     &         FILENAME,-32,0.d0,1.d0, .FALSE., 0, 0., IERR)
 
          IF (IERR .NE. 0) THEN
             PRINT *, 'Error writing FITS profile'
             XERR = .TRUE.
             RETURN
          END IF
          RETURN

        END IF

#ifndef MSBFirst
        L = NUMCHAR(FILENAME)
        FILENAME(L+1:L+1) = CHAR(0)
        ISTAT = OPENC(IFILE,FILENAME(1:L+1),1)
C  Save the header
        RECL = LEN(PRFHEAD(IC))
        RECW = RECL
        ISTAT = PACKFIT4(RECL,RECL,4)
        ISTAT = WRITEINT(IFILE,RECL,4)
        ISTAT = WRITEINT(IFILE,PRFHEAD(IC),RECW)
        ISTAT = WRITEINT(IFILE,RECL,4)

C  SAVE NPRF and PRFSC (all in one record)
        RECL = 8
        ISTAT = PACKFIT4(RECL,RECL,4)
        ISTAT = WRITEINT(IFILE,RECL,4)
        ISTAT = PACKFIT4(NPRF(IC),NPRF(IC),4)
        ISTAT = WRITEINT(IFILE,NPRF(IC),4)
        ISTAT = PACKFIT4(NPRF(IC),NPRF(IC),4)
        ISTAT = PACKFIT4(PRFSC(IC),PRFSC(IC),4)
        ISTAT = WRITEINT(IFILE,PRFSC(IC),4)
        ISTAT = PACKFIT4(PRFSC(IC),PRFSC(IC),4)
        ISTAT = WRITEINT(IFILE,RECL,4)

        RECL = 4*NPRFPARAM
        RECW = RECL
        ISTAT = PACKFIT4(RECL,RECL,4)
	DO 9710 I = 1, NPRF(IC)
          ISTAT = WRITEINT(IFILE,RECL,4)
          ISTAT = PACKFIT4(PARAMPRF(1,I,IC),PARAMPRF(1,I,IC),RECW)
          ISTAT = WRITEINT(IFILE,PARAMPRF(1,I,IC),RECW)
          ISTAT = PACKFIT4(PARAMPRF(1,I,IC),PARAMPRF(1,I,IC),RECW)
          ISTAT = WRITEINT(IFILE,RECL,4)
9710    CONTINUE


        ISTAT = CLOSEC(IFILE)
#else
#ifdef VMS
	OPEN (4,FILE=FILENAME,FORM='UNFORMATTED',STATUS='NEW',ERR=51)
#else
	OPEN (4,FILE=FILENAME,FORM='UNFORMATTED',
     &     STATUS='UNKNOWN',ERR=51)
#endif
	WRITE(4,ERR=9999) PRFHEAD(IC)
	WRITE (4,ERR=9999) NPRF(IC), PRFSC(IC)

	DO 8709 I = 1, NPRF(IC)
		WRITE (4,ERR=9999) (PARAMPRF(J,I,IC),J=1,21)
8709    CONTINUE
	CLOSE(4)
#endif
	RETURN

C   File OPENing error

51      XERR = .TRUE.
	PRINT *,'Can not open file: ',FILENAME
	CALL SYSERRPRINT(0,'While doing OPEN')
	RETURN

C   Error during write - issue message

9999    CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
	XERR = .TRUE.
	RETURN

	END

C   -------------------------------------------------------------------

	SUBROUTINE GETPROF(PROFNAME,IC)

	CHARACTER*(*) PROFNAME

C   Retrieve surface photometry (PROFILE) results
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Files and directories
	INCLUDE 'VINCLUDE:PROFILE.INC'         ! Profile parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'
	INCLUDE 'vistadisk/source/include/work.inc'
#endif
	CHARACTER    FILENAME*80, OLDHEAD*5760
        INTEGER CLOSEC
#ifndef MSBFirst
        INTEGER OPENC, READINT, PACKFIT4, RECL
        INTEGER JUNK(WORKSIZE)
        COMMON /WORK/ JUNK
#endif
        REAL*8 FHEAD

	IF (PROFNAME .EQ. ' ') THEN
		PRINT '(1x,''Surface photometry filename ? : '',$)'
		READ '(A)', PROFNAME
	END IF

	CALL FILEDEF(PROFNAME,FILENAME,DATADIR,'.prf')

        IFILE = -1
	CALL RDFITSHEAD(PRFHEAD(IC),FILENAME,IFILE,IERR,.TRUE.)

        IF (IERR .EQ. 0) THEN
 	  PRINT *, 'PROFILE file appears to be FITS....'
          NPRF(IC) = INHEAD('NAXIS2',PRFHEAD(IC))
          PRFSC(IC) = SNGL(FHEAD('CDELT2',PRFHEAD(IC)))
          IF (INHEAD('NAXIS1',PRFHEAD(IC)) .NE. NPRFPARAM) THEN
            PRINT *, 'Error: this does not appear to be a PROFILE FITS file!'
            XERR = .TRUE.
            ISTAT = CLOSEC(IFILE)
            RETURN
          END IF
          CALL RDFITS(PARAMPRF(1,1,IC),NPRF(IC),NPRFPARAM,IFILE,PRFHEAD(IC),IERR)
          IF (IERR .NE. 0) THEN
            PRINT *, 'Error reading PROFILE FITS file!'
            XERR = .TRUE.
            ISTAT = CLOSEC(IFILE)
          END IF
          RETURN

        ELSE
          ISTAT = CLOSEC(IFILE)
        END IF

#ifndef MSBFirst
        L = NUMCHAR(FILENAME)
        FILENAME(L+1:L+1) = CHAR(0)
        ISTAT = OPENC(IFILE,FILENAME(1:L+1),0)
C  Get the header
        ISTAT = READINT(IFILE,RECL,4)
        ISTAT = PACKFIT4(RECL,RECL,4)
        ISTAT = READINT(IFILE,PRFHEAD(IC),MIN(28800,RECL))
        RECL = RECL - MIN(28800,RECL)
511     IF (RECL .GT. 0) THEN
          ISTAT = READINT(IFILE,JUNK,MIN(WORKSIZE,RECL))
          RECL = RECL - MIN(WORKSIZE,RECL)
          GOTO 511
        END IF
        ISTAT = READINT(IFILE,RECL,4)

C  Get NPRF and PRFSC (all in one record)
        ISTAT = READINT(IFILE,RECL,4)
        ISTAT = PACKFIT4(RECL,RECL,4)
        IF (RECL .NE. 8) GOTO 37
        ISTAT = READINT(IFILE,NPRF(IC),4)
        ISTAT = PACKFIT4(NPRF(IC),NPRF(IC),4)
        ISTAT = READINT(IFILE,PRFSC(IC),4)
        ISTAT = PACKFIT4(PRFSC(IC),PRFSC(IC),4)
        ISTAT = READINT(IFILE,RECL,4)

	DO 9710 I = 1, NPRF(IC)
          ISTAT = READINT(IFILE,RECL,4)
          ISTAT = PACKFIT4(RECL,RECL,4)
          ISTAT = READINT(IFILE,PARAMPRF(1,I,IC),RECL)
          ISTAT = PACKFIT4(PARAMPRF(1,I,IC),PARAMPRF(1,I,IC),RECL)
          ISTAT = READINT(IFILE,RECL,4)
9710    CONTINUE

        ISTAT = CLOSEC(IFILE)
#else

	OPEN (4,FILE=FILENAME,FORM='UNFORMATTED',STATUS='OLD',
#ifdef __READONLY
     &                  READONLY,
#endif
     &                  ERR=51)

	PRFHEAD(IC) = ' '
	READ(4,ERR=31) PRFHEAD(IC)
	GOTO 32
31      CONTINUE
	REWIND(4)
	READ(4,ERR=37) OLDHEAD
	CALL VSTRCPY(PRFHEAD(IC),OLDHEAD)

32	CONTINUE
        READ (4,ERR=37) NPRF(IC), PRFSC(IC)
C    Sense whether this is an old or new PRF file (18 or 21 entries
C         per record)
        IREC = 21
        READ(4,ERR=36) (PARAMPRF(J,1,IC),J=1,IREC)
        GOTO 38
36      CONTINUE
	REWIND(4)
	READ(4,ERR=41) PRFHEAD(IC)
	GOTO 42
41	CONTINUE
	REWIND(4)
	READ(4,ERR=37) OLDHEAD
	CALL VSTRCPY(PRFHEAD(IC),OLDHEAD)

42	READ (4,ERR=37) NPRF(IC), PRFSC(IC)
        IREC = 18
        READ(4,ERR=36) (PARAMPRF(J,1,IC),J=1,IREC)

38	DO 8710 I = 2, NPRF(IC)
		READ (4,ERR=36) (PARAMPRF(J,I,IC),J=1,IREC)
8710    CONTINUE

	CLOSE (4)
#endif

C   Load various PROFILE variables
        SKY = SNGL(FHEAD('SKY',PRFHEAD(IC)))
        CALL VARIABLE('SKY     ',SKY,.TRUE.)
        CALL VARIABLE('PROFX0',PARAMPRF(2,1,IC),.TRUE.)
        CALL VARIABLE('PROFY0',PARAMPRF(1,1,IC),.TRUE.)
        CALL VARIABLE('PRFSC',PRFSC(IC),.TRUE.)
	RETURN

C   File OPENing Error

51      XERR = .TRUE.
	PRINT *,'Cannot open ',FILENAME
	CALL SYSERRPRINT(0,'While doing OPEN')
	RETURN

C   File READing error

37      XERR = .TRUE.
	PRINT *,'Read error in ',FILENAME
	CALL SYSERRPRINT(0,'Error while reading')
	CLOSE(4)
	RETURN

	END
C   -----------------------------------------------------------------------

	SUBROUTINE SAVELINEID(FILENAME)

	CHARACTER*(*) FILENAME
	CHARACTER*80  FULLNAME
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Files / directories
	INCLUDE 'VINCLUDE:WAVECAL.INC'                 ! Wave file
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/wavecal.inc'
#endif
	COMMON / RANGE/ COEF(5), IFIT

C   Get the filename.
	IF (FILENAME .EQ. ' ') THEN
		PRINT 103
103             FORMAT (' Enter lineid filename: ',$)
		READ 104, FILENAME
104             FORMAT(A)
	END IF

C   Add on directories, etc.
	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.lid')

C   Open the file which will hold the lineid data.

#ifdef VMS
	OPEN (4, FILE=FULLNAME, STATUS='NEW',IOSTAT=IERR)
#else
	OPEN (4, FILE=FULLNAME, STATUS='UNKNOWN',IOSTAT=IERR)
#endif
	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:80)
		CALL SYSERRPRINT(0,'While doing open')
		XERR = .TRUE.
		RETURN
	END IF

C   Write the SAVELAMBDA and RANGE commons
	WRITE(4,*) '  LINEID FILE: list of identified spectral lines.'
	WRITE(4,*) SAVEDSP, ' Approx. dispersion (A/px).'
	WRITE(4,*) COEF(1),COEF(2),
     &             ' Dispersion and Curvature from .wav file.'
	WRITE(4,*) NUMSAVED,' Spectral lines identified.'
	WRITE(4,*) '     PIXEL       WAVELENGTH      WEIGHT',
     &             '     ORDER  BLEND'
	DO 8710 I=1,NUMSAVED
	    WRITE(4,112,ERR=150) SAVECENTER(I), SAVEWAVE(I),
     &           SAVEWEIGHT(I), SAVEM(I), SAVEN(I)
8710    CONTINUE
112     FORMAT(3(1PE14.6),2I6)
	CLOSE (4)
	RETURN

C   Come here when WRITE error.
  150   CALL SYSERRPRINT(0,'ERROR While saving lineid.')
	PRINT *,'Write error: could not save lineid parameters.'
	XERR = .TRUE.
	CLOSE(4)
	RETURN
	END

C   -----------------------------------------------------------------------

	SUBROUTINE GETLINEID(FILENAME)

	CHARACTER*(*) FILENAME
	CHARACTER*80  FULLNAME
	CHARACTER*1   COMMENT
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Files / directories
	INCLUDE 'VINCLUDE:WAVECAL.INC'                 ! Wave file
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/wavecal.inc'
#endif
	COMMON / RANGE/ COEF(5), IFIT

C   Get the filename.
	IF (FILENAME .EQ. ' ') THEN
		PRINT 103
103             FORMAT (' Enter lineid filename: ',$)
		READ 104, FILENAME
104             FORMAT(A)
	END IF

C   Add on directories, etc.
	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.lid')

C   Open the file.
	OPEN (4, FILE=FULLNAME, STATUS='OLD',
#ifdef __READONLY
     &           READONLY,
#endif
     &           IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:80)
		XERR = .TRUE.
		RETURN
	END IF

C   Read in the SAVELAMBDA and RANGE commons
	READ(4,'(A1)',END=150) COMMENT
	READ(4,*,ERR=150,END=150) SAVEDSP
	READ(4,*,ERR=150,END=150) COEF(1), COEF(2)
	READ(4,*,ERR=150,END=150) NUMSAVED
	READ(4,'(A1)',ERR=150,END=150) COMMENT
	NUMSAVED = MIN0(NUMSAVED,NSAVES)
	DO 8710 I=1,NUMSAVED
	    READ(4,*,ERR=150,END=140) SAVECENTER(I), SAVEWAVE(I),
     &           SAVEWEIGHT(I), SAVEM(I), SAVEN(I)
8710    CONTINUE

	CLOSE (4)
	RETURN

C   Come here when END-OF-FILE error.
140     NUMSAVED = I-1
	PRINT*,'WARNIG: read only the first ',NUMSAVED,' lines.'
	CLOSE (4)
	RETURN

C   Come here when read error.
 150    PRINT *,'Read error: cannot fetch lineid parameters'
	XERR = .TRUE.
	CLOSE(4)
	RETURN
	END
C   -----------------------------------------------------------------------

	SUBROUTINE SAVEMULT(FILENAME)

	CHARACTER*(*) FILENAME
	CHARACTER*80  FULLNAME
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Files / directories
	INCLUDE 'VINCLUDE:MULT.INC'                 ! Wave file
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/mult.inc'
#endif

C   Get the filename.
	IF (FILENAME .EQ. ' ') THEN
		PRINT 103
103             FORMAT (' Enter multiple galaxy profile filename: ',$)
		READ 104, FILENAME
104             FORMAT(A)
	END IF

C   Add on directories, etc.
	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.mlt')

C   Open the file which will hold the data.

#ifdef VMS
	OPEN (4, FILE=FULLNAME, STATUS='NEW', FORM='UNFORMATTED',
     &        IOSTAT=IERR)
#else
	OPEN (4, FILE=FULLNAME, STATUS='UNKNOWN', FORM='UNFORMATTED',
     &        IOSTAT=IERR)
#endif
	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:80)
		CALL SYSERRPRINT(0,'While doing open')
		XERR = .TRUE.
		RETURN
	END IF

	WRITE(4,ERR=150) HEADMULT, MXISO, MXGAL, NG, SCALEMLT
	WRITE(4,ERR=150) (RISO(I),I=1,MXISO)
	WRITE(4,ERR=150) (CR(I),I=1, MXGAL)
	WRITE(4,ERR=150) (CC(I),I=1, MXGAL)
	WRITE(4,ERR=150) ((SURF(K,J),K=1,5),J=1,MXISO)
	WRITE(4,ERR=150) ((SURSIG(K,J),K=1,5),J=1,MXISO)

	CLOSE (4)
	RETURN

C   Come here when WRITE error.
  150   CALL SYSERRPRINT(0,'ERROR While saving mult.')
	PRINT *,'Write error: could not save MULT parameters.'
	XERR = .TRUE.
	CLOSE(4)
	RETURN
	END

C   -----------------------------------------------------------------------

	SUBROUTINE GETMULT(FILENAME)

	CHARACTER*(*) FILENAME
	CHARACTER*80  FULLNAME
	INTEGER TOP(10)
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Files / directories
	INCLUDE 'VINCLUDE:MULT.INC'                 ! Wave file
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/mult.inc'
#endif

C   Get the filename.
	IF (FILENAME .EQ. ' ') THEN
		PRINT 103
103             FORMAT (' Enter multiple galaxy profile filename: ',$)
		READ 104, FILENAME
104             FORMAT(A)
	END IF

C   Add on directories, etc.
	CALL FILEDEF(FILENAME,FULLNAME,DATADIR,'.mlt')

C   Open the file.
	OPEN (4, FILE=FULLNAME, STATUS='OLD', FORM='UNFORMATTED',
#ifdef __READONLY
     &           READONLY,
#endif
     &           IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open ',FULLNAME(1:80)
		XERR = .TRUE.
		RETURN
	END IF

	READ(4) HEADMULT, MXISO, MXGAL, NG, SCALEMLT
	READ(4) (RISO(I),I=1,MXISO)
        IF (MXISO .GT. 15) THEN
	      READ(4) (CR(I),I=1,MXGAL)
	      READ(4) (CC(I),I=1,MXGAL)
	      READ(4) ((SURF(K,J),K=1,5),J=1,MXISO)
	      READ(4) ((SURSIG(K,J),K=1,5),J=1,MXISO)
	ELSE
	      READ(4) (CR(I),I=1,MXISO)
	      READ(4) (CC(I),I=1,MXISO)
	      DO 2784 I = 1, NG
	        IF (I .EQ. 1) THEN
	  	  JS = 0
	        ELSE
	    	  JS = TOP(I-1)
	        END IF
	        READ(4) ((SURF(K,J+JS),K=1,5),J=1,MXISO)
	        TOP(I) = JS
	        DO 2788 J=1,MXISO
		  IF (SURF(1,J+JS) .NE. 0.0) THEN
		   TOP(I) = TOP(I) + 1
		   IF (J .EQ. MXISO) THEN
		     TOP(I) = TOP(I) + 1
		     SURF(1,J+1+JS) = 0.0
		   END IF
		  ELSE
		   TOP(I) = TOP(I) + 1
		   GOTO 2789
		  END IF
2788            CONTINUE
2789            CONTINUE
2784          CONTINUE

	      DO 2786 I = 1, NG
		IF (I .EQ. 1) THEN
		  JS = 0
		ELSE
		  JS = TOP(I-1)
		END IF

		READ(4,END=2787) ((SURSIG(K,J+JS),K=1,5),J=1,MXISO)
2786          CONTINUE
2787          CONTINUE
	      MXISO = MISO
	END IF
	      
	CLOSE (4)
	RETURN

	END

        SUBROUTINE LOADMASK(IUNIT,MASK,NWORDS)

	INTEGER*4 MASK(NWORDS)

	READ(IUNIT) (MASK(J),J=1,NWORDS)

	RETURN
	END
