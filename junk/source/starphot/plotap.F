#include "Vista.h"
      SUBROUTINE PLOTAP

C   Subroutine to plot results from an aperture photometry file

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif
      PARAMETER (MAXAP = 12, PI=3.14159)

      CHARACTER NAME*132, PARM*8, TABLE*132, LINE*132, LINE2*132
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, SWITCH
      CHARACTER*132 FILE, FILE2, FILE3, LINE3
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE,
     &      FILE, FILE2, FILE3
      LOGICAL NOFILES, KEYCHECK
      COMMON /DAOASK/ NOFILES
      REAL LOWBAD, PAR(30), AMAG(0:MAXAP), RAD(30), SKYRAD(2), ERR(0:30)
      REAL DATA(1000,-1:30), SUM(0:30), ERROR(0:30), STARS(30)
      INTEGER JD(30), IDNST(1000)
      REAL NSTMAG(1000), NSTSKY(1000), NSTERR(1000)
      COMMON /WORK/ DATA, NSTMAG, NSTERR, NSTSKY

      LOGICAL HARD, PORT, OLD, NOERASE, HAVERAD, HAVESKYRAD
      INTEGER VTERM, VHARD, PMGO(FILEPLOT)
      COMMON /VGRAPHICS/ VTERM, VHARD

C Keywords
      CALL KEYINIT
      CALL KEYDEF('ID=')
      CALL KEYDEF('HARD')
      CALL KEYDEF('PORT')
      CALL KEYDEF('OLD')
      CALL KEYDEF('NOERASE')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('RAD=')
      CALL KEYDEF('SKYRAD=')
      CALL KEYDEF('STARS=')
   
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF

      HARD = .FALSE.
      PORT = .FALSE.
      OLD = .FALSE.
      NOERASE = .FALSE.
      HAVERAD = .FALSE.
      HAVESKYRAD = .FALSE.
      IDPLOT = 0
      NSTARS = 0
      DO 8701 I = 1, NCON
        IF (WORD(I)(1:3) .EQ. 'ID=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            IDPLOT = NINT(TEMP)
        ELSE IF (WORD(I) .EQ. 'HARD') THEN
            HARD = .TRUE.
        ELSE IF (WORD(I) .EQ. 'PORT') THEN
            PORT = .TRUE.
        ELSE IF (WORD(I) .EQ. 'OLD') THEN
            OLD = .TRUE.
        ELSE IF (WORD(I) .EQ. 'NOERASE') THEN
            NOERASE = .TRUE.
        ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
            CALL ASSIGN(WORD(I),GAIN,PARM)
            IF (XERR) RETURN
        ELSE IF (WORD(I)(1:3) .EQ. 'RN=') THEN
            CALL ASSIGN(WORD(I),RN,PARM)
            IF (XERR) RETURN
        ELSE IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
            HAVERAD = .TRUE.
            CALL ASSIGNV(WORD(I),MAXAP,RAD,NRAD,PARM)
            IF (XERR) RETURN
        ELSE IF (WORD(I)(1:7) .EQ. 'SKYRAD=') THEN
            HAVESKYRAD = .TRUE.
            CALL ASSIGNV(WORD(I),2,SKYRAD,NSKYRAD,PARM)
            IF (XERR) RETURN
        ELSE IF (WORD(I)(1:6) .EQ. 'STARS=') THEN
            CALL ASSIGNV(WORD(I),30,STARS,NSTARS,PARM)
            IF (XERR) RETURN

        END IF
 8701 CONTINUE

      IF (.NOT. HAVERAD) THEN
      TABLE = 'photo.tab'
      CALL ASKFILE('Enter photometry table name:',TABLE)
      IF (TABLE .EQ. 'END OF FILE') RETURN
      CALL FILEDEF(TABLE,FILE,DAODIR,'.tab')
      CALL PTABLE (PAR, FILE, .TRUE., ISTAT)
      IF (ISTAT .LT. 0) RETURN
C
C Count up the number of apertures that will be used.  The first zero or
C negative number encountered terminates the list.
      NAPER=MAXAP
      APMAXSQ=-1.
      XMIN = 1.e10
      XMAX = -1.E10
      DO 1010 I=1,MAXAP
      IF (PAR(I) .LE. 0.0) GO TO 1020
      XMIN = MIN(XMIN, PAR(I))
      XMAX = MAX(XMAX, PAR(I))
 1010 CONTINUE
      GO TO 1030
 1020 NAPER=I-1
 1030 CONTINUE

      ELSE
        IF (RAD(1).LT.0) THEN
          NRAD = ABS(NINT(RAD(1)))
          SRAD = RAD(2)
          DRAD = RAD(3)
          DO 7702 I=1,NRAD
            RAD(I) = SRAD+(I-1)*DRAD
7702      CONTINUE
        END IF

        NAPER = NRAD
        XMAX=-1.
        XMIN=1.E10
        DO 1110 I=1,NAPER
          PAR(I) = RAD(I)
          XMAX=MAX(XMAX,RAD(I))
          XMIN=MIN(XMIN,RAD(I))
 1110   CONTINUE

      END IF

C Get input aperture file

      CALL ASKFILE('File with aperture results:',FILE)
      IF (FILE .EQ. 'END OF FILE') RETURN
      CALL FILEDEF(FILE,NAME,DAODIR,'.ap')
      CALL INFILE(2, NAME, IFLAG)
      IF (IFLAG .LT. 0) GOTO 9400
      REWIND(2)
      CALL RDHEAD(2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     &      EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)

C Get input profile data for APCOR command
      IF (COM .EQ. 'APCOR') THEN
        CALL ASKFILE('File with profile results:',FILE2)
        IF (FILE .EQ. 'END OF FILE') RETURN
        CALL FILEDEF(FILE2,NAME,DAODIR,'.nst')
        CALL INFILE(3, NAME, IFLAG)
        IF (IFLAG .LT. 0) GOTO 9400
        REWIND(3)
        CALL RDHEAD(3, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     &      EXPOSE, HJD, AP1, PHPADU, READNS, FRAD,LINE3)
        ISTAR = 1
1001    READ(3,'(A)',END=1050) LINE
#ifdef FORTRANCC
        READ(LINE,*) IDNST(ISTAR), X, Y, NSTMAG(ISTAR), NSTERR(ISTAR),
     &               NSTSKY(ISTAR)
#else
        READ(LINE(2:),*) IDNST(ISTAR), X, Y, NSTMAG(ISTAR), NSTERR(ISTAR),
     &               NSTSKY(ISTAR)
#endif
        IF (NSTARS .GT. 0) THEN
          DO 1002 I=1,NSTARS
            IF (IDNST(ISTAR) .EQ. NINT(STARS(I))) GOTO 1003
1002      CONTINUE
          GOTO 1001
        END IF
1003    ISTAR = ISTAR + 1
        GOTO 1001
1050    NST = ISTAR - 1

      END IF

C Open output file for aperture correction data
      IF (COM .EQ. 'APCOR') THEN
        NAME = SWITCH(FILE,'.apdat')
        CALL OUTFILE(7, NAME)
      END IF

C  Initialize accumulators
      DO I=0,12
        SUM(I) = 0
        ERROR(I) = 0.
      END DO
      ISTAR = 0

C Loop over each star, plotting and getting aperture correction for APCOR
5     CONTINUE
      IF (NOGO) GOTO 999
      READ(2,'(A)',END=999) LINE

#ifdef FORTRANCC
      READ(LINE,*) ID, X, Y, (AMAG(I),I=1,NAPER)
#else
      READ(LINE(2:),*) ID, X, Y, (AMAG(I),I=1,NAPER)
#endif
      READ(2,'(A)') LINE2
      READ(LINE2,*) OLDSKY, X, X, (ERR(I),I=1,NAPER)
 
      IF (IDPLOT .NE. 0 .AND. ID .NE. IDPLOT) GOTO 5

      IF (NSTARS .GT. 0) THEN
        DO 5506 I=1,NSTARS
          IF (ID .EQ. NINT(STARS(I))) GOTO 15
5506    CONTINUE
        GOTO 5
      END IF

C  Get profile mag and sky for this star

      IF (COM .EQ. 'APCOR') THEN
        JNST = 0
        DO 5507 I = 1, NST
          IF (ID .EQ. IDNST(I)) JNST = I
5507    CONTINUE
        IF (JNST .EQ. 0) THEN
          PRINT *, 'No profile magnitude found for star: ', ID
          PRINT *, 'Moving to next star'
          GOTO 5
        END IF
        AMAG(0) = NSTMAG(JNST)
        ERR(0) = NSTERR(JNST)
        DSKY = NSTSKY(JNST) - OLDSKY
	PRINT *, 'Aperture sky: ', OLDSKY
	PRINT *, 'Profile sky: ', NSTSKY(JNST)
      END IF
      
15    CONTINUE

      YMIN = 1.E10
      YMAX = -1.E10
      DO 8702 I=1,NAPER
        IF (AMAG(I) .LT. 90) THEN
          YMIN = MIN(YMIN,AMAG(I))
          YMAX = MAX(YMAX,AMAG(I))
        END IF
 8702 CONTINUE

      CALL PLOTSETUP(HARD,PORT,' ',OLD,NOERASE,.FALSE.,XERR)
      IF (XERR) RETURN

      CALL PMGO(SETLIM)(0.,0.,1.,1.)
      CALL PMGO(RELOCATE)(0.8,0.3)
      WRITE(PARM,888) ID
888   FORMAT('ID: ', I4)
      CALL PMGO(PUTLABEL)(8,PARM,6)
      XRANGE = XMAX - XMIN
      YRANGE = YMAX - YMIN
      CALL PMGO(SETLIM)(XMIN-0.05*XRANGE,YMAX+0.05*YRANGE,
     &            XMAX+0.05*XRANGE,YMIN-0.05*YRANGE)
      CALL PMGO(BOX)(1,2)
      CALL PMGO(XLABEL)(24,'Aperture radius (pixels)')
      CALL PMGO(YLABEL)(18,'Aperture magnitude')

      CALL PMGO(POINTS)(43.0,1,PAR,AMAG(1),NAPER)
      CALL PMGO(CONNECT)(PAR,AMAG(1),NAPER)

      IF (COM .EQ. 'APCOR') THEN

	CALL PMGO(TIDLE)
C	PRINT '(1X,''Enter delta sky: ''$)'
C	READ *, DSKY
C       DSKY = 0
        IF (DSKY .NE. 0) THEN
          DO I=1,NAPER
            AREA = PI * RAD(I)**2
            AMAG(I) = 10.**(-0.4*(AMAG(I)-25)) + DSKY*AREA
            IF (AMAG(I) .GT. 0) THEN
              AMAG(I) = -2.5*LOG10(AMAG(I)) + 25
            ELSE
              AMAG(I) = 99.999
            END IF
          END DO
          DSKY = 0
          GOTO 15
        ELSE
151       PRINT '(1X,''Hit +/- to raise/lower sky by 0.1 DN'')'
          PRINT '(1X,''Hit any other key at position of desired mag...'')'
          CALL PMGO(MONGOHAIRS)(ICR,XPOS,YPOS)
          DSKY = 0.
          IF (CHAR(ICR) .EQ. '+' .OR. CHAR(ICR) .EQ. '=') THEN
            DSKY = 0.1
          ELSE IF (CHAR(ICR) .EQ. '-') THEN
            DSKY = -0.1
          END IF
          IF (DSKY .NE. 0) THEN
            DO I=1,NAPER
              AREA = PI * RAD(I)**2
              AMAG(I) = 10.**(-0.4*(AMAG(I)-25)) + DSKY*AREA
              IF (AMAG(I) .GT. 0) THEN
                AMAG(I) = -2.5*LOG10(AMAG(I)) + 25
              ELSE
                AMAG(I) = 99.999
              END IF
            END DO
              YMIN = 1.E10
              YMAX = -1.E10
              DO 9702 I=1,NAPER
                IF (AMAG(I) .LT. 90) THEN
                  YMIN = MIN(YMIN,AMAG(I))
                  YMAX = MAX(YMAX,AMAG(I))
                END IF
 9702         CONTINUE

              CALL PLOTSETUP(HARD,PORT,' ',OLD,NOERASE,.FALSE.,XERR)
              IF (XERR) RETURN

              CALL PMGO(SETLIM)(0.,0.,1.,1.)
              CALL PMGO(RELOCATE)(0.8,0.3)
              CALL PMGO(PUTLABEL)(8,PARM,6)
              CALL PMGO(SETLIM)(XMIN-0.05*XRANGE,YMAX+0.05*YRANGE,
     &            XMAX+0.05*XRANGE,YMIN-0.05*YRANGE)
              CALL PMGO(BOX)(1,2)
              CALL PMGO(XLABEL)(24,'Aperture radius (pixels)')
              CALL PMGO(YLABEL)(18,'Aperture magnitude')

              CALL PMGO(POINTS)(43.0,1,PAR,AMAG(1),NAPER)
              CALL PMGO(CONNECT)(PAR,AMAG(1),NAPER)
              CALL PMGO(TIDLE)
              GOTO 151
          END IF
          ISTAR = ISTAR + 1
          DO J = 0, NAPER
            SUM(J) = SUM(J) + (AMAG(J) - YPOS)/ERR(J)**2
            ERROR(J) = ERROR(J) + 1./ERR(J)**2
            DATA(ISTAR,J) = AMAG(J) - YPOS
          END DO
          DATA(ISTAR,-1) = YPOS
          JD(ISTAR) = ID
        END IF

      END IF

      IF (HARD) THEN
        N = PMGO(FILEPLOT)(0)
        PRINT *, 'Number of vectors plotted: ', N
      ELSE
	CALL PMGO(TIDLE)
      END IF

      IF (IDPLOT .EQ. 0 .AND. COM .NE. 'APCOR') THEN
         PRINT *, 'Hit <CR> for next star'
         READ '(A)', LINE
         GOTO 5
      ELSE IF (IDPLOT .EQ. 0) THEN
         GOTO 5
      END IF


999   CLOSE(2)

C  Output summary of aperture corrections
      IF (COM .EQ. 'APCOR') THEN
        PRINT 76
	WRITE(7,76)
76      FORMAT('Individual star profile/aperture data: ')
        DO I = 1, ISTAR
          PRINT 77, JD(I), (DATA(I,J),J=0,NAPER), DATA(I,-1)
          WRITE(7,77) JD(I), (DATA(I,J),J=0,NAPER), DATA(I,-1)
77	  FORMAT(I6,14F8.3)
        END DO
        PRINT *
        PRINT 78
	WRITE(7,78)
78      FORMAT('Mean corrections for aperture data: ')
        DO I = 1, 12
          IF (ERROR(I) .GT. 0) THEN
            SIG = SQRT(1./ERROR(I))
          ELSE
            SIG = 0.
          END IF
          PRINT *, SUM(I)/ERROR(I), SIG
          WRITE(7,*) SUM(I)/ERROR(I), SIG
        END DO
        PRINT 79
	WRITE(7,79)
79      FORMAT('Mean correction for profile data: ')
        I = 0
        IF (ERROR(I) .GT. 0) THEN
          SIG = SQRT(1./ERROR(I))
        ELSE
          SIG = 0.
        END IF
        PRINT *, SUM(I)/ERROR(I), SIG
        WRITE(7,*) SUM(I)/ERROR(I), SIG

      END IF

      IF (COM .EQ. 'APCOR') CLOSE(7)
      RETURN

9400  PRINT *, 'Error opening file'
      XERR = .TRUE.
      RETURN

      END
