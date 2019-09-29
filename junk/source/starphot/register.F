#include "Vista.h"

#define __MAXPAR 20
      SUBROUTINE REGISTER

C  Subroutine to determine plate transformation coefficients between
C     two or more frames given coordinates of at least three stars
C     in each frame

      PARAMETER (MAXSTARS = 1000, MAXFRAME = 30, MAXPAR=__MAXPAR)
      CHARACTER FILE*64,NAME*80,REFFILE*64,DATFILE*64, PARM*8, RESFILE*64
      INTEGER ID1(MAXSTARS), ID2(MAXSTARS), UPPER
      INTEGER ID1T(MAXSTARS), ID2T(MAXSTARS)
      REAL*8 X2T(MAXSTARS), Y2T(MAXSTARS), X1T(MAXSTARS), Y1T(MAXSTARS)
      REAL*8 XPOS(MAXSTARS,MAXFRAME), YPOS(MAXSTARS,MAXFRAME), FHEAD
      REAL*8 C(MAXPAR,MAXPAR), Z(MAXPAR), V(MAXPAR), D(MAXPAR,2)
      REAL*8 SUMX, SUMY, SUMXY, SUMX2, SUMY2
      REAL*8 XSQ,YSQ,XY, XDX, YDX, XDY, YDY, X, Y, DX, DY, XNEW, YNEW
      REAL*8 SCALE, THETA, PAR(MAXPAR,2)
      LOGICAL REJECT(MAXSTARS), HAVERES
      LOGICAL KEYCHECK, HEADER, LOAD, MEAN, ROT, TRANS, DAOPHOT, CUBIC
      LOGICAL HAVEREF, HAVEDAT, HAVEDPOS, SILENT, VRMS, VROT, VSCALE, VDX, VDY
      CHARACTER FITSCARD*8, LINE*80, CRMS*8, CROT*8, CSCALE*8, CDX*8, CDY*8
      CHARACTER*6 TERM(10)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif
      COMMON /WORK/ XPOS, YPOS

      CALL KEYINIT
      CALL KEYDEF('LOAD')
      CALL KEYDEF('HEADER')
      CALL KEYDEF('MEAN')
      CALL KEYDEF('ROT')
      CALL KEYDEF('TRANS')
      CALL KEYDEF('CUBIC')
      CALL KEYDEF('REF=')
      CALL KEYDEF('DAT=')
      CALL KEYDEF('RES=')
      CALL KEYDEF('DPOS=')
      CALL KEYDEF('NITER=')
      CALL KEYDEF('MAXRMS=')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('RMS=')
      CALL KEYDEF('ROT=')
      CALL KEYDEF('SCALE=')
      CALL KEYDEF('DX=')
      CALL KEYDEF('DY=')

C 
C  Find out how many frames were given, if any

      NIM = 0
      INDX = 1
 8701 IF (INDX .LE. NCON .AND. IBUF(INDX) .NE. 0) THEN
        NIM = NIM + 1
        INDX = INDX + 1
        GOTO 8701
      END IF

C  Get keywords

      HEADER = .FALSE.
      LOAD = .FALSE.
      MEAN = .FALSE.
      ROT = .FALSE.
      CUBIC = .FALSE.
      TRANS = .FALSE.
      HAVEDPOS = .FALSE.
      HAVERES = .FALSE.
      VRMS = .FALSE.
      VROT = .FALSE.
      VSCALE = .FALSE.
      VDX = .FALSE.
      VDY = .FALSE.
      SILENT = .FALSE.
      NITER = 1	
      RMSMAX = -1
      DO 8702 I = 1, NCON
         LINE = WORD(I)
         L = UPPER(LINE)
         IF (LINE .EQ. 'LOAD') THEN
            LOAD = .TRUE.
         ELSE IF (LINE .EQ. 'HEADER') THEN
            HEADER = .TRUE.
         ELSE IF (LINE .EQ. 'MEAN') THEN
            MEAN = .TRUE.
         ELSE IF (LINE .EQ. 'ROT') THEN
            ROT = .TRUE.
         ELSE IF (LINE .EQ. 'CUBIC') THEN
            CUBIC = .TRUE.
         ELSE IF (LINE .EQ. 'TRANS') THEN
            TRANS = .TRUE.
         ELSE IF (LINE .EQ. 'SILENT') THEN
            SILENT = .TRUE.
         ELSE IF (LINE(1:5) .EQ. 'DPOS=') THEN
            HAVEDPOS = .TRUE.
            REFFILE = WORD(I)(6:)
         ELSE IF (LINE(1:4) .EQ. 'REF=') THEN
            HAVEREF = .TRUE.
            REFFILE = WORD(I)(5:)
         ELSE IF (LINE(1:4) .EQ. 'DAT=') THEN
            HAVEDAT = .TRUE.
            DATFILE = WORD(I)(5:)
         ELSE IF (LINE(1:4) .EQ. 'RES=') THEN
            HAVERES = .TRUE.
            RESFILE = WORD(I)(5:)
         ELSE IF (LINE(1:4) .EQ. 'RMS=') THEN
            VRMS = .TRUE.
            CRMS = LINE(5:)
         ELSE IF (LINE(1:4) .EQ. 'ROT=') THEN
            VROT = .TRUE.
            CROT = LINE(5:)
         ELSE IF (LINE(1:6) .EQ. 'SCALE=') THEN
            VSCALE = .TRUE.
            CSCALE = LINE(7:)
         ELSE IF (LINE(1:3) .EQ. 'DX=') THEN
            VDX = .TRUE.
            CDX = LINE(4:)
         ELSE IF (LINE(1:3) .EQ. 'DY=') THEN
            VDY = .TRUE.
            CDY = LINE(4:)
	 ELSE IF (LINE(1:6) .EQ. 'NITER=') THEN
            CALL ASSIGN(LINE,TMP,PARM)
            IF (XERR) RETURN
            NITER = NINT(TMP)
	 ELSE IF (LINE(1:7) .EQ. 'MAXRMS=') THEN
            CALL ASSIGN(LINE,RMSMAX,PARM)
            IF (XERR) RETURN
         END IF
 8702 CONTINUE

      IF (.NOT.KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF

      IF ((VDX .OR. VDY .OR. VROT .OR. VSCALE) .AND. .NOT. ROT) THEN
        PRINT *, 
     &    'ERROR: DX=, DY=, ROT=, SCALE= not applicable without ROT option'
        XERR = .TRUE.
        RETURN
      END IF

      IF (HEADER .AND. NIM .LT. 1) THEN
         PRINT *, ' You must specify some image numbers for the ',
     &            'HEADER option'
         XERR = .TRUE.
         RETURN
      END IF

      IF (NIM .GT. 2 .AND. .NOT. HEADER .AND. .NOT. HAVEDPOS) THEN
         PRINT *, ' To do more than 2 frames at a time, you must ',
     &    'use the header or dpos option, or just do pairs of 2 at a time'
         XERR = .TRUE.
         RETURN
      END IF
      IF (LOAD .AND. NIM .LT. 1) THEN
         PRINT *, ' You must specify some image numbers for the ',
     &            'LOAD option'
         XERR = .TRUE.
         RETURN
      END IF
	
      IF (NIM .GT. MAXFRAME) THEN
         PRINT *, 'Error, only ', MAXFRAME, ' frames allowed ...'
         XERR = .TRUE.
         RETURN
      END IF

      IF (HAVERES) THEN
        CALL FILEDEF(RESFILE,NAME,DAODIR,'.res')
        OPEN(7,FILE=NAME,STATUS='UNKNOWN',IOSTAT=IERR)
        IF (IERR .NE. 0) GOTO 999
      END IF

C  Get file names, open and read files, if we don''t have the HEADER option

      IF (HEADER) THEN

C  read in the reference positions from the HEADER cards
C         RSTARX1, RSTARX2 ....RSTARX(NRSTAR)
C         RSTARY1, RSTARY2 ....RSTARY(NRSTAR)
C
        DO 8703 IMG = 1, NIM
          TEMPHEAD = ' '
          CALL CCVSTRCPY(TEMPHEAD, HEADBUF(1,IBUF(IMG))) 
          NTOT = INHEAD('NRSTAR  ',TEMPHEAD)
          IF (NTOT .EQ. 0) THEN
            PRINT *, 'Error reading NRSTAR card for image: ',IBUF(IMG)
            XERR = .TRUE.
            RETURN
          END IF
          IF (NTOT .GT. 9) THEN
            PRINT *, ' A maximum of 9 reference stars are allowed '
            PRINT *, ' Continuing using the first nine '
            NTOT = 9
          END IF
          IF (IMG .EQ. 1) THEN
            NSAVE = NTOT
          ELSE IF (NTOT .NE. NSAVE) THEN
            PRINT *, ' The number of reference stars must be the same '
            XERR = .TRUE.
            RETURN
          END IF
          IF (NTOT .LE. 2 .AND. .NOT. TRANS) THEN
            PRINT *, ' There must be at least 3 reference stars '
            XERR = .TRUE.
            RETURN
          ELSE
            DO 8704 I = 1, NTOT
              FITSCARD(1:8) = 'RSTARX  '
              WRITE(FITSCARD(7:7),'(I1)') I
              XPOS(I,IMG) = FHEAD(FITSCARD,TEMPHEAD)
              IF (XPOS(I,IMG) .EQ. 0.) THEN
                PRINT *, 
     &            'Error reading RSTARX card from image: ',IBUF(IMG)
                XERR = .TRUE.
                RETURN
              END IF
              FITSCARD(6:6) = 'Y'
              YPOS(I,IMG) = FHEAD(FITSCARD,TEMPHEAD)
              IF (YPOS(I,IMG) .EQ. 0.) THEN
                PRINT *, 
     &          'Error reading RSTARY card from image: ',IBUF(IMG)
                XERR = .TRUE.
                RETURN
              END IF
 8704       CONTINUE
          END IF

 8703   CONTINUE

      ELSE

C     We will read coordinates from file(s)
        IF (HAVEDPOS .OR. HAVEREF) THEN
          CALL FILEDEF(REFFILE,NAME,DAODIR,'.dat')
        ELSE
          PRINT *, ' We will read files from the default data directory '
          PRINT *, '         with default extension .dat '
          PRINT *
          PRINT '(1x,''Enter name of first file with coordinates: ''$)'
          READ '(A)', FILE
          CALL FILEDEF(FILE,NAME,DAODIR,'.dat')
        END IF
        OPEN(1,FILE=NAME,STATUS='OLD',IOSTAT=IERR)
        IF (IERR .NE. 0) GOTO 999

        IF (HAVEDPOS) THEN

          READ(1,*) NIM
          I = 1
8713      READ(1,*,END=8714) X1T(I), Y1T(I), (XPOS(I,J), YPOS(I,J), J=1,NIM)
C      DPOS files store position differences, so add these to first position
C        to get pairs of positions
          XPOS(I,1) = X1T(I)
          YPOS(I,1) = Y1T(I)
          DO 8799 J=2,NIM
            XPOS(I,J) = X1T(I) + XPOS(I,J)
            YPOS(I,J) = Y1T(I) + YPOS(I,J)
 8799     CONTINUE
          ID2(I) = 0
          I = I + 1
          IF (I .GT. MAXSTARS) THEN
             PRINT *, 'Only ', MAXSTARS,' stars allowed '
             GOTO 1999
          END IF
          GOTO 8713
 8714     NTOT = I - 1
	  print *, 'got stars: ', Ntot
          CLOSE(1)

        ELSE 
C         Read first file
          I = 1
          DAOPHOT = .FALSE.
5         READ(1,'(A)',END=15) LINE
          IF (INDEX(LINE,'NL') .GT. 0) THEN
            DAOPHOT = .TRUE.
            READ(1,'(A)',END=15) LINE 
            READ(1,'(A)',END=15) LINE 
            READ(1,'(A)',END=15) LINE 
          END IF

          IF (DAOPHOT) THEN
            READ(LINE,301,ERR=998) ID1T(I), X1T(I), Y1T(I)
          ELSE
            ID1(I) = I
            READ(LINE,*,ERR=998) XPOS(I,1), YPOS(I,1)
          END IF
          I = I + 1
          IF (I .GT. MAXSTARS) THEN
            PRINT *, ' Currently only ',MAXSTARS, ' stars can be used '
            GOTO 1999
          END IF
          GOTO 5
15        N1 = I - 1
          CLOSE(1)

C         Read second file
          IF (HAVEDAT) THEN
            CALL FILEDEF(DATFILE,NAME,DAODIR,'.dat')
          ELSE
            PRINT '(1x,''Enter name of second file with coordinates: ''$)'
            READ '(A)', FILE
            CALL FILEDEF(FILE,NAME,DAODIR,'.dat')
          END IF
          OPEN(2,FILE=NAME,STATUS='OLD',IOSTAT=IERR)
          IF (IERR .NE. 0) GOTO 999

          I = 1
          DAOPHOT = .FALSE.
25        READ(2,'(A)',END=35) LINE
          IF (INDEX(LINE,'NL') .GT. 0) THEN
            DAOPHOT = .TRUE.
            READ(2,'(A)',END=35) LINE 
            READ(2,'(A)',END=35) LINE 
            READ(2,'(A)',END=35) LINE 
          END IF

          IF (DAOPHOT) THEN
            READ(LINE,301,ERR=998) ID2T(I), X2T(I), Y2T(I)
301         FORMAT(I6,2F9.2)
          ELSE
            ID2(I) = I
            READ(LINE,*,ERR=998) XPOS(I,2), YPOS(I,2)
          END IF
          I = I + 1
          IF (I .GT. MAXSTARS) THEN
            PRINT *, ' Currently only ',MAXSTARS, ' stars can be used '
            GOTO 1999
          END IF
          GOTO 25
35        N2 = I - 1
          CLOSE(2)

C       Check same number of stars in both files if we dont have DAOPHOT IDs
          IF (.NOT. DAOPHOT .AND. N1 .NE. N2) THEN
             PRINT *,' The number of stars in both files must be the same'
             XERR = .TRUE.
             RETURN
          END IF

C       If we have a DAOPHOT file, match up corresponding ID numbers
          IF (DAOPHOT) THEN
            NTOT = 0
            DO 6601 I=1, N1
              DO 6602 J=1, N2
               IF (ID2T(J) .EQ. ID1T(I)) THEN
                NTOT = NTOT + 1
                ID1(NTOT) = ID1T(I)
                XPOS(NTOT,1) = X1T(I)
                YPOS(NTOT,1) = Y1T(I)
                ID2(NTOT) = ID2T(J)
                XPOS(NTOT,2) = X2T(J)
                YPOS(NTOT,2) = Y2T(J)
               END IF
6602          CONTINUE
6601        CONTINUE
          ELSE
            NTOT = N2
          END IF

        END IF

      END IF

C  With the MEAN option, compute shift relative to 1st position shifted to
C    have mean position (0,0)
      IF (MEAN) THEN
        SUMX = 0.
        SUMY = 0.
        DO 8721 I = 1, NTOT
          SUMX = SUMX + XPOS(I,1)
          SUMY = SUMY + YPOS(I,1)
 8721   CONTINUE
        DO 8722 I = 1, NTOT
          XPOS(I,1) = XPOS(I,1) - SUMX/NTOT
          YPOS(I,1) = YPOS(I,1) - SUMY/NTOT
 8722   CONTINUE
      END IF

      IF (NIM .EQ. 0) NIM = 2

C  With LOAD option, load up unit solution for first image
      IF (LOAD) THEN
          IF (.NOT. BUFF(IBUF(1))) THEN
            PRINT *, 'No image in buffer: ', IBUF(1)
            GOTO 1999
          END IF
          TEMPHEAD = ' '
          CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IBUF(1)))
          DO J=1,10
            WRITE(FITSCARD,111) J
111         FORMAT('X(',i2.2,',1)') 
            CALL FHEADSET(FITSCARD,0.d0,TEMPHEAD)
            WRITE(FITSCARD,112) J
112         FORMAT('X(',i2.2,',2)') 
            CALL FHEADSET(FITSCARD,0.d0,TEMPHEAD)
          END DO
	  IF (MEAN) THEN
            CALL FHEADSET('X(1,1)',-SUMX/NTOT,TEMPHEAD)
            CALL FHEADSET('X(1,2)',-SUMY/NTOT,TEMPHEAD)
	  END IF
          CALL CCVSTRADD(HEADBUF(1,IBUF(1)),TEMPHEAD)
      END IF

C  Loop over each separate image and do a solution relative to first image

      DO 8705 IMG = 2, NIM

       PRINT *, '*****************************'
       PRINT *, 'Image: ', IMG
       SUMX2 = 0.
       DO I=1,MAXPAR
         Z(I) = 0.
       END DO
       DO I=1,MAXPAR/2
         PAR(I,1) = 0.
         PAR(I,2) = 0.
       END DO

C  For DPOS option, need to reverse coordinates to get proper direction of
C    transformation
       IF (HAVEDPOS) THEN
         DO 9705 I = 1, NTOT
           TMP = XPOS(I,IMG)
           XPOS(I,IMG) = XPOS(I,1)
           XPOS(I,1) = TMP
           TMP = YPOS(I,IMG)
           YPOS(I,IMG) = YPOS(I,1)
           YPOS(I,1) = TMP
9705	 CONTINUE
       END IF

C  We will allow for multiple iterations with 3-sigma rejection 
       DO 8706 ITER = 1, NITER

	 IF (ITER .GT. 1 .AND. RMSMAX .GT. 0 .AND. SUMX2 .LT. RMSMAX) GOTO 8706

C        Load up the matrices
         NPTS = 0
C       initialize derivatives
  	 DO J = 1, MAXPAR
           DO L=1,MAXPAR
             C(L,J) = 0.
           END DO
           V(J) = 0.
         END DO

C       Loop over each star
         DO 8707 I = 1, NTOT

	  X = XPOS(I,IMG)
	  Y = YPOS(I,IMG)
          DX = XPOS(I,1)-XPOS(I,IMG)
          DY = YPOS(I,1)-YPOS(I,IMG)

C      initialize derivatives
	  DO J = 1, MAXPAR
            D(J,1) = 0.
            D(J,2) = 0.
          END DO

          REJECT(I) = .FALSE.
	  IF ((ABS(DX) .GT. 9999. .AND. ABS(DX) .LT. 10000.) .OR. 
     &        (ABS(DY) .GT. 9999. .AND. ABS(DY) .LT. 10000.) ) THEN
            REJECT(I) = .TRUE.
            GOTO 8707
          END IF
C      3-sigma rejection if past first iteration
          IF (ITER .GT. 1) THEN
            XNEW = XPOS(I,IMG)
            YNEW = YPOS(I,IMG)
C         Load up general cubic terms (coeffs in PAR will be zero if necessary)
            CALL GETXFUNC(XNEW,YNEW,D,MAXPAR,NPAR,.FALSE.,.FALSE.,.TRUE.)
            DO 9701 J=1,MAXPAR/2
              XNEW = XNEW + PAR(J,1)*D(J,1)
              YNEW = YNEW + PAR(J,2)*D(J+NPAR/2,2)
9701        CONTINUE
            R = ( (XNEW-XPOS(I,1))**2 + (YNEW-YPOS(I,1))**2 )
            IF (R .GT. 0) R = SQRT(R)

            IF (SUMX2 .GT. 0. .AND.  R .GT. 3*SUMX2) THEN
              REJECT(I) = .TRUE.
              GOTO 8707
            END IF
          END IF

          NPTS = NPTS + 1

C      load up derivatives for this particular sort of 
C         solution (rot, cubic, etc)
          CALL GETXFUNC(X,Y,D,MAXPAR,NPAR,TRANS,ROT,CUBIC)

C       Load up matrices
	  DO L=1,NPAR
            DO J=L,NPAR
              DO K=1,2
                C(J,L) = C(J,L) + D(L,K)*D(J,K)
              END DO
            END DO
            V(L) = V(L) + DX*D(L,1)
            V(L) = V(L) + DY*D(L,2)
          END DO

8707    CONTINUE

C      Call GETXFUNC to set NPAR appropriately (in case last star was 
C        rejected )
          CALL GETXFUNC(X,Y,D,MAXPAR,NPAR,TRANS,ROT,CUBIC)

C   Reflect matrix across diagonal 
        DO 5501 I = 1, NPAR
          DO 5502 J = 1,I-1
            C(J,I) = C(I,J)
5502      CONTINUE
5501    CONTINUE

C   Invert matrix
        CALL DINVERS(C, MAXPAR, NPAR, IERR)
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error inverting matrix ', NPTS, MAXPAR, NPAR, ROT, TRANS, CUBIC
	  DO I=1,NPAR
            PRINT *, I, C(I,I)
          END DO
          GOTO 1999
        END IF

        CALL DVMUL(C, MAXPAR, NPAR, V, Z)

C   Load parameters back into general cubic formalism
        DO 9909 I=1,MAXPAR
          PAR(I,1) = 0.
          PAR(I,2) = 0.
 9909   CONTINUE
        IF (TRANS) THEN
          PAR(1,1) = Z(1)
          PAR(1,2) = Z(2)
 
        ELSE IF (ROT) THEN
          PAR(1,1) = Z(1)
          PAR(2,1) = Z(2)
          PAR(3,1) = Z(3)
          PAR(1,2) = Z(4)
          PAR(2,2) = -Z(3)
          PAR(3,2) = Z(2)

        ELSE IF (CUBIC) THEN
          DO I=1,10
            PAR(I,1) = Z(I)
            PAR(I,2) = Z(I+10)
          END DO
 
        ELSE
          PAR(1,1) = Z(1)
          PAR(2,1) = Z(2)
          PAR(3,1) = Z(3)
          PAR(1,2) = Z(4)
          PAR(2,2) = Z(5)
          PAR(3,2) = Z(6)
        END IF

	IF (.NOT. SILENT .OR. .NOT. LOAD) THEN
          PRINT *
          PRINT *, 'The transformation coefficients are: '
	  PRINT *, '        TERM          X                     Y'
          TERM(1) = 'CONST'
          TERM(2) = 'X'
          TERM(3) = 'Y'
          TERM(4) = 'X**2'
          TERM(5) = 'X*Y'
          TERM(6) = 'Y**2'
          TERM(7) = 'X**3'
          TERM(8) = 'X**2*Y'
          TERM(9) = 'X*Y**2'
          TERM(10) = 'Y**3'
          DO J=1,10
            PRINT *, J, PAR(J,1), PAR(J,2), '   ', TERM(J)
          END DO
        END IF

	IF (ROT) THEN
           SCALE = SQRT((PAR(2,1)+1)**2 + PAR(3,1)**2 )
C           THETA = ACOS((1+PAR(2,1))/SCALE)*180./3.14159265
           THETA = ATAN2(PAR(3,1)/SCALE,(1+PAR(2,1))/SCALE)*180./3.14159265
           DX = PAR(1,1)
           DY = PAR(1,2)
	   PRINT *
           PRINT 1007, SCALE, THETA
1007	   FORMAT('  SCALE:  ', F10.3,  '  ROTATION:  ', F10.3)
	END IF

        IF (LOAD) THEN
          IF (.NOT. BUFF(IBUF(IMG))) THEN
            PRINT *, 'No image in buffer: ', IBUF(IMG)
            GOTO 1999
          END IF
          TEMPHEAD = ' '
          CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IBUF(IMG)))
          DO J=1,10
            WRITE(FITSCARD,111) J
            CALL FHEADSET(FITSCARD,PAR(J,1),TEMPHEAD)
            WRITE(FITSCARD,112) J
            CALL FHEADSET(FITSCARD,PAR(J,2),TEMPHEAD)
          END DO
          CALL CCVSTRADD(HEADBUF(1,IBUF(IMG)),TEMPHEAD)
        END IF

        IF (.NOT. SILENT) THEN
          PRINT *
          PRINT *, ' Residuals from frame: ', IMG
        END IF
        SUMX = 0
	SUMX2 = 0
        DO 8708 I = 1, NTOT

          X = XPOS(I,IMG)
          Y = YPOS(I,IMG)
          CALL GETXFUNC(X,Y,D,MAXPAR,NPAR,.FALSE.,.FALSE.,.TRUE.)

          XNEW = X
          YNEW = Y
          DO J=1,MAXPAR/2
            XNEW = XNEW + PAR(J,1)*D(J,1)
            YNEW = YNEW + PAR(J,2)*D(J+NPAR/2,2)
          END DO
          IF (ID2(I) .NE. 0) THEN
              J = ID2(I)
          ELSE
              J = I
          END IF
          IF (.NOT. SILENT .AND. .NOT. REJECT(I)) 
     &       PRINT 7, J, XPOS(I,IMG), YPOS(I,IMG), 
     &        XNEW-XPOS(I,1), YNEW-YPOS(I,1), XNEW, YNEW, XPOS(I,1), YPOS(I,1)
7	  FORMAT(I6,2(0PF12.4),2(1PG12.4),4(0PF12.4))

C      Calculate RMS
          IF (.NOT. REJECT(I)) THEN
              X = ( (XNEW-XPOS(I,1))**2 + (YNEW-YPOS(I,1))**2 )
              IF (X .GT. 0) X = SQRT(X)
	      SUMX = SUMX + X
	      SUMX2 = SUMX2 + X**2
          END IF
8708    CONTINUE


C   Output RMS and save to variable if requested
	PRINT *
        IF (NPTS .GT. 1) THEN
          SUMX2 = ( (SUMX2 - SUMX**2/NPTS) / (NPTS-1) )
        ELSE
          SUMX2 = 0.
        END IF
        IF (SUMX2 .GT. 0) THEN
          SUMX2 = SQRT(SUMX2)
        ELSE
          SUMX2 = 0.
        END If
	PRINT *, 'RMS scatter: ',  SUMX2, '  NPTS: ', NPTS
	IF (VRMS) THEN
          CALL VARIABLE(CRMS,SNGL(SUMX2),.TRUE.)
	  IF (XERR) RETURN
        END IF
        IF (ROT) THEN
	  IF (VROT) THEN
            CALL VARIABLE(CROT,SNGL(THETA),.TRUE.)
	    IF (XERR) RETURN
          END IF
	  IF (VSCALE) THEN
            CALL VARIABLE(CSCALE,SNGL(SCALE),.TRUE.)
	    IF (XERR) RETURN
          END IF
	  IF (VDX) THEN
            CALL VARIABLE(CDX,SNGL(DX),.TRUE.)
	    IF (XERR) RETURN
          END IF
	  IF (VDY) THEN
            CALL VARIABLE(CDY,SNGL(DY),.TRUE.)
	    IF (XERR) RETURN
          END IF
        END IF

C   Loop to next iteration
8706  CONTINUE
      IF (HAVERES) THEN
          DO I = 1, NTOT
            X = XPOS(I,IMG)
            Y = YPOS(I,IMG)
            CALL GETXFUNC(X,Y,D,MAXPAR,NPAR,.FALSE.,.FALSE.,.TRUE.)

            XNEW = X
            YNEW = Y
            DO J=1,MAXPAR/2
              XNEW = XNEW + PAR(J,1)*D(J,1)
              YNEW = YNEW + PAR(J,2)*D(J+NPAR/2,2)
            END DO
            IF (ID2(I) .NE. 0) THEN
              J = ID2(I)
            ELSE
              J = I
            END IF
            ISIGN = 1
            IF (REJECT(I)) ISIGN = -1
            WRITE(7,8) ITER, ISIGN*J, XPOS(I,IMG), YPOS(I,IMG), 
     &        XNEW-XPOS(I,1), YNEW-YPOS(I,1), XNEW, YNEW
8	    FORMAT(2I6,2(0PF12.4),2(1PG12.4),2(0PF12.4))
          END DO
        END IF

       IF (HAVEDPOS) THEN
         DO 9706 I = 1, NTOT
           TMP = XPOS(I,IMG)
           XPOS(I,IMG) = XPOS(I,1)
           XPOS(I,1) = TMP
           TMP = YPOS(I,IMG)
           YPOS(I,IMG) = YPOS(I,1)
           YPOS(I,1) = TMP
9706	 CONTINUE
       END IF

C   Loop to next frame
8705  CONTINUE

      IF (HAVERES) CLOSE(7)

      RETURN

998   PRINT *, ' Error reading from file: ', NAME
      XERR = .TRUE.
      CLOSE(1)
      CLOSE(2)
      RETURN

999   PRINT *, ' Error opening file: ', NAME
1999  XERR = .TRUE.
      CLOSE(1)
      CLOSE(2)
      RETURN

      END

      SUBROUTINE  DINVERS (A, MAX, N, IFLAG)
C
C Although it seems counter-intuitive, the tests that I have run
C so far suggest that the 180 x 180 matrices that NSTAR needs can
C be inverted with sufficient accuracy if the elements are REAL*4
C rather than REAL*8.
C
C Arguments
C
C     A (INPUT/OUTPUT) is a square matrix of dimension N.  The inverse
C       of the input matrix A is returned in A.
C
C   MAX (INPUT) is the size assigned to the matrix A in the calling
C       routine.  It''s needed for the dimension statement below.
C
C IFLAG (OUTPUT) is an error flag.  IFLAG = 1 if the matrix could not
C       be inverted; IFLAG = 0 if it could.
C
      REAL*8 A(MAX,MAX)
C
C-----------------------------------------------------------------------
C
      IFLAG=0
      I=1
  300 IF(A(I,I).EQ.0.0D0)GO TO 9100
      A(I,I)=1.0D0/A(I,I)
      J=1
  301 IF(J.EQ.I)GO TO 304
      A(J,I)=-A(J,I)*A(I,I)
      K=1
  302 IF(K.EQ.I)GO TO 303
      A(J,K)=A(J,K)+A(J,I)*A(I,K)
  303 IF(K.EQ.N)GO TO 304
      K=K+1
      GO TO 302
  304 IF(J.EQ.N)GO TO 305
      J=J+1
      GO TO 301
  305 K=1
  306 IF(K.EQ.I)GO TO 307
      A(I,K)=A(I,K)*A(I,I)
  307 IF(K.EQ.N)GO TO 308
      K=K+1
      GO TO 306
  308 IF(I.EQ.N)RETURN
      I=I+1
      GO TO 300
C
C-----------------------------------------------------------------------
C
C Error:  zero on the diagonal.
C
 9100 IFLAG=1
      RETURN
C
      END
C
C=======================================================================
C
      SUBROUTINE  DVMUL (A, MAX, N, V, X)
C
C Multiply a matrix by a vector:
C
C                    A * V = X
C
C Arguments
C
C    A  (INPUT) is a square matrix of dimension N.
C
C  MAX  (INPUT) is the size assigned to the array in the calling
C       routine.
C
C    V  (INPUT) is a column vector of dimension N.
C
C    X  (OUTPUT) is a column vector of dimension N.
C
      REAL*8 SUM
      REAL*8 A(MAX,MAX), V(MAX)
      REAL*8 X(MAX)
C
C-----------------------------------------------------------------------
C
      I=1
  200 SUM=0.0D0
      J=1
  201 SUM=SUM+A(I,J)*V(J)
      IF (J .EQ. N) GO TO 203
      J=J+1
      GO TO 201
  203 X(I)=SNGL(SUM)
      IF (I .EQ. N) RETURN
C       Normal return
      I=I+1
      GO TO 200
      END

      SUBROUTINE GETXFUNC(X,Y,D,MAXPAR,NPAR,TRANS,ROT,CUBIC)

      REAL*8 X, Y, D(MAXPAR,2)
      LOGICAL TRANS, ROT, CUBIC

      IF (TRANS) THEN
C          Translation only
            NPAR = 2
            D(1,1) = 1.
            DO J=1,NPAR/2
              D(J,2) = 0.
              D(J+NPAR/2,1) = 0.
              D(J+NPAR/2,2) = D(J,1)
            END DO
      ELSE IF (ROT) THEN
C          Rotation, translation, and scale only
            NPAR = 4
            D(1,1) = 1.
            D(2,1) = X
            D(3,1) = Y
            D(4,1) = 0.
            D(1,2) = 0.
            D(2,2) = Y
            D(3,2) = -X
            D(4,2) = 1.
      ELSE IF (CUBIC) THEN
C          Full cubic transformation
            NPAR = 20
            D(1,1) = 1
            D(2,1) = X
            D(3,1) = Y
            D(4,1) = X*X
            D(5,1) = X*Y
            D(6,1) = Y*Y
            D(7,1) = X*X*X
            D(8,1) = X*X*Y
            D(9,1) = X*Y*Y
            D(10,1) = Y*Y*Y
            DO J=1,NPAR/2
              D(J,2) = 0.
              D(J+NPAR/2,1) = 0.
              D(J+NPAR/2,2) = D(J,1)
            END DO
      ELSE
C          Full linear transformation          
            NPAR = 6
            D(1,1) = 1
            D(2,1) = X
            D(3,1) = Y
            DO J=1,NPAR/2
              D(J,2) = 0.
              D(J+NPAR/2,1) = 0.
              D(J+NPAR/2,2) = D(J,1)
            END DO
      END IF

      RETURN
      END
