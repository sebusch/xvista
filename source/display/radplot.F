#include "Vista.h"

      SUBROUTINE RADPLOT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C
C   RADPLOT - Make a radial intensity plot of an image section
C
C     This subroutine allows the user to make a radial intensity plot
C     of a section of the currently displayed image in one of four ways:
C        1) non-interactive: center is as defined in the AXES common
C             block (AXES command)
C        2) non-interactive: center is specified as a command line argument
C        3) interactive: the center is marked with the video cursor
C        4) interactive: find centroid of intensity peak nearest the cursor
C             (similar to MARKSTAR)
C     If no image buffer is specified on the command line, the currently
C     displayed image is assumed.
C
C   Interactive Command:
C     TVRPLOT [RAD=r[,c]] [SCALE=s]
C
C   Non-Interactive Command:
C     RPLOT imbuf [RAD=r[,c]] [CEN=r0,c0] [SCALE=s] [HARD]
C
C   Optional Keywords:
C     imbuf     RPLOT ONLY.  Specify the image buffer.
C     RAD=r     sets the radius of the box in pixels in both rows and 
C                 columns.  A second argument sets a rectangular box
C                 with the radius in Rows and Columns separately defined.
C                 [Default:  RAD=5]
C     CEN=r0,c0 RPLOT ONLY.  Specify the central pixel for the radius plot
C     SCALE=s   specify the pixel scale in Arcsec/Pixel for the plot
C                 (does not apply to the RAD= keyword values)
C     HARD      RPLOT ONLY.  Make a hardcopy plot
C
C   TVRPLOT Interactive Mode Cursor Commands:
C     C        Compute the centroid and make the radial intensity plot
C     X        Use the current cursor position as the center
C     E        Finish making radial intensity plots and exit
C     :        Make a hardcopy of the current radial intensity plot
C     S        Change the plotting box radius interactively
C     ?        Print this Help Menu
C
C --------------------
C
C  VISTA Version 4.2.OSU
C  R. Pogge
C  OSU Astronomy Dept
C  pogge@bruno.mps.ohio-state.edu
C  1992 May 15
C
C  Modification History:
C     1992 May 15 - New Program [RWP]
C     1992 May 21 - various bug patches and options [RWP]
C
C----------------------------------------------------------------------

C  The Image

      REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C  Include statements for VISTA.
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Image headers and parameters
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC' ! File information
      INCLUDE 'VINCLUDE:TV.INC'
      INTEGER SYS$CANCEL
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/tv.inc'
#endif

C  Statements for this routine.

      CHARACTER*8  PARM
      CHARACTER*80 TITLE, VPLOTID, FSTRCAT
      CHARACTER*1  KEY
      CHARACTER*20 NSTR, NSTR1

      INTEGER ROW, COL, UPPER, IDR, IDC, IC0, IR0
      INTEGER NPR, NPC, IPSR, IPER, IPSC, IPEC
      INTEGER MARK

      REAL*4  F(2)
      REAL*4  R0, C0, DR, DC, RAD, FLX
      REAL*4  PSCALE, LWEIGHT
      REAL*4  RMAX, YMN, YMIN, YMX, YMAX, YTEXT

C  Logical Flags

      LOGICAL INTERACT, HARD, HAVEMIN, HAVEMAX
      LOGICAL FINDCENT, OK, NOERASE, WRITEOUT, MASK
      LOGICAL HAVESCALE, HAVEPLOT, HAVECENT, HAVELOG
      LOGICAL KEYCHECK

C  The AX common block:    
C     ECCAX     Eccentrity of the pixels
C     ANGAX     Angle of the principal axes
C     ANGMAJAX  Angle of the major axis
C     ANGMINAX  Angle of the minor axis
C     XCAX      Column centroid location
C     YCAX      Row centroid location
C     XPAX      Column peak pixel location
C     YPAX      Row peak pixel location

      COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &            XPAX, YPAX

C   VISTA default terminal and hardcopy devices

      INTEGER VTERM, VHARD
      COMMON /VGRAPHICS/ VTERM, VHARD

C  Figure out if interactive or non-interactive mode

      IF (COM .EQ. 'RPLOT') THEN
         INTERACT = .FALSE.
      ELSE
         INTERACT = .TRUE.
      END IF

C  Keyword definitions  

      CALL KEYINIT
      CALL KEYDEF('RAD=')
      CALL KEYDEF('SCALE=')
      CALL KEYDEF('MIN=')
      CALL KEYDEF('MAX=')
      CALL KEYDEF('COLOR=')
      CALL KEYDEF('NOERASE')
      CALL KEYDEF('LOG')
      CALL KEYDEF('MASK')
      IF (.NOT. INTERACT) THEN
         CALL KEYDEF('CEN=')
         CALL KEYDEF('HARD')
      END IF

C  Make sure all of the keywords given are valid

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Make sure there is an image displayed if in interactive mode

      IF (INTERACT .AND. (.NOT. TVSTAT)) THEN
         PRINT *,'No image is currently displayed'
         XERR = .TRUE.
         RETURN
      END IF

C  Other initializations

      HAVECENT = .FALSE.
      HAVEPLOT = .FALSE.
      HAVESCALE = .FALSE.
      HAVEMIN = .FALSE.
      HAVEMAX = .FALSE.
      NOERASE = .FALSE.
      HAVELOG = .FALSE.
      WRITEOUT = .FALSE.
      HARD = .FALSE.
      PSCALE = 1.0
      IDR = 5
      IDC = 5
      ICOLOR = 1
      MASK = .FALSE.

C  Parse the command line

      DO 8701 I = 1, NCON
         L = UPPER(WORD(I))

C  User has specified the radius of the region of interest (RPLOT & TVRPLOT)

         IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
            CALL ASSIGNV(WORD(I),2,F,NRAD,PARM)
            IF (XERR) RETURN
            IF (NRAD .EQ. 1) THEN
               IDR = NINT(F(1))
               IDC = IDR
            ELSE
               IDR = NINT(F(1))
               IDC = NINT(F(2))
            END IF
            IF (IDR .LT. 1 .OR. IDC .LT. 1) THEN
               PRINT *,'Radii should be larger than 1 pixel'
               XERR = .TRUE.
               RETURN
            END IF

C  User has specified a plotting scale (RPLOT & TVRPLOT)

         ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
            CALL ASSIGN(WORD(I),PSCALE,PARM)
            IF (XERR) RETURN
            HAVESCALE = .TRUE.

C  Set plotting limits
         ELSE IF (WORD(I)(1:4) .EQ. 'MIN=') THEN
            CALL ASSIGN(WORD(I),YMIN,PARM)
            IF (XERR) RETURN
            HAVEMIN = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'MAX=') THEN
            CALL ASSIGN(WORD(I),YMAX,PARM)
            IF (XERR) RETURN
            HAVEMAX = .TRUE.

         ELSE IF (WORD(I)(1:6) .EQ. 'COLOR=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            ICOLOR = NINT(TMP)

C   User has specified the center (RPLOT only)

         ELSE IF (WORD(I)(1:4) .EQ. 'CEN=') THEN
            CALL ASSIGNV(WORD(I),2,F,NPAR,PARM)
            IF (XERR) RETURN
            IF (NPAR .LT. 2) THEN
               PRINT *,'Usage: CEN=R0,C0  -- 2 values required'
               XERR = .TRUE.
               RETURN
            END IF
            R0 = F(1)
            C0 = F(2)
            HAVECENT = .TRUE.

C  Make a Hardcopy plot (RPLOT only - hardcopy in TVPLOT through a cursor key)
         ELSE IF (WORD(I)(1:4) .EQ. 'HARD') THEN
            HARD = .TRUE.

         ELSE IF (WORD(I) .EQ. 'NOERASE') THEN
            NOERASE = .TRUE.

         ELSE IF (WORD(I) .EQ. 'LOG') THEN
            HAVELOG = .TRUE.

         ELSE IF (WORD(I) .EQ. 'MASK') THEN
            MASK = .TRUE.

         END IF
 8701 CONTINUE

C  Print basic instructions for interactive mode

      IF (INTERACT) THEN
         write(*,'(1x,''TVRPLOT Cursor Commands:'')')
         write(*,'(3x,''C . . . Mark Star and do the plot'')')
         write(*,'(3x,''X . . . Make plot here'')')
         write(*,'(3x,''S . . . Change the plotting box radius'')')
         write(*,'(3x,'': . . . Make a Hardcopy of this plot'')')
         write(*,'(3x,''E . . . Exit TVRPLOT'')')
         write(*,'(3x,''? . . . Print this Menu'')')
      END IF

C*********************
C
C  INTERACTIVE MODE
C

 777  IF (INTERACT) THEN

C     Get a keystroke from the user and execute the command

         ISTAT = MARK(ROW,COL,KEY)
         L = UPPER(KEY)

C** '?' = Help

         IF (KEY .EQ. '?') THEN
            write(*,'(1x,''TVRPLOT Cursor Commands:'')')
            write(*,'(3x,''C . . . Mark Star and do the plot'')')
            write(*,'(3x,''X . . . Make plot here'')')
            write(*,'(3x,''S . . . Change the plotting box radius'')')
            write(*,'(3x,'': . . . Make a Hardcopy of this plot'')')
            write(*,'(3x,''E . . . Exit TVRPLOT'')')
            write(*,'(3x,''? . . . Print this Menu'',/)')

C** 'E' = Exit TVRPLOT

         ELSE IF (KEY .EQ. 'E') THEN
            PRINT *,'TVRPLOT Done'
            RETURN

C** ':' = Make a Hardcopy of the Current Plot

         ELSE IF (KEY .EQ. ':') THEN
            IF (HAVEPLOT) THEN
               HARD = .TRUE.
               GO TO 666
            ELSE
               PRINT *,'NO CURRENT PLOT! -- CANNOT MAKE HARDCOPY'
               GO TO 777
            END IF

C** 'S' = Change the Plotting Box Radius

         ELSE IF (KEY .EQ. 'S') THEN
 4000       IF (IDR .EQ. IDC) THEN
               WRITE(*,'(1X,''Current Box Radius ='',I3)') IDR
               WRITE(*,'(1X,''New Radius DR: '',$)')
               READ(*,*) F(1)
               F(2) = F(1)
            ELSE
               WRITE(*,301) IDR, IDC
301            FORMAT(1X,'Current Box Radii (DR,DC): (',i3,',',i3,')') 
               CALL ASKDATA('New Radii (DR,DC):',F,2)
            END IF
            IDR = NINT(F(1))
            IDC = NINT(F(2))
            IF (IDR .LT. 1 .OR. IDC .LT. 1) THEN
               PRINT *,'Radii should be larger than 1 pixel'
               GO TO 4000
            END IF

         ELSE IF (KEY .EQ. 'O') THEN
            WRITEOUT = .NOT. WRITEOUT

         END IF

         IF (KEY .NE. 'X' .AND. KEY .NE. 'C') GO TO 777

C
C   BOTTOM OF THE CURSOR COMMAND LOOP
C
C***************************************

C  Execute the 'C' or 'X' commands
C

         IF (KEY .EQ. 'X') THEN
            R0 = FLOAT(ROW)
            C0 = FLOAT(COL)
            IF (ROW .LT. IMAGESR .OR. ROW .GT. IMAGEER) THEN
               PRINT *,'Cursor is Out of Range Vertically'
               GO TO 777
            ELSE IF (COL .LT. IMAGESC .OR. COL .GT. IMAGEEC) THEN
               PRINT *,'Cursor is Out of Range Horizontally'
               GO TO 777
            END IF

         ELSE
            R0 = FLOAT(ROW)
            C0 = FLOAT(COL)
            MAXITER = 6
            ISIZE = 3
            OK = FINDCENT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                    C0,R0,ISIZE,MAXITER,MASK)
            IF (.NOT. OK) THEN
               PRINT *,'Cannot Compute Centroid'
               GO TO 777
            END IF
         END IF
         IR0 = NINT(R0)
         IC0 = NINT(C0)

C  Non-Interactive Mode  (RPLOT)

      ELSE
         IF (.NOT. HAVECENT) THEN
            IF (XCAX .EQ. 0. .AND. YCAX .EQ. 0.) THEN
               PRINT *,'No Centroid computed with AXES, and no CEN='
               PRINT *,'keyword given.  RPLOT Aborting'
               XERR = .TRUE.
               RETURN
            END IF
            R0 = YCAX
            C0 = XCAX
         END IF
         IR0 = NINT(R0)
         IC0 = NINT(C0)
         IF (IR0 .LT. IMAGESR .OR. IR0 .GT. IMAGEER) THEN
            WRITE(*,'(1X,''Center: ('',f8.2,'','',f8.2,'')'')') R0,C0
            PRINT *,'Center is Out of Range Vertically'
            XERR = .TRUE.
            RETURN
         ELSE IF (IC0 .LT. IMAGESC .OR. IC0 .GT. IMAGEEC) THEN
            WRITE(*,'(1X,''Center: ('',f8.2,'','',f8.2,'')'')') R0,C0
            PRINT *,'Center is Out of Range Horizontally'
            XERR = .TRUE.
            RETURN
         END IF
      END IF

C  Do the Plot

 666  CALL PLOTSETUP(HARD,.TRUE.,' ',.FALSE.,NOERASE,.FALSE.,XERR)
      IF (XERR) RETURN

      IF (HARD) THEN
         LWEIGHT=0.5
      ELSE
         LWEIGHT=1.0
      END IF

      CALL PMGO(SETLWEIGHT)(LWEIGHT)

C   Setup the limits of the image section for which the radial plot is to be 
C   drawn

      IPSR = IR0 - IDR
      IPSR = MIN(IMAGEER,MAX(IMAGESR,IPSR))
      IPER = IR0 + IDR
      IPER = MIN(IMAGEER,MAX(IMAGESR,IPER))
      IPSC = IC0 - IDC
      IPSC = MIN(IMAGEEC,MAX(IMAGESC,IPSC))
      IPEC = IC0 + IDC
      IPEC = MIN(IMAGEEC,MAX(IMAGESC,IPEC))

C   Do a first pass over the box to find the limits in R and I, unless
C   we are doing a hardcopy of the current plot

      IF (.NOT. (HARD .AND. INTERACT)) THEN
         YMN = A(IPSR,IPSC)
         YMX = A(IPSR,IPSC)
         RMAX = 0.0
         DO 100 IR = IPSR, IPER
            DR = R0 - FLOAT(IR)
            DO 101 IC = IPSC, IPEC
               DC = C0 - FLOAT(IC)
               FLX = A(IC,IR)
               IF (HAVELOG) THEN
                 IF (FLX .GT. 0) THEN
                   YMN = AMIN1(YMN,LOG10(FLX))
                   YMX = AMAX1(YMX,LOG10(FLX))
                 END IF
               ELSE
                 YMN = AMIN1(YMN,FLX)
                 YMX = AMAX1(YMX,FLX)
               END IF
               RAD = PSCALE*SQRT(DR*DR + DC*DC)
               RMAX = AMAX1(RMAX,RAD)
 101        CONTINUE
 100     CONTINUE
         RMAX = 1.1*RMAX
         IF (.NOT. HAVEMIN) YMIN = YMN - 0.1*(YMX-YMN)
         IF (.NOT. HAVEMAX) YMAX = YMX + 0.1*(YMX-YMN)
      END IF

C   Set the Plotting limits & Draw the plotting box, with labels

      CALL PMGO(SETEXPAND)(1.001)
      CALL PMGO(SETLIM)(0.0,YMIN,RMAX,YMAX)
      CALL PMGO(ABOX)(1,2,0,0)
      IF (HAVESCALE) THEN
         CALL PMGO(XLABEL)(15,'Radius (Arcsec)')
      ELSE
         CALL PMGO(XLABEL)(15,'Radius (Pixels)')
      END IF
      CALL PMGO(YLABEL)(9,'Intensity')
      IF (COM .EQ. 'TVRPLOT') THEN
         CALL CCCHEAD('OBJECT',HEADBUF(1,IMTV),TITLE)
      ELSE
         CALL CCCHEAD('OBJECT',HEADBUF(1,IM),TITLE)
      END IF
      L = NUMCHAR(TITLE)
      CALL PMGO(TLABEL)(L,TITLE(1:L))
      ITMP = INT(1000.0*R0)
C      PRINT *, ITMP
      CALL NUMBSTR(ITMP,-3,1,NSTR,LNSTR)
      ITMP = INT(1000.0*C0)
C      PRINT *, ITMP
      CALL NUMBSTR(ITMP,-3,1,NSTR1,LNSTR1)
      VPLOTID = FSTRCAT('Center @ R=',
     &           FSTRCAT(NSTR(1:LNSTR),
     &            FSTRCAT(', C=',NSTR1(1:LNSTR1))))
C      PRINT *,VPLOTID(1:NUMCHAR(VPLOTID))
      YTEXT = YMAX + 0.025*(YMAX - YMIN)
      CALL PMGO(SETEXPAND)(0.5)
      CALL PMGO(RELOCATE)(0.0,YTEXT)
      CALL PMGO(LABEL) (NUMCHAR(VPLOTID),VPLOTID(1:NUMCHAR(VPLOTID)))
      CALL PMGO(SETEXPAND)(1.001)

C   Loop over the contents of the box, putting a + at each (R,F) point

      CALL PMGO(SETANGLE)(45.0)
      CALL PMGO(SETCOLOR)(ICOLOR)
      DO 200 IR = IPSR, IPER
         DR = R0 - FLOAT(IR)
         DO 201 IC = IPSC, IPEC
            DC = C0 - FLOAT(IC)
            FLX = A(IC,IR)
            RAD = PSCALE*SQRT(DR*DR + DC*DC)
            IF (HAVELOG) THEN
              IF (FLX .GT. 0) CALL PMGO(RELOCATE)(RAD,LOG10(FLX))
            ELSE
              CALL PMGO(RELOCATE)(RAD,FLX)
            END IF
            CALL PMGO(POINT)(4,1)
            IF (WRITEOUT) WRITE(olu,444) DC, DR, RAD, FLX
 444        FORMAT(4F12.3)
 201     CONTINUE
 200  CONTINUE
      CALL PMGO(SETANGLE)(0.0)

      HAVEPLOT = .TRUE. 

C   Finish the plot as required

      IF (HARD) THEN
         N = PMGO(FILEPLOT)(0)
         IF (VHARD .NE. 5) PRINT *,'Number of Vectors Plotted:',N
      ELSE
         CALL PMGO(TIDLE)
      END IF

C   If in interactive cursor mode, go back to the interactive command
C   loop, otherwise exit

      IF (INTERACT) GO TO 777
      RETURN
      END
