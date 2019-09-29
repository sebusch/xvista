#include "Vista.h"
        SUBROUTINE COLORSC

C       Routine to load or change the AED color map

C       This routine can be used to create or modify the color map
C       used by the AED512 to display the pixel intensities.  The AED
C       displays each pixel as one of 256 possible values, therefore,
C       the color translation map contains 256 seperate entries for
C       each of the red, green, blue guns.  This routine by default
C       allows you to define a color map by ramping between specified
C       color levels at specified pixel intensity values.  If desired
C       the map can be saved to be recalled later.

C       KEYWORDS:       - none -        Interactively define a new color map
C                       CF='xxx'        Get and load color map file xxx.CLR
C                       INV             Invert the current color map
C                       BW              Load a black and white color map

C       Modified by:    April Atwood    10/16/86, 1/87
#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
        INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Default file information
        INCLUDE 'VINCLUDE:TV.INC'
        INTEGER SYS$CANCEL
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif

#if defined(__AED512) || defined(__AED1024)
C       AED commands
        INCLUDE 'VINCLUDE:AED.INC'
        BYTE COLOR(256,3), CBYTE
#endif
#ifdef __PER
        INTEGER R(256),G(256),B(256),ICOLOR
#endif
#if defined(__SUNVIEW) || defined(__X11)
        INTEGER*2 R(256),G(256),B(256),ICOLOR,COLSAV(256,3)
        INTEGER TVCOLORLD
#endif
        CHARACTER*80 JSTRG, FILENAME, TWORD
        CHARACTER*5 COLORN(3)
        CHARACTER*3 YN
	INTEGER UPPER

        LOGICAL CFILE, INTDEF, INV, BW
#if defined(__AED512) || defined(__AED1024)
        COMMON /TVCOLOR/ COLOR
        EQUIVALENCE (CBYTE,ICOLOR)
#endif
#ifdef __PER
        COMMON /TVCOLOR/ R,G,B
#endif
#if defined(__SUNVIEW) || defined(__X11)
        COMMON /TVCOLOR/ R,G,B,COLSAV,LCOLZ,LCOLCON
#endif

        LOGICAL KEYCHECK

        DATA COLORN /'Red  ','Green','Blue '/

C       List allowed keywords. Don`t check till later to avoid capitalization.

        CALL KEYINIT
        CALL KEYDEF('BW')
        CALL KEYDEF('CF=')
        CALL KEYDEF('INV')

C       Check parameter list

        CFILE   =.FALSE.
        INTDEF  =.TRUE.
        INV     =.FALSE.
        BW      =.FALSE.
        IF (.NOT. TVSTAT) THEN
                PRINT *,'TV is not initialized - Call TV command'
                XERR    =.TRUE.
                RETURN
        END IF

        DO 8701 I=1,NCON
		TWORD = WORD(I)
		L = UPPER(TWORD)
                IF (TWORD(1:3) .EQ. 'CF=') THEN
C                  Read in color file
                        JSTRG   =WORD(I)(4:80)
                        CFILE   =.TRUE.
                        BW      =.FALSE.
                        INTDEF  =.FALSE.
                ELSE IF (TWORD .EQ. 'BW') THEN
C                  Load B&W color map
                        CFILE   =.FALSE.
                        BW      =.TRUE.
                        INTDEF  =.FALSE.
                ELSE IF (TWORD .EQ. 'INV') THEN
C                  Invert color map
                        INV     =.TRUE.
                        INTDEF  =.FALSE.
                END IF
8701    CONTINUE

        IF (CFILE) THEN
C             Read in old color map file
                CALL FILEDEF(JSTRG,FILENAME,COLORDIR,COLOREXT)
                OPEN (4,FILE=FILENAME,
#ifdef __READONLY
     &                  READONLY,
#endif
     &                  STATUS='OLD',IOSTAT=IERR)
                IF (IERR .NE. 0) THEN
                        PRINT *,'Can''t open color map file ',FILENAME
                        XERR    =.TRUE.
                        RETURN
                END IF

#if defined(__AED512) || defined(__AED1024)
                READ (4,ERR=999) COLOR
#endif
#if defined(__PER) || defined(__SUNVIEW) || defined(__X11)
                do icolor=1,256
                READ (4,*,ERR=999) R(icolor),G(icolor),B(icolor)
                end do
#endif
#ifndef MSBFirst
C		IERR = PACKFIT(R,R,512)
C		IERR = PACKFIT(G,G,512)
C		IERR = PACKFIT(B,B,512)
#endif
                CLOSE(4)
        END IF

C       If the user gave us any unknown keywords, exit.
        IF (.NOT. KEYCHECK()) THEN
                XERR = .TRUE.
                RETURN
        END IF

        IF (BW) THEN
C           Ramp up to make B&W map
#if defined(__AED512) || defined(__AED1024)
                DO 8702 I=1,3
                        DO 8703 J=1,255
                                ICOLOR  =J-1
                                COLOR(J,I)      =CBYTE
8703                    CONTINUE
8702            CONTINUE
                COLOR(256,1)    =255
                COLOR(256,2)    =0
                COLOR(256,3)    =255
#endif
#if defined(__SUNVIEW) || defined(__X11)
                DO 8704 I=1,256
                  R(I) = I-1
                  G(I) = I-1
                  B(I) = I-1
8704            CONTINUE
                R(256) = 255
                G(256) = 0
                B(256) = 255
#endif
        END IF

        IF (INTDEF) THEN
C           Set up color map interactivly
        DO 8705 I=1,3
                LL      =0
                IF (I .EQ. 1) THEN
                   PRINT *,'Interactive definition of color map:'
                   PRINT *,'Type Coltrol-C followed by a number'
                   PRINT *,'followed by return to exit immediately.'
                   PRINT *,' '
                END IF

50              IF (NOGO) RETURN
                PRINT 101, COLORN(I), LL
101             FORMAT (/,1X,A,' starting level',I2,'  Enter initial value: ',$)
                READ (*,*,ERR=50), IVL
                IF (NOGO) RETURN
                IF (IVL .LT. 0 .OR. IVL .GT. 255) GO TO 50

C       Step up into the levels for the given color table

51              IF (NOGO) RETURN
                PRINT 102
102             FORMAT (' Go to level:    ',$)
                READ (*,*,ERR=51), LN
                IF (NOGO) RETURN
                IF (LN .LT. 0) GO TO 60
                IF (LN .LE. LL .OR. LN .GT. 255) GO TO 51

52              IF (NOGO) RETURN
                PRINT 103
103             FORMAT (' With intensity: ',$)
                READ (*,*,ERR=52), IVN
                IF (NOGO) RETURN
                IF (IVN .GT. 255) GO TO 52
                IF (IVN .LT. 0) GO TO 60

C       Ramp between new color value and the old one

                SLOPE   =FLOAT(IVN-IVL)/FLOAT(LN-LL)
                DO 8706 J=LL+1,LN
                        ICOLOR  =(J-LL)*SLOPE+IVL
#if defined(__AED512) || defined(__AED1024)
                        COLOR(J,I)      =CBYTE
#endif
#if defined(__PER) ||  defined(__SUNVIEW) || defined(__X11)
                        IF (I .EQ. 1) THEN
                                R(J) = ICOLOR
                        ELSE IF (I .EQ. 2) THEN
                                G(J) = ICOLOR
                        ELSE
                                B(J) = ICOLOR
                        END IF
#endif
8706            CONTINUE

                LL      =LN
                IVL     =IVN
                IF (LN .LT. 255) GO TO 51
60              CONTINUE
8705    CONTINUE

C       Load graphics color into highest level

                PRINT 107
107             FORMAT (' Enter graphics color levels')
                DO 8707 I=1,3
70                      IF (NOGO) RETURN
                        PRINT 108, COLORN(I)
108                     FORMAT (1X,A,': ',$)
                        READ (*,*,ERR=70) ICOLOR
                        IF (NOGO) RETURN
                        IF (ICOLOR .GE. 0 .AND. ICOLOR .LE. 255) THEN
#if defined(__AED512) || defined(__AED1024)
                                COLOR(256,I)    =CBYTE
#endif
#if defined(__PER) || defined(__SUNVIEW) || defined(__X11)
                                IF (I .EQ. 1) THEN
                                  R(256) = ICOLOR
                                ELSE IF (I .EQ. 2) THEN
                                  G(256) = ICOLOR
                                ELSE
                                  B(256) = ICOLOR
                                ENDIF
#endif
                        ELSE
                                GO TO 70
                        END IF
8707            CONTINUE
        END IF

C       Invert current color map if requested

        IF (INV) THEN
                DO 8708 I=1,127
#if defined(__AED512) || defined(__AED1024)
                        DO 8709 J=1,3
                                CBYTE           =COLOR(I,J)
                                COLOR(I,J)      =COLOR(256-I,J)
                                COLOR(256-I,J)  =CBYTE
8709                    CONTINUE
#endif
#if defined(__PER) || defined(__SUNVIEW) || defined(__X11)
                                ICOLOR = R(I)
                                R(I) = R(256-I)
                                R(256-I) = ICOLOR
                                ICOLOR = G(I)
                                G(I) = G(256-I)
                                G(256-I) = ICOLOR
                                ICOLOR = B(I)
                                B(I) = B(256-I)
                                B(256-I) = ICOLOR
#endif
8708            CONTINUE
        END IF

C       Load color table into TV display

#ifdef __AED512
C     Cancel pending IO
        ISTAT   =SYS$CANCEL(%VAL(TTCHAN))
        CALL AECOLOR(1,0,256,COLOR(1,1),COLOR(1,2),COLOR(1,3))
        CALL AEFLUSH(0)
        CALL AEDION
C                                       ! Reset IO
#endif
#ifdef __AED1024
        CALL QCOLOR(1,0,256,COLOR(1,1),COLOR(1,2),COLOR(1,3))
#endif
#ifdef __PER
        IF (BW) THEN
#ifdef __HAVEBW
		CALL FGRYVLT (1,127,128,-2)
#else
                CALL FGRYVLTO(1,127,0,-2)
                CALL FGRYVLTO(1,127,255,-1)
                CALL FGRYVLTO(1,127,255,-4)
#endif
        ELSE
                CALL FCOLVLT (R,G,B)
        END IF
#endif
#if defined(__SUNVIEW) || defined(__X11)
        IF (TVCOLORLD(R,G,B,256,0) .LT. 0) THEN
                PRINT *, 'Error transmitting color map '
                XERR = .TRUE.
                RETURN
        END IF
	LCOLCON = 0
	LCOLZ = 0
	DO 2770 I=1,256
	  COLSAV(I,1) = R(I)
	  COLSAV(I,2) = G(I)
	  COLSAV(I,3) = B(I)
 2770   CONTINUE
#endif
        TVCOL   =2
        IF (INTDEF) THEN
C          Save file?
54              PRINT 104
104             FORMAT (' Save color file? ',$)
                READ 106, YN
106             FORMAT (A)
                IF (YN(1:1) .EQ. 'Y' .OR. YN(1:1) .EQ. 'y') THEN
                        PRINT 105
105                     FORMAT (' Enter filename: ',$)
                        READ 106, JSTRG
                        CALL FILEDEF(JSTRG,FILENAME,COLORDIR,COLOREXT)
                        OPEN (4,FILE=FILENAME,
#ifdef VMS
     &                          STATUS='NEW',
#else
     &                          STATUS='UNKNOWN',
#endif
     &                          IOSTAT=IERR)
                        IF (IERR .NE. 0) THEN
                                PRINT *,'Can''t open color map file ',
     &                                FILENAME
                                XERR    =.TRUE.
                                RETURN
                        END IF
#if defined(__AED512) || defined(__AED1024)
                        WRITE (4) COLOR
#endif
#if defined(__PER) || defined(__SUNVIEW) || defined(__X11)
                        do icolor=1,256
                          WRITE (4,*,ERR=999) R(icolor),G(icolor),B(icolor)
                        end do
#endif
                        CLOSE (4,STATUS='KEEP')

                ELSE IF (YN .NE. 'N' .AND. YN .NE. 'n') THEN
                        GO TO 54

                END IF

        END IF

        RETURN

999	PRINT *, 'Error reading color file: ', FILENAME
        XERR = .TRUE.
        RETURN

        END
