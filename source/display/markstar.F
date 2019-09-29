#include "Vista.h"
	SUBROUTINE MARKSTAR(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C       Stores positions of stars to make a photometry file.

C------------------------------------------------------------------------------
C
C  This routine constructs a 'photometry file', which is a listing of
C  information about stars on a CCD image.
C
C  The user of this routine marks the approximate positions of the stars
C  on the CCD frame with the TV cursor.  The exact position of the star
C  is found by multiple centroiding.
C  These positions, along with other information which the user has
C  the option to enter, are written to a common block.  The data in that
C  block can be written to the disk with the SAVE command.
C
C  This routine is called by the user with:
C
C       MARKSTAR [NEW] [NOBOX] [RADIUS=n] [DR=r] [DC=c] [AUTO]
C               [RSHIFT=rs] [CSHIFT=cs] [STAR=s1,s2,s3,s4,s5...]
C
C
C       NEW     If this word is in the command, a new list is started.
C               Otherwise, the program appends any new stars to an
C               already existing list.
C
C       NOBOX   prevents display of previously marked stars if the 'new' option
C               is not being run.  Prevents display of boxes with the
C               'auto' option.
C
C       RADIUS  lets the size of the region used in the multiple centroiding.
C               If not given, the default is to fit on a 7 by 7 box.
C
C       DC and DR       give offsets to apply to the positions of the
C               stars of the photometry list when showing the positions.
C               This can be used to compare frames.
C
C       AUTO    has the program look for stars at positions specified by
C               an existing photometry file, with offsets given by dr and dc.
C
C       RSHIFT and CSHIFT       give limits on how far the centroid in
C                               the AUTO option can be from the original
C                               position
C       STAR=s1,s2...           mark the listed stars on the TV
C
C       The format for the records in the data file is
C
C               PHOTLABEL, PHOTDATA(NUMDATA)
C
C       LABEL is a character*80 descriptior of the star in the image
C       Currently, NUMDATA is set equal to 30; see 'PHOTFILES.INC'.
C       The arrangement of the records in the file is determined by
C       PARAMETER constants set up in the include file.
C
C       Ths position of the last marked star is saved in the variables
C       'R' and 'C'.
C
C       Language:               FORTRAN 77
C       Programmer:             Donald Terndrup
C       Date of Last Revision:  April 17, 1985       Version 3
C------------------------------------------------------------------------------

C       The image.

	REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C       Include statements for VISTA.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! VISTA parameters and commons
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image sizes
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Location of datafiles
	INCLUDE 'VINCLUDE:PHOTFILES.INC'       ! Array for holding data
	INCLUDE 'VINCLUDE:TIMEDATA.INC'        ! For observation parameters
	INCLUDE 'VINCLUDE:TV.INC'
	INCLUDE 'VINCLUDE:WORK.INC'
	INTEGER SYS$CANCEL
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
	INCLUDE 'vistadisk/source/include/timedata.inc'
	INCLUDE 'vistadisk/source/include/tv.inc'
	INCLUDE 'vistadisk/source/include/work.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
	INCLUDE 'VINCLUDE:AED.INC'             ! Constants for TV
	BYTE BKEY, COLOR(256,3), COLRBUF(32)
#endif
#ifdef __PER
	PARAMETER (NCUR = 8)
	INTEGER LASTX(NCUR), LASTY(NCUR)
	BYTE OLD(512,512), NEW(512,512)
#endif
	PARAMETER (CONVERSN = 180.0 / 3.141562654 * 3600.0 / 15.)
C                               ! Rad -> sec time

C       Components of a photometry record.

	CHARACTER*80    PHOTLABEL, FSTRCAT
	REAL*4          PHOTDATA(NUMDATA)
	REAL*8          DVAL

C       Statements for this routine.

	CHARACTER*8  PARM, FITSCARD
	CHARACTER*1  KEY,      OLDKEY

	REAL*4       TEMPLIST(100)
	REAL*4       XPLOT(1024), YPLOT(1024)

	INTEGER      ROW,      COL,   BOXCOLOR, SR, ER, SC, EC
c       INTEGER      ISR,      ISC
#if defined(__PER) || defined(__AED512) || defined(__AED1024)
	INTEGER      ROW2,     COL2
#endif
	INTEGER      UPPER

	LOGICAL      APPEND,   DATHEAD,  DOBOX, RADIUSSET, REFERENCE, REFPOS
	LOGICAL      OK, ON, AUTO, EXIT, OP, MULT, FINDCENT, MASK
	LOGICAL      DISABLE,  SELECTSTARS, CLR, FIRSTRUN, COMPLETE
	LOGICAL      HAVERSHIFT, HAVECSHIFT, QSAVE, BLINKSTAT,BLINK
	LOGICAL      KEYCHECK, ISONLIST, ROTATE, QUICk
	LOGICAL      COLORON, OBSNUM, CIRC, CROSS, HAVEFOCUS, SURGICAL
        LOGICAL      ZAPAPPEND, HAVEZAPFILE
        CHARACTER    ZAPFILE*80, TWORD*300
        REAL         FOC(2) 
#ifdef __PER
	LOGICAL      UNREAD, ORIGINAL
#endif

	COMMON /TVCURS/   ON,     DISABLE
	COMMON /BLINKTV/ IXLEFT, IYLEFT, IXRIGHT, IYRIGHT, IBLZ,
     &                   BLINKSTAT

C       We need to save the row and column positions of the photometry
C       list if we are running in the AUTO option.

	INTEGER DAONUMS
	COMMON /LISTOFPOSITIONS/ ROWS(MAXNUMSTAR),
     &        COLS(MAXNUMSTAR), DAONUMS(MAXNUMSTAR), STARINFO(MAXNUMSTAR)
	COMMON /MOMENT/ FMOMENT

C       We also need a list of which records were found in the AUTO option.
	REAL            DELTAR(MAXNUMSTAR), DELTAC(MAXNUMSTAR)
	LOGICAL         WASFOUND(MAXNUMSTAR)
        INTEGER RSIZE, CSIZE, SEARCH
	REAL ZAPTEMP(WORKSIZE)
	COMMON /WORK/   DELTAR, DELTAC, WASFOUND, XPLOT, YPLOT, ZAPTEMP
	INTEGER VTERM, VHARD
	COMMON /VGRAPHICS/ VTERM, VHARD
#ifdef __PER
	UNREAD = .TRUE.
	ORIGINAL = .TRUE.
	ISR = IRTV
	ISC = ICTV
C               TV constants
	COL2 = NCTV / (2*ICOMP)
	ROW2 = NRTV / (2*ICOMP)
	IF (IEXP .GT. 1) THEN
		COL2 = IEXP*NCTV/2
		ROW2 = IEXP*NRTV/2
	END IF
C               Turn on overlay if not on
	CALL FVLTSEL(0)
#endif

C       List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('NEW')
	CALL KEYDEF('NOBOX')
	CALL KEYDEF('RADIUS=')
	CALL KEYDEF('DR=')
	CALL KEYDEF('DC=')
	CALL KEYDEF('AUTO')
	CALL KEYDEF('RSHIFT=')
	CALL KEYDEF('CSHIFT=')
	CALL KEYDEF('STAR=')
	CALL KEYDEF('ID=')
	CALL KEYDEF('MULT')
	CALL KEYDEF('OBSNUM')
	CALL KEYDEF('DMIN=')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('COMPLETE')
	CALL KEYDEF('ANGLE=')
	CALL KEYDEF('AR=')
	CALL KEYDEF('AC=')
	CALL KEYDEF('QUICK')
	CALL KEYDEF('QSAVE')
	CALL KEYDEF('EXIT')
	CALL KEYDEF('CLR')
	CALL KEYDEF('MOMENT=')
	CALL KEYDEF('REF')
	CALL KEYDEF('REFPOS')
	CALL KEYDEF('COLOR=')
	CALL KEYDEF('CIRC=')
	CALL KEYDEF('CROSS')
	CALL KEYDEF('SIGLIM=')
	CALL KEYDEF('FOCUS=')
	CALL KEYDEF('SIZE=')
	CALL KEYDEF('SIG=')
	CALL KEYDEF('SEARCH=')
	CALL KEYDEF('SURGICAL')
	CALL KEYDEF('ZAPFILE=')
	CALL KEYDEF('APPEND')
	CALL KEYDEF('MASK')

C       Make sure there is an image in the television.
	IF (.NOT. TVSTAT) THEN
		PRINT *,'There is no image in the television.'
		XERR = .TRUE.
		RETURN
	END IF

C       Look for the STAR= option.  This displays the selected stars on the
C       TV, then exits immediately.

	IDEL = 4
	CLR = .FALSE.
	BOXCOLOR = 0
	CIRC = .FALSE.
        CROSS = .FALSE.
        HAVEFOCUS = .FALSE.
	DO 8701 I=1,NCON
		TWORD = WORD(I)
                L = UPPER(TWORD)
		IF (TWORD(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),FDEL,PARM)
                        IF (XERR) RETURN
			IDEL = NINT(FDEL)
		ELSE IF (TWORD(1:5) .EQ. 'CIRC=') THEN
			CALL ASSIGN(WORD(I),FDEL,PARM)
                        IF (XERR) RETURN
			IDEL = NINT(FDEL)
			CIRC = .TRUE.
		ELSE IF (TWORD(1:5) .EQ. 'CROSS') THEN
			CROSS = .TRUE.
		ELSE IF (TWORD(1:6) .EQ. 'COLOR=') THEN
			CALL ASSIGN(WORD(I),FDEL,PARM)
                        IF (XERR) RETURN
			BOXCOLOR = NINT(FDEL)
		ELSE IF (TWORD .EQ. 'CLR') THEN
			CLR = .TRUE.
		END IF
8701    CONTINUE

	SELECTSTARS = .FALSE.
	DO 8702 I=1,NCON
		TWORD = WORD(I)
                L = UPPER(TWORD)
		IF (TWORD(1:5) .EQ. 'STAR=') THEN
			SELECTSTARS = .TRUE.
			CALL ASSIGNV(WORD(I),100,TEMPLIST,NF,PARM)
			IF (XERR) RETURN

			IF (NSTARS .EQ. 0) THEN
				PRINT *,'There is no photometry file.'
				XERR = .TRUE.
				RETURN
			END IF

			DO 8703 J=1,NF
				ISTAR = INT(TEMPLIST(J))

C                         Get DAOPHOT star numbers if we have them
				IF (DAONUMS(1) .NE. 0) THEN
				  DO 8704 II=1,NSTARS
				    IF (DAONUMS(II) .EQ. ISTAR) THEN
				      ISTAR = II
				      GOTO 533
				    END IF
8704                              CONTINUE
				  IF (ISTAR .NE. 0)
     &                              PRINT *, 'Star ', ISTAR,' is not ',
     &                                       'on the list.'
				  ISTAR = 0
				ELSE
C                                       Not DAOPHOT numbers
				  IF (ISTAR .LE. 0 .OR. ISTAR .GT.
     &                                  NSTARS) THEN
					IF (ISTAR .NE. 0)
     &                                     PRINT *,'Star ',ISTAR,
     &                                         'is not on the list.'
				  ELSE
533                                     JMIN = NINT(ROWS(ISTAR) - IDEL)
					JMAX = NINT(ROWS(ISTAR) + IDEL)
					IMIN = NINT(COLS(ISTAR) - IDEL)
					IMAX = NINT(COLS(ISTAR) + IDEL)
#ifdef __AED512
					CALL AEDBOX(IMIN,IMAX,JMIN,
     &                                        JMAX)
#endif
#ifdef __AED1024
					CALL QBOX(IMIN,IMAX,JMIN,JMAX)
#endif
#if defined(__SUNVIEW) || defined(__X11)
				IF (CIRC) THEN
				  CALL TVCIRC(ROWS(ISTAR),COLS(ISTAR),
     &                              FLOAT(IDEL),0.0,0.0,BOXCOLOR)
				ELSE IF (CROSS) THEN
				  CALL TVCROSS(NINT(ROWS(ISTAR)),
     &                              NINT(COLS(ISTAR)))
				ELSE
				  CALL TVBOX(IMIN,IMAX,JMIN,JMAX,BOXCOLOR)
				END IF
#endif
#ifdef __PER
					IF (CLR) THEN
					  IF (IEXP .GT. 1) THEN
					    MINRO = (IMAGEER - JMAX) *
     &                                            IEXP + IYLO + IEXP/2
					    MINCO = (IMIN - IMAGESC) *
     &                                            IEXP + IXLO + IEXP/2
					    NN = (2*IDEL+1) * IEXP
					  ELSE
					    MINRO = (IMAGEER - JMAX) /
     &                                            ICOMP + IYLO
					    MINCO = (IMIN - IMAGESC) /
     &                                            ICOMP + IXLO
					    NN = (2*IDEL+1) / ICOMP
					  END IF
					  CALL FCLROVR(MINCO-1,MINRO-1,
     &                                          NN+2, NN+2)
					ELSE
					  IF (JMIN .GE. IRTV .AND.
     &                                        IMIN .GE. ICTV .AND.
     &                                        JMAX .LT. IRTV+NRTV .AND.
     &                                        IMAX .LT. ICTV+NCTV )
     &                                          CALL PERBOX(IMIN,IMAX,
     &                                                JMIN,JMAX)
					END IF
#endif
				  END IF
				END IF
8703                    CONTINUE
		END IF
8702    CONTINUE

	IF (SELECTSTARS) RETURN

C       Look for options.  We append to the current list if the word NEW
C       is not present.  We do automatic fitting if the AUTO word is present.
C       If the AUTO word is present, we run in APPEND mode.

	APPEND     = .TRUE.
	DOBOX     = .TRUE.
	AUTO       = .FALSE.
	RADIUSSET = .FALSE.

	HAVERSHIFT = .FALSE.
	HAVECSHIFT = .FALSE.

	ROTATE = .FALSE.
	QUICK = .FALSE.
	QSAVE = .FALSE.
	EXIT = .FALSE.
	COMPLETE = .FALSE.
	FIRSTRUN = .FALSE.
	REFERENCE = .FALSE.
	REFPOS = .FALSE.
	OP = .FALSE.
	BLINK = .FALSE.
	ON = .TRUE.
	DISABLE = .FALSE.
	COLORON = .FALSE.
	FMOMENT = 0.
	DMIN = 1.0
	AR = 0.0
	AC = 0.0
	ANGLE = 0.0
	ID = 1
	MULT = .FALSE.
	OBSNUM = .FALSE.
	IBCN = 0
	DR = 0.0
	DC = 0.0
        SIGLIM = 3.
	RSIZE = 5
	CSIZE = 5
        SIG = 5
        PERC = 0.5
        SEARCH = 5
        SURGICAL = .FALSE.
        HAVEZAPFILE = .FALSE.
        ZAPAPPEND = .FALSE.
        MASK = .FALSE.
	DO 8705 I=1,NCON
		TWORD = WORD(I)
                L = UPPER(TWORD)
		IF (TWORD .EQ. 'NEW' ) THEN
			APPEND = .FALSE.

		ELSE IF (TWORD .EQ. 'AUTO') THEN
			AUTO   = .TRUE.

		ELSE IF (TWORD .EQ. 'NOBOX') THEN
			DOBOX = .FALSE.

		ELSE IF (TWORD .EQ. 'REF') THEN
			REFERENCE = .TRUE.

		ELSE IF (TWORD .EQ. 'REFPOS') THEN
			REFPOS = .TRUE.
			OP = .TRUE.
                        OPEN(61,FILE='ref.pos',status='unknown',iostat=ierr)
	                if (ierr .ne. 0) then
                           print *, 'Error opening ref.pos file'
                           XERR = .TRUE.
                           RETURN
                        end if

		ELSE IF (TWORD(1:3) .EQ. 'DR=') THEN
			CALL ASSIGN(WORD(I),DR,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:3) .EQ. 'DC=') THEN
			CALL ASSIGN(WORD(I),DC,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:7) .EQ. 'SIGLIM=') THEN
			CALL ASSIGN(WORD(I),SIGLIM,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:3) .EQ. 'ID=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			ID = NINT(TEMP)

		ELSE IF (TWORD .EQ. 'MULT') THEN
			MULT = .TRUE.

		ELSE IF (TWORD .EQ. 'OBSNUM') THEN
			OBSNUM = .TRUE.

		ELSE IF (TWORD(1:7) .EQ. 'RADIUS=') THEN
			RADIUSSET = .TRUE.
			CALL ASSIGN(WORD(I), TEMP, PARM)
			IF (XERR) RETURN
			ISIZE = INT(TEMP)
			IF (ISIZE .LT. 1) ISIZE=1

		ELSE IF (TWORD(1:7) .EQ. 'RSHIFT=') THEN
			CALL ASSIGN(WORD(I),RSHIFT,PARM)
			IF (XERR) RETURN
			HAVERSHIFT = .TRUE.

		ELSE IF (TWORD(1:7) .EQ. 'CSHIFT=') THEN
			CALL ASSIGN(WORD(I),CSHIFT,PARM)
			IF (XERR) RETURN
			HAVECSHIFT = .TRUE.

		ELSE IF (TWORD .EQ. 'EXIT') THEN
			EXIT = .TRUE.

		ELSE IF (TWORD(1:6) .EQ. 'ANGLE=') THEN
			CALL ASSIGN(WORD(I),ANGLE,PARM)
			IF (XERR) RETURN
			ROTATE = .TRUE.
			AUTO = .TRUE.

		ELSE IF (TWORD .EQ. 'QUICK') THEN
			QUICK = .TRUE.

		ELSE IF (TWORD .EQ. 'QSAVE') THEN
			QSAVE = .TRUE.
			QUICK = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'AR=') THEN
			CALL ASSIGN(WORD(I),AR,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:3) .EQ. 'AC=') THEN
			CALL ASSIGN(WORD(I),AC,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:5) .EQ. 'DMIN=') THEN
			CALL ASSIGN(WORD(I),DMIN,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:7) .EQ. 'MOMENT=') THEN
			CALL ASSIGN(WORD(I),FMOMENT,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD .EQ. 'COMPLETE') THEN
			COMPLETE = .TRUE.

                ELSE IF (TWORD(1:6) .EQ. 'FOCUS=') THEN
                        CALL ASSIGNV(WORD(I),2,FOC,NF,PARM)
                        IF (XERR) RETURN
                        HAVEFOCUS = .TRUE.

                ELSE IF (TWORD(1:5) .EQ. 'SIZE=') THEN
                        CALL ASSIGNV(WORD(I),2,TEMPLIST,NF,PARM)
                        IF (XERR) RETURN
                        IF (NF .EQ. 1) THEN
                           RSIZE = 2*INT(TEMPLIST(1)/2.) + 1
                           CSIZE = RSIZE
                        ELSE
                           RSIZE = 2*INT(TEMPLIST(1)/2.) + 1
                           CSIZE = 2*INT(TEMPLIST(2)/2.) + 1
                        END IF

                ELSE IF (TWORD(1:7) .EQ. 'SEARCH=') THEN
                   CALL ASSIGN(WORD(I),TEMPLIST,PARM)
                   SEARCH = TEMPLIST(1)
                   IF (SEARCH .GT. 50) THEN
                      PRINT *,'A SEARCH value of 50 is ',
     &                'the maximum allowed. SEARCH has been truncated to 50'
                      SEARCH = 50
                   END IF

                ELSE IF (TWORD(1:4) .EQ. 'SIG=') THEN
                   CALL ASSIGN(WORD(I),SIG,PARM)
                   IF (XERR) RETURN
                   IF (SIG .LT. 0.) THEN
                      PRINT *,'Negative zapping threshold specified'
                      PRINT *,'Aborting...'
                      XERR = .TRUE.
                      RETURN
                   END IF

                ELSE IF (TWORD .EQ. 'SURGICAL') THEN
                   SURGICAL = .TRUE.

                ELSE IF (TWORD(1:8) .EQ. 'ZAPFILE=') THEN
                   HAVEZAPFILE = .TRUE.
                   ZAPFILE = WORD(I)(9:)

                ELSE IF (TWORD .EQ. 'APPEND') THEN
                   ZAPAPPEND = .TRUE.

                ELSE IF (TWORD .EQ. 'MASK') THEN
                   MASK = .TRUE.

		END IF

8705    CONTINUE

C       Exit if the user gave any keywords we do not understand.
	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

	IF (HAVEZAPFILE) THEN
          L = NUMCHAR(ZAPFILE)
          IF (ZAPAPPEND) THEN
            OPEN(61,FILE=ZAPFILE(1:L),STATUS='unknown',ACCESS='append',
     &           IOSTAT=IERR)
          ELSE
            OPEN(61,FILE=ZAPFILE(1:L),STATUS='unknown',IOSTAT=IERR)
          END IF
          IF (IERR .NE. 0) THEN
            PRINT *, 'Error opening ZAPFILE: ', ZAPFILE
            XERR = .TRUE.
            RETURN
          END IF
        END IF


	IF (CLR) THEN
#ifdef __PER
		CALL FCLROVR(IXLO,IYLO,2*COL2,2*ROW2)
		RETURN
#else
		PRINT *, 'CLR not enabled on this device yet '
		XERR = .TRUE.
		RETURN
#endif
	END IF

	IF (AUTO)             APPEND = .TRUE.
	IF (.NOT. RADIUSSET) ISIZE = 3

	IF (COMPLETE) THEN
		IF (.NOT. AUTO) THEN
		  PRINT *, 'COMPLETE is meaningless unless AUTO ',
     &                     'is specified'
		  XERR = .TRUE.
		  RETURN
		END IF
		FIRSTRUN = .TRUE.
		IF (QUICK) THEN
		  PRINT *, 'For QUICK, COMPLETE isnt meaningful'
		  COMPLETE = .FALSE.
		END IF
	END IF

	IF (ROTATE) THEN
		IF (AR .EQ. 0. .OR. AC .EQ. 0.) THEN
		       CALL ASKDATA('Enter AR, AC:',TEMPLIST,2)
		       AR = TEMPLIST(1)
		       AC = TEMPLIST(2)
		END IF
	END IF

C       If we are making a new list, do so.

	IF (.NOT. APPEND) THEN
		IF (NSTARS .GT. 0) CALL CLOSEPHOT

		CALL OPENPHOT
		NSTARS = 0
	ELSE
		IF (NSTARS .EQ. 0) CALL OPENPHOT
	END IF

	IF (COM .EQ. 'ITV') GOTO 95

C       Draw the positions of the current stars on the list if
C       we are appending to the current list and are not in the AUTO mode.

	IF ((.NOT. AUTO) .AND. (APPEND)) THEN

C               Turn off the cursor.
#ifdef __AED512
		CALL AE1BYTE(AED_DJC_INST)
		CALL AEFLUSH(0)
#endif
#ifdef __AED1024
		CALL QCURSOFF
#endif

C               Draw boxes around the stars on the input list.

		IF (DOBOX) THEN
			DO 8706 K=1,NSTARS
				IF (NOGO) RETURN

				CALL GETPHOTREC(PHOTLABEL,PHOTDATA,K)
				IF (XERR) RETURN

				ROW = NINT(PHOTDATA(ROWLOC) + DR)
				COL = NINT(PHOTDATA(COLLOC) + DC)

				MINROW    = ROW - IDEL
				MAXROW    = ROW + IDEL
				MINCOL    = COL - IDEL
				MAXCOL    = COL + IDEL
#ifdef __AED512
				CALL AEDBOX(MINCOL,MAXCOL,MINROW,
     &                                MAXROW)
#endif
#ifdef __AED1024
				CALL QBOX(MINCOL,MAXCOL,MINROW,MAXROW)
#endif
#if defined(__SUNVIEW) || defined(__X11)
                                IF (ROW .GT. IMAGESR .AND. 
     &                              ROW .LT. IMAGEER .AND.
     &                              COL .GT. IMAGESC .AND.
     &                              COL .LT. IMAGEEC) THEN
			          IF (CIRC) THEN
			            CALL TVCIRC(FLOAT(ROW),FLOAT(COL),
     &                                          FLOAT(IDEL),0.0,0.0,BOXCOLOR)
			          ELSE IF (CROSS) THEN
			            CALL TVCROSS(ROW,COL)
		          	  ELSE
			            CALL TVBOX(MINCOL,MAXCOL,MINROW,MAXROW,
     &                                         BOXCOLOR)
			          END IF
                                END IF
#endif
#ifdef __PER
				IF (MINROW .GE. IRTV .AND.
     &                              MINCOL .GE. ICTV .AND.
     &                              MAXROW .LT. IRTV+NRTV .AND.
     &                              MAXCOL .LT. ICTV+NCTV )
     &                                  CALL PERBOX(MINCOL,MAXCOL,
     &                                           MINROW,MAXROW)
#endif

8706                    CONTINUE
		END IF
	END IF


C       Miscellaneous initializations.

	DATHEAD = .FALSE.

	IF (REFERENCE) THEN
	   IF (IBUF(1) .LE. 0) THEN
	      PRINT *, ' You must specify a buffer number with ',
     &                 '   the REF keyword '
              XERR = .TRUE.
              RETURN
	   ELSE
	      CALL CCUNFIT('NRSTAR',HEADBUF(1,IBUF(1)))
	      FITSCARD(1:8) = 'RSTARX  '
	      DO 8543 I = 1, 9
		WRITE(FITSCARD(7:7),'(I1)') I
		CALL CCUNFIT(FITSCARD,HEADBUF(1,IBUF(1)))
 8543         CONTINUE
	      FITSCARD(6:6) = 'Y'
	      DO 8544 I = 1, 9
		WRITE(FITSCARD(7:7),'(I1)') I
		CALL CCUNFIT(FITSCARD,HEADBUF(1,IBUF(1)))
 8544         CONTINUE
	   END IF
	   NREF = 0
	END IF

C       Compute parameters of the observation using the header.
C       These are stored in the common block defined in 'TIMEDATA.INC'
	CALL CCGETHJD(HEADBUF(1,IMTV),XERR)
        IF (XERR) RETURN

C       If we are in the AUTO mode, use the current list as pointers to
C       the new stars.

	IF (AUTO) THEN

C               Initialize variables for computing average coordinate
C               shift.

127             SUMDR = 0.0
		SUMDC = 0.0
		SUMDR2 = 0.0
		SUMDC2 = 0.0
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

C               Turn off the cursor.
#ifdef __AED512
		CALL AE1BYTE(AED_DJC_INST)
		CALL AEFLUSH(0)
#endif
#ifdef __AED1024
		CALL QCURSOFF
#endif
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

			COLSTART = AC + (COLSTART-AC)*COS(ANGLE)
     &                                 + (ROWSTART-AR)*SIN(ANGLE)
			ROWSTART = AR + (ROWSTART-AR)*COS(ANGLE)
     &                                 - (COLSTART-AC)*SIN(ANGLE)

			XCAX     = COLSTART
			YCAX     = ROWSTART

			IF (QSAVE) GOTO 444
			IF (QUICK) GOTO 555

			ROW       = NINT(ROWSTART)
			COL       = NINT(COLSTART)

C                       Clear out the record for the star, but do not
C                       touch the star`s name.

			DO 8709 I=1,NUMDATA
				PHOTDATA(I) = 0.0
8709                    CONTINUE

C                       If the star is not too close to the
C                       boundary, look for the star.

			IF (ROW .LT. (IMAGESR + ISIZE) .OR.
     &                      ROW .GT. (IMAGEER - ISIZE) .OR.
     &                      COL .LT. (IMAGESC + ISIZE) .OR.
     &                      COL .GT. (IMAGEEC - ISIZE)) THEN

				IF (COMPLETE) THEN
				  OK = .FALSE.
				  GOTO 91
				END IF

				PRINT *,'Star ',K,' is outside ',
     &                      'the image or too close to the edge.'
				GOTO 90
			ELSE
				MAXITER = 6
				OK = FINDCENT(A,IMAGESR,IMAGEER,
     &                                   IMAGESC,IMAGEEC,
     &                                   XCAX,YCAX,ISIZE,MAXITER,MASK)
			END IF

C                       If the star could not be found, do not include
C                       it in the new list.

91                      IF (.NOT. OK) THEN
				IF (FIRSTRUN .OR. .NOT. COMPLETE) GOTO 90
			END IF

C                       The position of the star has been found.  Check
C                       that the position did not converge to that of another
C                       star on the list.

			IF (ISONLIST(XCAX,YCAX,K,DMIN)) THEN
			   IF (FIRSTRUN) THEN
				PRINT *,'Position for star ',K
				PRINT *,'converged to that of ',
     &                                  'another star.'
				GO TO 90
			   END IF
			   XCAX = COLSTART
			   YCAX = ROWSTART
			END IF

C                       Check that the star did not stray too far from
C                       the original position.

			DELTAR(NDELT+1) = YCAX - ROWSTART
			DELTAC(NDELT+1) = XCAX - COLSTART

			IF (HAVERSHIFT) THEN
				IF (ABS(DELTAR(NDELT+1)) .GT. RSHIFT) THEN
					WRITE(olu,*,ERR=9999)
     &                                   'Star ',K,' can''t be found.'
					IF (.NOT. COMPLETE .OR. FIRSTRUN) THEN
						GO TO 90
					ELSE
						XCAX = COLSTART
						YCAX = ROWSTART
					END IF
				END IF
			END IF

			IF (HAVECSHIFT) THEN
				IF (ABS(DELTAC(NDELT+1)) .GT. CSHIFT) THEN
					WRITE(olu,*,ERR=9999)
     &                                   'Star ',K,' can''t be found.'
					IF (.NOT. COMPLETE .OR. FIRSTRUN) THEN
						GO TO 90
					ELSE
						XCAX = COLSTART
						YCAX = ROWSTART
					END IF
				END IF
			END IF

			IF (FIRSTRUN .AND. COMPLETE) GOTO 27
C                       The star has been found.  Load the information
C                       about it to the current record.

444                     CALL GETPHOTREC(PHOTLABEL,PHOTDATA,K)
			IF (XERR) RETURN

			PHOTDATA(ROWLOC)        = YCAX
			PHOTDATA(COLLOC)        = XCAX
			CALL CCFHEAD('EXPOSURE',HEADBUF(1,IMTV),DVAL)
			PHOTDATA(EXPOSURETIME) = SNGL(DVAL)
			CALL CCFHEAD('FOCUS',HEADBUF(1,IMTV),DVAL)
			PHOTDATA(FOCUS) = SNGL(DVAL)
			CALL CCFHEAD('GAIN',HEADBUF(1,IMTV),DVAL)
			PHOTDATA(PHOTGAIN) = SNGL(DVAL)
			IF (DVAL .EQ. 0) PHOTDATA(PHOTGAIN) = 1.
			CALL CCFHEAD('RONOISE',HEADBUF(1,IMTV),DVAL)
			PHOTDATA(PHOTRN) = SNGL(DVAL)
			PHOTDATA(FRAMERA) = RA
			PHOTDATA(FRAMEDEC) = DEC
			PHOTDATA(HOURANGLE) = HA
			IF (HJD .GT. 0.5) THEN
			  PHOTDATA(JULIAN) = SNGL(HJD - 2444000.D0)
			  PHOTDATA(AIRMASS) = SNGL(AM)
			ELSE
			  PHOTDATA(JULIAN) = 0.
			  PHOTDATA(AIRMASS) = 0.
			END IF
			PHOTDATA(UTMONTH)        = MONTH
			PHOTDATA(UTDAY)          = DAY
			PHOTDATA(UTYEAR)         = YEAR
			PHOTDATA(UTEXPOS)        = UT * 3600.
			IF (OBSNUM) THEN
			 CALL CCINHEAD('OBSNUM',HEADBUF(1,IMTV),JOBS)
			 IOLD = PHOTDATA(NALTER)
			 IF (IOLD .GT. 1000) IOLD = IOLD/1000
			 PHOTDATA(NALTER)       = IOLD*1000 + MOD(JOBS,1000)
			END IF

			CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,K)
			IF (XERR) RETURN

			IF (REFERENCE) THEN
			   NREF = NREF + 1
			   CALL CCINHEADSET('NRSTAR',NREF,
     &                            HEADBUF(1,IBUF(1)))
			   FITSCARD(1:8) = 'RSTARX  '
			   WRITE(FITSCARD(7:7),'(I1)') NREF
			   CALL CCFHEADSET(FITSCARD,
     &                            DBLE(XCAX),HEADBUF(1,IBUF(1)))
			   FITSCARD(6:6)= 'Y'
			   CALL CCFHEADSET(FITSCARD,
     &                            DBLE(YCAX),HEADBUF(1,IBUF(1)))
			END IF

			WASFOUND(K) = .TRUE.

C                       Draw a box around the current star on the frame.

555                     COL    = NINT(XCAX)
			ROW    = NINT(YCAX)
			MINCOL = COL - IDEL
			MAXCOL = COL + IDEL
			MINROW = ROW - IDEL
			MAXROW = ROW + IDEL
			IF (DOBOX) THEN
#ifdef __AED512
				CALL AEDBOX(MINCOL,MAXCOL,MINROW,
     &                                MAXROW)
#endif
#ifdef __AED1024
				CALL QBOX(MINCOL,MAXCOL,MINROW,MAXROW)
#endif
#if defined(__SUNVIEW) || defined(__X11)
			IF (CIRC) THEN
			  CALL TVCIRC(YCAX,XCAX,FLOAT(IDEL),0.0,0.0,BOXCOLOR)
			ELSE IF (CROSS) THEN
			  CALL TVCROSS(NINT(YCAX),NINT(XCAX))
			ELSE
			  CALL TVBOX(MINCOL,MAXCOL,MINROW,MAXROW,BOXCOLOR)
			END IF
#endif
#ifdef __PER
				IF (MINROW .GE. IRTV .AND.
     &                              MINCOL .GE. ICTV .AND.
     &                              MAXROW .LE. IRTV+NRTV .AND.
     &                              MAXCOL .LE. ICTV+NCTV)
     &                                  CALL PERBOX(MINCOL,MAXCOL,
     &                                         MINROW,MAXROW)
#endif
			END IF

			WRITE(olu,901,ERR=9999) K, DELTAR(NDELT+1), DELTAC(NDELT+1)
 901                    FORMAT (' Star ',I6,' DR= ',F9.2,'    DC= ',
     &                            F9.2)

C                       Accumulate information for average shift.

27                      SUMDR  = SUMDR + DELTAR(NDELT+1)
			SUMDC  = SUMDC + DELTAC(NDELT+1)
                        SUMDR2  = SUMDR2 + DELTAR(NDELT+1)**2
			SUMDC2  = SUMDC2 + DELTAC(NDELT+1)**2
			NDELT  = NDELT + 1
 90                     CONTINUE
8708            CONTINUE

C               Print average shift.

		IF (NDELT .GT. 0) THEN
		    SUMDR = SUMDR / FLOAT(NDELT)
		    SUMDC = SUMDC / FLOAT(NDELT)
                    IF (NDELT .GT. 1) THEN
                      IF (SUMDR2 .GT. 0) THEN
	                SUMDR2 = SQRT((SUMDR2 - NDELT*SUMDR**2)/ FLOAT(NDELT-1))
                      ELSE
                        SUMDR2 = 0.
                      END IF
                      IF (SUMDC2 .GT. 0) THEN
		        SUMDC2 = SQRT((SUMDC2 - NDELT*SUMDC**2)/ FLOAT(NDELT-1))
                      ELSE
                        SUMDC2 = 0.
                      END IF
                    END IF

		    WRITE(olu,*,ERR=9999) ' '
		    WRITE(olu,902,ERR=9999) DR+SUMDR,SUMDR2,DC+SUMDC,SUMDC2
 902                FORMAT(3X,'Average DR = ',F9.2,' +/-', F9.3,4X,
     &                         'Average DC = ',F9.2,' +/-', F9.3)
		    PRINT *, 'Loaded into variables DR, DC '
		    CALL VARIABLE('DR',DR+SUMDR,.TRUE.)
		    CALL VARIABLE('DC',DC+SUMDC,.TRUE.)

                    DROW = SUMDR
                    DCOL = SUMDC
                    DROWS = SUMDR2
                    DCOLS = SUMDC2
                    N = NDELT
                    SUMDR = 0.
                    SUMDC = 0.
                    SUMDR2 = 0.
                    SUMDC2 = 0.
                    NDELT = 0
		    DO 5901 I = 1, N
                      IF (ABS(DELTAR(I)-DROW) .LT. SIGLIM*DROWS .AND.
     &                    ABS(DELTAC(I)-DCOL) .LT. SIGLIM*DCOLS) THEN
                        SUMDR = SUMDR + DELTAR(I)
                        SUMDC = SUMDC + DELTAC(I)
                        SUMDR2 = SUMDR2 + DELTAR(I)**2
                        SUMDC2 = SUMDC2 + DELTAC(I)**2
                        NDELT = NDELT + 1
                      END IF
5901                CONTINUE

                    IF (NDELT .GT. 0) THEN                    
		      SUMDR = SUMDR / FLOAT(NDELT)
		      SUMDC = SUMDC / FLOAT(NDELT)
                    END IF
                    IF (NDELT .GT. 1) THEN
                      IF (SUMDR2 .GT. 0) THEN	
	                SUMDR2 = SQRT((SUMDR2 - NDELT*SUMDR**2)/ FLOAT(NDELT-1))
                      ELSE
                        SUMDR2 = 0.
                      END IF
                      IF (SUMDC2 .GT. 0) THEN
		        SUMDC2 = SQRT((SUMDC2 - NDELT*SUMDC**2)/ FLOAT(NDELT-1))
                      ELSE
                        SUMDC2 = 0.
                      END IF
                    END IF

		    WRITE(olu,*,ERR=9999) ' '
		    WRITE(olu,903,ERR=9999) SIGLIM
 903                FORMAT('After',F9.2,' sigma rejection')
		    WRITE(olu,902,ERR=9999) DR+SUMDR,SUMDR2,DC+SUMDC,SUMDC2
		    PRINT *, 'Loaded into variables DR, DC '
		    CALL VARIABLE('DR',DR+SUMDR,.TRUE.)
		    CALL VARIABLE('DC',DC+SUMDC,.TRUE.)
		END IF

		IF (COMPLETE .AND. FIRSTRUN) THEN
			FIRSTRUN = .FALSE.
			IF ( .NOT. ROTATE) THEN
			  DR = DR + SUMDR
			  DC = DC + SUMDC
			END IF
			GOTO 127
		END IF

C               Now go through the list, compressing the list to include
C               only those stars that were found.

		NFOUND = 0
		DO 8710 I=1,NSTARS
		    IF (WASFOUND(I)) THEN
			  NFOUND = NFOUND + 1
			  CALL GETPHOTREC(PHOTLABEL,PHOTDATA,I)
			  IF (XERR) RETURN
			  CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,NFOUND)
			  IF (XERR) RETURN
		    END IF
8710            CONTINUE
		IF (NSTARS .NE. NFOUND) THEN
		  PRINT *, 
     &             'Missed at least one star for some reason.'
		  PRINT *, 
     &             'If completeness is desired, rerun with '
		  PRINT *, 
     &             ' COMPLETE or check edges. Remember to reread '
		  PRINT *, ' in original photometry file '
		  PRINT *
		END IF
		NSTARS = NFOUND
	END IF

	IF (EXIT) THEN
          CALL VARIABLE('NSTARS',FLOAT(NSTARS),.TRUE.)
          RETURN
        END IF

C       INTERACTIVE ENTRY OF STARS:
C       Initialize constants for interactive use of TV
  95     CONTINUE
#ifdef __AED512
	COL2 = NCTV / (2 * ICOMP)
	ROW2 = NRTV / (2 * ICOMP)
	ISTAT = SYS$CANCEL(%VAL(TTCHAN))        ! Cancel pending IO
	CALL AECURSON                           ! Turn on cursor
	CALL AEDION                             ! Reset to expect characters
	ON = .TRUE.
	CALL TVCHAR(KEY)                        ! Clear key
 100    CALL VISTAWAIT(100)
C       When key is hit, get key character.
	CALL TVCHAR(KEY)
	L = UPPER(KEY)
#endif
#ifdef __AED1024
	COL2 = NCTV / (2 * ICOMP)
	ROW2 = NRTV / (2 * ICOMP)
	CALL SETAED ('PAR')
	CALL SETAED ('PIN')
	CALL QSIF ('P')
	CALL QCURSON
	ON = .TRUE.
 100    CALL VISTAWAIT(100)
	IF (KSR(IK) .EQ. 1) THEN
		CALL SKS (BKEY)
		KEY = CHAR(BKEY)
		L = UPPER(KEY)
	ELSE
		KEY = CHAR(0)
	END IF
#endif
#if defined(__SUNVIEW) || defined(__X11)
	OLDKEY = ' '
 100    ISTAT = MARK(ROW,COL,KEY)
	IF (OLDKEY .EQ. '[') KEY = ' '
	OLDKEY = KEY
	L = UPPER(KEY)
	IX = COL
	IY = ROW
#endif
#ifdef __PER
	ON = .TRUE.
	IF (IEXP .GT. 1) THEN
		IBLOCK = -1 * IEXP
	ELSE
		IBLOCK = ICOMP
	END IF
100     IF (IBLOCK .LT. 1) THEN
	       ISEND = IBLOCK * 2**(IZ-1)
	ELSE IF (IBLOCK .EQ. 1) THEN
	       ISEND = - IBLOCK * 2**(IZ-1)
	ELSE IF (IBLOCK .EQ. 2) THEN
	       IF (IZ .EQ. 1) ISEND = 2
	       IF (IZ .EQ. 2) ISEND = 1
	       IF (IZ .GT. 2) ISEND = -1 * 2**(IZ-2)
	ELSE
	       PRINT *, 'Can''t use ITV with such a compressed image'
	END IF
	CALL FSETCURXFORM(ISC,ISR,IXLO,IYLO,ISEND)
	CALL SETUPMARKCUR
	CALL FCURKEY(0,LASTX,LASTY,1,0,0,DUMMY)

	ILAST = INT(LASTX(1) / 1000.)
	LASTX(1) = LASTX(1) - ILAST*1000
	IF (ILAST .EQ. 1) THEN
		KEY = 'E'
		KEY = 'F'
	ELSE IF (ILAST .EQ. 2) THEN
		KEY = 'I'
	ELSE IF (ILAST .EQ. 3) THEN
		KEY = 'O'
	ELSE IF (ILAST .EQ. 4) THEN
		KEY = 'P'
	ELSE IF (ILAST .EQ. 5) THEN
		KEY = 'R'
	ELSE IF (ILAST .EQ. 6) THEN
		KEY = 'C'
	ELSE IF (ILAST .EQ. 7) THEN
		KEY = 'J'
	ELSE IF (ILAST .EQ. 8) THEN
		KEY = 'M'
	ELSE IF (ILAST .EQ. 9) THEN
		KEY = 'D'
	ELSE IF (ILAST .EQ. 10) THEN
		KEY = 'B'
	ELSE IF (ILAST .EQ. 11) THEN
		KEY = '#'
	ELSE IF (ILAST .EQ. 12) THEN
		KEY = 'X'
	ELSE IF (ILAST .EQ. 13) THEN
		KEY = 'Y'
	ELSE IF (ILAST .EQ. 14) THEN
		KEY = 'U'
	ELSE IF (ILAST .EQ. 15) THEN
		KEY = 'V'
	ELSE IF (ILAST .EQ. 16) THEN
		KEY = 'W'
	ELSE IF (ILAST .EQ. 17) THEN
		KEY = 'Z'
	ELSE IF (ILAST .EQ. 18) THEN
		KEY = '!'
	ELSE
		KEY = 'F'
	END IF
#endif
C------------------------------------------------------------------------------

C       Execute the various commands.

	IF (KEY .EQ. 'E' .OR. KEY .EQ. 'F')THEN
C               Turn off cursor and return.
#ifdef __AED512
		CALL AE1BYTE(AED_DJC_INST)
		CALL AEFLUSH(0)
#endif
#ifdef __AED1024
		CALL QCURSOFF
#endif
#ifdef __PER
		IF (KEY .EQ. 'E') CALL FCLROVR(IXLO,IYLO,2*COL2,2*ROW2)
		IF (.NOT. ORIGINAL) THEN
		     CALL PZWRIT(OLD)
		     ISR = IRTV
		     ISC = ICTV
		     IZ = 1
		END IF
#endif
		ON = .FALSE.
		DISABLE = .FALSE.
		IF (OP) THEN
		   OP = .FALSE.
		   CLOSE(61)
		END IF
                IF (HAVEZAPFILE) CLOSE(61)
		IF (NSTARS .EQ. 0 .AND. COM .EQ. 'MARKSTAR') THEN
			PRINT *,'No stars have been marked.'
                        CALL VARIABLE('NSTARS',FLOAT(NSTARS),.TRUE.)
			RETURN
		END IF
                CALL VARIABLE('NSTARS',FLOAT(NSTARS),.TRUE.)
		RETURN

C   Need to reenable commands for AED after 'A' or 'B' commands
	ELSE IF (DISABLE .AND. KEY .NE. CHAR(0)) THEN
		DISABLE =.FALSE.
		COLORON =.FALSE.
		IF (BLINK) THEN
#ifdef __AED512
			CALL AEZOOM(0,256,256)
#endif
#ifdef __AED1024
			CALL QZOOM(0,NCOLSTV/2,NROWSTV/2)
#endif
			BLINK = .FALSE.
		END IF
#ifdef __AED512
		CALL AECURSON
#endif
#ifdef __AED1024
		CALL QCURSON
#endif

#ifndef __PER
	ELSE IF (KEY .EQ. 'H')THEN
C         Help
		CALL MARKHELP
#endif
C------------------------------------------------------------------------------

	ELSE IF (KEY .EQ. 'B') THEN
C  For BLINK option, we have different code for the Peritek/Sun than for the
C        AEDs, which do the blink internally
#ifdef __PER
		IF (BLINKSTAT) THEN
		   CALL BLINKVLT(11)
		ELSE
		   PRINT *, 'Must display 2 images with BLINK first'
		END IF
#endif
		IF (.NOT. BLINK)THEN
#ifdef __AED512
			CALL AECURSOFF
#endif
#ifdef __AED1024
			CALL QCURSOFF
#endif
C                 0=right 1=left
			IPIC    = 0
			BLINK   =.TRUE.
			DISABLE =.TRUE.
		END IF
#ifdef __AED512
		ISTAT   =SYS$CANCEL(%VAL(TTCHAN))
		CALL AEJOYPOS(IX,IY)
#endif
#ifdef __AED1024
C      Joystick position.
		 CALL QRJOY(IX,IY)
#endif
		IRATE   = 1
		IY = IABS(IY-256)
		IF(IY.GT. 100) THEN
			IRATE = 50.0*(206.0/FLOAT(IY-50))
			IF(IPIC .EQ. 0) THEN
				IPIC = 1
#ifdef __AED512
				CALL AEZOOM(IBLZ,IXRIGHT,IYRIGHT)
#endif
#ifdef __AED1024
				CALL QZOOM(IBLZ,IXRIGHT,IYRIGHT)
#endif
			ELSE
				IPIC = 0
#ifdef __AED512
				CALL AEZOOM(IBLZ,IXLEFT,IYLEFT)
#endif
#ifdef __AED1024
				CALL QZOOM(IBLZ,IXLEFT,IYLEFT)
#endif
			END IF
#ifdef __AED512
		ELSE IF (IX .GT. 400) THEN
			CALL AEZOOM(IBLZ,IXRIGHT,IYRIGHT)
		ELSE IF (IX .LT. 100) THEN
			CALL AEZOOM(IBLZ,IXLEFT,IYLEFT)
#endif
		END IF
#ifdef __AED512
		CALL AEDION
#endif
		CALL VISTAWAIT(IRATE)

C       Use joystick position to change the zero point of the color map
	ELSE IF (KEY .EQ. 'A' .OR. COLORON) THEN
#ifdef __AED512
		IF (.NOT. COLORON) CALL AECURSOFF
#endif
#ifdef __AED1024
		IF (.NOT. COLORON) CALL QCURSOFF
#endif
		COLORON =.TRUE.
		DISABLE =.TRUE.
#ifdef __AED512
		ISTAT   =SYS$CANCEL(%VAL(TTCHAN))
		CALL AEJOYPOS(IX,IY)
		IY      =IY-256
#endif
#ifdef __AED1024
		CALL QRJOY(IX,IY)
		IY = IY - NROWSTV/2
#endif
C    Break 'Y' value into multiples of 32 away from center
#if defined(__AED512) || defined(__AED1024)

C    Move color zero point up or down 2**IY units during each pass through here
		IF (IY .GE. 0) THEN
			ISIGN   =1
		ELSE
			ISIGN   =-1
		END IF
		IY      =IABS(IY-1)/32-2
C     Limit to 2**5 (=32)
		IF (IY .GT. 5) IY=5
C     Roll up
		IF (IY .GE. 0 .AND. ISIGN .LT. 0) THEN
			IUP     =2**IY
			DO 9703 J=1,3
				DO 9704 I=1,IUP
				  COLRBUF(I) = COLOR(255-IUP+I,J)
9704                            CONTINUE
				DO 9705 I=1, 255-IUP
				  COLOR(256-I,J) = COLOR(256-IUP-I,J)
9705                            CONTINUE
				DO 9706 I=1,IUP
				  COLOR(I,J) = COLRBUF(I)
9706                            CONTINUE
9703                    CONTINUE
C     Roll down
		ELSE IF (IY .GE. 0 .AND. ISIGN .GT. 0) THEN
			IUP     =2**IY
			DO 9707 J=1,3
				DO 9708 I=1,IUP
				    COLRBUF(I)      =COLOR(I,J)
9708                            CONTINUE
				DO 9709 I=IUP+1,255
				    COLOR(I-IUP,J)  =COLOR(I,J)
9709                            CONTINUE
				DO 9710 I=1,IUP
				    COLOR(255-IUP+I,J)=COLRBUF(I)
9710                            CONTINUE
9707                    CONTINUE
		END IF
#else
		PRINT *, ' You can''t roll the color map this way ',
     &                   'on this device'
#endif
C       Transmit new color map.
		IF (IY .GE. 0) THEN
#ifdef __AED512
		    CALL AECOLOR(1,0,256,COLOR(1,1),COLOR(1,2),
     &                           COLOR(1,3))
#endif
#ifdef __AED1024
		    CALL QCOLOR(1,0,NNCOLOR,COLOR(1,1),COLOR(1,2),
     &                           COLOR(1,3))
#endif
		END IF
#ifdef __AED512
C    Reset to expect characters
		CALL AEDION
#endif

#ifdef __PER
	ELSE IF (KEY .EQ. 'R') THEN
		IF (UNREAD) THEN
		    CALL PZREAD(OLD)
		    UNREAD = .FALSE.
		END IF
		CALL PZWRIT(OLD)
		ORIGINAL = .TRUE.
		ISR = IRTV
		ISC = ICTV
		IZ = 1
#endif

        ELSE IF (KEY .EQ. 'S') THEN
                ID = ID + 1

        ELSE IF (KEY .EQ. 'Q') THEN
                NSTARS = 0

C  Get the cursor position for all options which require it.
	ELSE IF (KEY .EQ. 'I' .OR. KEY .EQ. 'O' .OR. KEY .EQ. 'P' .OR.
     &           KEY .EQ. 'C' .OR. KEY .EQ. 'J' .OR. KEY .EQ. 'W' .OR.
     &           KEY .EQ. 'D' .OR. KEY .EQ. '#' .OR. KEY .EQ. 'X' .OR.
     &           KEY .EQ. 'Y' .OR. KEY .EQ. 'U' .OR. KEY .EQ. 'V' .OR.
     &           KEY .EQ. '!' .OR. KEY .EQ. 'N' .OR.
#if defined(__SUNVIEW) || defined(__AED512) || defined(__AED1024)
     &           (KEY .GE. '0' .AND. KEY .LE. '9') .OR.
#endif
     &           KEY .EQ. 'Z') THEN

C               Note that the Peritek and the SUN
C                routines return ARRAY coordinates, while the AED routines
C                return SCREEN(TV) coordinates.
#ifdef __AED512
		ISTAT = SYS$CANCEL(%VAL(TTCHAN))! Cancel pending IO
		CALL AECURSPOS(IX,IY)           ! Get cursor position
		CALL AEDION
#endif
#ifdef __AED1024
		CALL QRCURS(IX,IY)
		IF (MOD(NRTV,2) .EQ. 0) IY = IY - 1
#endif
#ifdef __PER
C       Peritek row value needs to be flipped.
		IX = LASTX(1)
		IY = ISR + NRTV/2**(IZ-1) - 1 - LASTY(1) + ISR
#endif
#if defined(__PER) || defined(__SUNVIEW) || defined(__X11)
		IF (IX .GE. IMAGESC .AND. IX .LE. IMAGEEC .AND.
     &              IY .GE. IMAGESR .AND. IY .LE. IMAGEER) THEN
			ROW = IY
			COL = IX
			QVAL =  A(COL,ROW)
#endif

#if defined(__AED512) || defined(__AED1024)
		IF (IX .GE. IXLO .AND. IX .LE. IXHI .AND.
     &              IY .GE. IYLO .AND. IY .LE. IYHI) THEN
			ROW     = IRTV + (IY - IYLO) * ICOMP
			COL     = ICTV + (IX - IXLO) * ICOMP
			QVAL    = A(COL,ROW)
#endif
C                       Print the position and values for the 'D' option.
			IF (KEY .EQ. 'D')  THEN
				IF (.NOT. DATHEAD) THEN
					PRINT 154
154                                     FORMAT (/3X,' Row ',3X,' Col ',6X,
     &                                      'Value')
					DATHEAD =.TRUE.
				END IF
				PRINT 155, ROW, COL, QVAL
155                             FORMAT (3X,I5,3X,I5,3X,1PE10.3)

C                       Print out array around cursor for MARKSTAR
				IF (COM .EQ. 'MARKSTAR' .AND.
     &                              COL-2 .GE. IMAGESC .AND.
     &                              ROW-2 .GE. IMAGESR .AND.
     &                              COL+2 .LE. IMAGEEC .AND.
     &                              ROW+2 .LE. IMAGEER) THEN

156                             FORMAT (5(1X,I8))
				PRINT 156, NINT(A(COL-2,ROW-2)),
     &                            NINT(A(COL-1,ROW-2)),
     &                            NINT(A(COL,ROW-2)),
     &                            NINT(A(COL+1,ROW-2)),
     &                            NINT(A(COL+2,ROW-2))
				PRINT 156, NINT(A(COL-2,ROW-1)),
     &                            NINT(A(COL-1,ROW-1)),
     &                            NINT(A(COL,ROW-1)),
     &                            NINT(A(COL+1,ROW-1)),
     &                            NINT(A(COL+2,ROW-1))
				PRINT 156, NINT(A(COL-2,ROW)),
     &                            NINT(A(COL-1,ROW)),NINT(A(COL,ROW)),
     &                            NINT(A(COL+1,ROW)),NINT(A(COL+2,ROW))
				PRINT 156, NINT(A(COL-2,ROW+1)),
     &                            NINT(A(COL-1,ROW+1)),
     &                            NINT(A(COL,ROW+1)),
     &                            NINT(A(COL+1,ROW+1)),
     &                            NINT(A(COL+2,ROW+1))
				PRINT 156, NINT(A(COL-2,ROW+2)),
     &                            NINT(A(COL-1,ROW+2)),
     &                            NINT(A(COL,ROW+2)),
     &                            NINT(A(COL+1,ROW+2)),
     &                            NINT(A(COL+2,ROW+2))

				END IF

C                       Store the current position in R and C for 'D' command
				PARM = 'R'
				CALL VARIABLE(PARM,FLOAT(ROW),.TRUE.)
				PARM = 'C'
				CALL VARIABLE(PARM,FLOAT(COL),.TRUE.)

C                       Print the location of the nearest star to this
C                       position in the '#' command.  If there is not
C                       a star within 10 pixels, say so.
			ELSE IF (KEY .EQ. '#' .OR. KEY .EQ. '!') THEN
			  YCAX = FLOAT(ROW)
			  XCAX = FLOAT(COL)
			  DIST  = 1E10

			  ISAVE = 0
			  DO 8711 I=1,NSTARS
			    DR = ABS(YCAX - ROWS(I))
			    IF (DR .LE. DIST) THEN
			      DC = ABS(XCAX - COLS(I))
			      IF (DC .LE. DIST) THEN
				DS = SQRT(DR**2 + DC**2)
				IF (DS .LT. DIST) THEN
				  DIST  = DS
				  CALL GETPHOTREC(PHOTLABEL,PHOTDATA,I)
				  IF (XERR) RETURN
				  INDEX = I
				  ISAVE = I
				  IF (PHOTDATA(19) .NE. 0)
     &                              ISAVE =NINT(PHOTDATA(19))
				END IF
			      END IF
			    END IF
8711                      CONTINUE
			  IF (ISAVE .NE. 0) THEN
			      IF (KEY .EQ. '!') THEN
				PHOTDATA(19) = -1
				CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,INDEX)
				PRINT *, 'DELETED: '
			      END IF
			      PRINT 1234,ISAVE,ROWS(INDEX),COLS(INDEX), 
     &                                STARINFO(INDEX)
1234                          FORMAT(3x,'Star number ',I6,
     &                             ' at row, column ',2(F7.1,3X),F10.3)
			      IF (REFPOS .AND. KEY .EQ. '#') THEN
			        XCAX = FLOAT(COL)
			        YCAX = FLOAT(ROW)
			        MAXITER = 6
			        OK =  FINDCENT(A,IMAGESR,IMAGEER,
     &                               IMAGESC,IMAGEEC,XCAX,YCAX,ISIZE,MAXITER,MASK)
			        MINCOL = XCAX - IDEL
			        MAXCOL = XCAX + IDEL
			        MINROW = YCAX - IDEL
			        MAXROW = YCAX + IDEL
			        CALL TVBOX(MINCOL,MAXCOL,MINROW,MAXROW,BOXCOLOR)
				CALL GETPHOTREC(PHOTLABEL,PHOTDATA,INDEX)
				WRITE(61,*) INDEX, XCAX, YCAX, COLS(INDEX), ROWS(INDEX), PHOTDATA(CALCRA), PHOTDATA(CALCDEC)
                              END IF
			      IF (REFERENCE .AND. KEY .EQ. '#') THEN
				NREF = NREF + 1
				CALL CCINHEADSET('NRSTAR',NREF,HEADBUF(1,IBUF(1)))
				FITSCARD(1:8) = 'RSTARX  '
				WRITE(FITSCARD(7:7),'(I1)') NREF
				CALL CCFHEADSET(FITSCARD,
     &                            DBLE(COLS(INDEX)),HEADBUF(1,IBUF(1)))
				FITSCARD(6:6)= 'Y'
				CALL CCFHEADSET(FITSCARD,
     &                            DBLE(ROWS(INDEX)),HEADBUF(1,IBUF(1)))
			      END IF
			  ELSE
				PRINT *,'No star within 10 ','pixels.'
			  END IF

C  Keys= 'I','O','P':  These are built in for the AEDs, and are done with
C       the mouse on the SUNs. Here we only have code for the Peritek.
			ELSE IF (KEY .EQ. 'I' .OR. KEY .EQ. 'O') THEN
#if defined(__SUNVIEW) || defined(__X11)
			  PRINT *, ' Use the mouse to zoom in and out '
#endif
#if defined(__AED512) || defined(__AED1024)
		       PRINT *, ' Use built in keys to zoom in and out'
#endif
#ifdef __PER
C       Read in old array
			  IF (UNREAD) THEN
			      CALL PZREAD(OLD)
			      UNREAD = .FALSE.
			  END IF

C       Increment zoom factor
			  IF (KEY .EQ. 'I') THEN
			    IZ = IZ + 1
			  ELSE IF (KEY .EQ. 'O') THEN
			     IF (IZ .EQ. 1) THEN
				PRINT *, 'Already all the way out '
				GOTO 100
			     END IF
			     IZ = IZ - 1
			  END IF
			  IFACT = 2**(IZ-1)

			  IF (ICOMP .GT. 1) THEN
			    PRINT *, 'Can''t zoom on compressed images'
			    PRINT *, 'Redisplay with box first'
			    GOTO 100
			  END IF

			  NROLD = (NRTV / IFACT)
			  NRDISP = (NRTV / IFACT) * IEXP * IFACT
			  NCDISP = (NCTV / IFACT) * IEXP * IFACT

			  ISC = COL - ( NCTV / IFACT ) / 2
			  IF (ISC .LT. IMAGESC) ISC = IMAGESC
			  I1 = (ISC - ICTV) * IEXP + IXLO + 1
			  I1 = MIN (512, MAX(1, I1))
C                       (Note: The + comes from transformation from screen
C                              coordinates (0-511) to array coords (1-512)
			  IER = ROW + ( NRTV / IFACT ) / 2
			  IF (IER .GT. IMAGEER) IER = IMAGEER
			  J1 = (IRTV + NRTV - 1 - IER) * IEXP
     &                               + IYLO + 1
			  J1 = MIN (512, MAX(1, J1))
			  ISR = IER - ( NRTV / IFACT ) + 1
C       Load up new array and display it
			  CALL ITVZOOM(NEW,OLD(I1,J1),NROLD,NCDISP,
     &                      IFACT,IEXP,NCDISP,NRDISP,0,0,IXLO,IYLO,0)

			  IF (ICOMP .GT. 1) IEXP = 1
			  IF (IZ .NE. 1) ORIGINAL = .FALSE.
#endif

			ELSE IF (KEY .EQ. 'P') THEN
#if defined(__AED512) || defined(__AED1024)
			  PRINT *, ' Use built in keys to pan'
#endif
#ifdef __PER
			  PRINT *, 'Pan not yet enabled'
#endif
			ELSE IF (KEY .EQ. 'Y') THEN
C               Row plot
			     CALL PMGO(DEVICE)(VTERM)
			     CALL PMGO(TSETUP)
			     CALL PMGO(ERASE)
			     II = 1
			     XMIN = 1.E30
			     YMIN = 1.E30
			     XMAX = -1.E30
			     YMAX = -1.E30
			     IF (IZ .EQ. 1) THEN
			       ISTART = IMAGESR
			       IEND = IMAGEER
			     ELSE
			       NN = (IMAGEER - IMAGESR + 1 ) /
     &                               2**(IZ-1)
			       ISTART = MAX(IMAGESR,ROW-NN/2)
			       IEND = MIN(IMAGEER,ROW+NN/2)
			     END IF
			     DO 8801 I = ISTART, IEND
				XPLOT(II) = I
				YPLOT(II) = A(COL,I)
				XMIN = MIN(XMIN,XPLOT(II))
				XMAX = MAX(XMAX,XPLOT(II))
				YMIN = MIN(YMIN,YPLOT(II))
				YMAX = MAX(YMAX,YPLOT(II))
				II = II + 1
8801                         CONTINUE
			     RANGE =XMAX - XMIN
			     IF (RANGE .EQ. 0) RANGE=1.
			     XMIN = XMIN - 0.05*RANGE
			     XMAX = XMAX + 0.05*RANGE
			     RANGE = YMAX - YMIN
			     IF (RANGE .EQ. 0) RANGE=1.
			     YMIN = YMIN - 0.05*RANGE
			     YMAX = YMAX + 0.05*RANGE
			     CALL PMGO(SETLIM)(XMIN,YMIN,XMAX,YMAX)
			     CALL PMGO(BOX)(1,2)
			     CALL PMGO(XLABEL)(3,'ROW')
			     CALL PMGO(YLABEL)(9,'INTENSITY')
			     CALL PMGO(HISTOGRAM)(XPLOT,YPLOT,II-1)
			     CALL PMGO(TIDLE)

			ELSE IF (KEY .EQ. 'X') THEN
C               Column plot
			     CALL PMGO(DEVICE)(VTERM)
			     CALL PMGO(TSETUP)
			     CALL PMGO(ERASE)
			     II = 1
			     XMIN = 1.E30
			     YMIN = 1.E30
			     XMAX = -1.E30
			     YMAX = -1.E30
			     IF (IZ .EQ. 1) THEN
			       ISTART = IMAGESC
			       IEND = IMAGEEC
			     ELSE
			       NN = (IMAGEEC - IMAGESC + 1 ) /
     &                               2**(IZ-1)
			       ISTART = MAX(IMAGESC,COL-NN/2)
			       IEND = MIN(IMAGEEC,COL+NN/2)
			     END IF
			     DO 8802 I = ISTART, IEND
				XPLOT(II) = I
				YPLOT(II) = A(I,ROW)
				XMIN = MIN(XMIN,XPLOT(II))
				XMAX = MAX(XMAX,XPLOT(II))
				YMIN = MIN(YMIN,YPLOT(II))
				YMAX = MAX(YMAX,YPLOT(II))
				II = II + 1
8802                         CONTINUE
			     RANGE = XMAX - XMIN
			     IF (RANGE .EQ. 0) RANGE=1.
			     XMIN = XMIN - 0.05*RANGE
			     XMAX = XMAX + 0.05*RANGE
			     RANGE = YMAX - YMIN
			     IF (RANGE .EQ. 0) RANGE=1.
			     YMIN = YMIN - 0.05*RANGE
			     YMAX = YMAX + 0.05*RANGE
			     CALL PMGO(SETLIM)(XMIN,YMIN,XMAX,YMAX)
			     CALL PMGO(BOX)(1,2)
			     CALL PMGO(XLABEL)(6,'COLUMN')
			     CALL PMGO(YLABEL)(9,'INTENSITY')
			     CALL PMGO(HISTOGRAM)(XPLOT,YPLOT,II-1)
			     CALL PMGO(TIDLE)

			ELSE IF (KEY .EQ. 'U') THEN
			     IF (IBCN .LT. 1) THEN
				IBC = COL
				IBR = ROW
				IBCN = 1
			     ELSE
#ifdef __AED512
				CALL AECURSOFF
#endif
#ifdef __AED1024
				CALL QCURSOFF
#endif
				CALL TVBOX(MIN0(IBC,COL),MAX0(IBC,COL),
     .                              MIN0(IBR,ROW),MAX0(IBR,ROW),BOXCOLOR)
#ifdef __AED512
				CALL AECURSON
#endif
#ifdef __AED1024
				CALL QCURSON
#endif
				IBCN = 0
			     END IF

			ELSE IF (KEY .EQ. 'V') THEN
#ifdef __PER
			  PRINT *, 'Circle drawing not yet implemented'
#endif
			     IF (IBCN .LT. 1) THEN
				IBC = COL
				IBR = ROW
				IBCN = 1
			     ELSE
				FR = FLOAT(IBR)
				FC = FLOAT(IBC)
				RAD = SQRT((ROW-FR)**2+(COL-FC)**2)
#ifdef __AED512
				CALL AECURSOFF
#endif
#ifdef __AED1024
				CALL QCURSOFF
#endif
				CALL TVCIRC(FR,FC,RAD,0.0,0.0,BOXCOLOR)
#ifdef __AED512
				CALL AECURSON
#endif
#ifdef __AED1024
				CALL QCURSON
#endif
				IBCN = 0
			     END IF

			ELSE IF (KEY .EQ. 'Z') THEN

                          JJ = (SEARCH - 1) / 2
                          SR = ROW - JJ
                          SC = COL - JJ
                          ER = SR + SEARCH - 1
                          EC = SC + SEARCH - 1
C ****************************************

C  Make sure don`t go beyond boundaries

                          SR = MAX0(IMAGESR+RSIZE/2, SR)
                          SC = MAX0(IMAGESC+CSIZE/2, SC)
                          ER = MIN0(IMAGEER-RSIZE/2, ER)
                          EC = MIN0(IMAGEEC-(CSIZE+1)/2,EC)

	                  IF (SURGICAL) THEN
                          CALL SURGICALZAP(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                      SR,ER,SC,EC,ROW,COL,SIG,PERC,.FALSE.,.FALSE.,1.,0.)
                          ELSE
                          CALL ZAPTHATSUCKER(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                      ZAPTEMP,SR,ER,SC,EC,RSIZE,CSIZE,SIG,PERC,.FALSE.,
     &                      .FALSE.,1.,0.)
                          END IF
                          CALL UPDATETV(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                      SR,ER,SC,EC)

		 	  IF (HAVEZAPFILE) THEN
			    WRITE(61,998,ERR=9999) ROW,COL,RSIZE,CSIZE,SIG
998			    FORMAT(1X,I5,1X,I5,1X,I3,1X,I3,1X,F9.3)

 
			  ELSE IF (.NOT. OP )THEN
			      OPEN(UNIT=61,FILE='test.dat',
#ifdef VMS
     .                          STATUS='NEW',CARRIAGECONTROL='LIST')
#else
     .                          STATUS= 'UNKNOWN')
#endif
				OP = .TRUE.
				PRINT *,'Opened file test.dat'
                          ELSE
			    WRITE(61,999,ERR=9999)ROW,COL,A(COL,ROW)
999                         FORMAT(2X,I5,',',2X,I5,',',1PE10.3)
			  END IF

			ELSE IF (KEY .EQ. 'C' .OR.
     &                           KEY .EQ. 'J' .OR.
     &                           KEY .EQ. 'N' .OR.
     &                           KEY .EQ. 'W') THEN

#ifdef __AED512
			  CALL AE1BYTE(AED_DJC_INST)
			  CALL AEFLUSH(0)
#endif
#ifdef __AED1024
			  CALL QCURSOFF
#endif
C       Find the centroid if the 'C' or 'W' commands

			  IF (KEY .EQ. 'C' .OR. KEY .EQ. 'W' .OR.
     &                        KEY .EQ. 'N') THEN
			     DATHEAD = .FALSE.

			     XCAX = FLOAT(COL)
			     YCAX = FLOAT(ROW)

			     MAXITER = 6
			     OK =  FINDCENT(A,IMAGESR,IMAGEER,
     &                        IMAGESC,IMAGEEC,XCAX,YCAX,ISIZE,MAXITER,MASK)

C               If the centroid could not be found, get new keystroke.
			     IF (.NOT. OK) THEN
 88                               CONTINUE
#ifdef __AED512
				  CALL AECURSON
				  CALL AEDION
#endif
#ifdef __AED1024
				  CALL QCURSON
#endif
				  GO TO 100
			      END IF

			      COL   = NINT(XCAX)
			      ROW   = NINT(YCAX)

			  ELSE
C               'J' command: nearest integer position
			     XCAX = FLOAT(COL)
			     YCAX = FLOAT(ROW)
			  END IF

C               Store the position in variables.
			  PARM = 'R'
			  CALL VARIABLE(PARM,YCAX,.TRUE.)
			  PARM = 'C'
			  CALL VARIABLE(PARM,XCAX,.TRUE.)

C       Check that this star has not already been marked.
			  IF (ISONLIST(XCAX,YCAX,0,DMIN)) THEN
			      PRINT *,'You already marked that star.'
			      GO TO 88
			  END IF

C       Increment the count of the marked stars
			  NSTARS = NSTARS + 1

C       REFERENCE option:
			  IF (REFERENCE) THEN
				NREF = NREF + 1
				CALL CCINHEADSET('NRSTAR',NREF,
     &                            HEADBUF(1,IBUF(1)))
				FITSCARD(1:8) = 'RSTARX  '
				WRITE(FITSCARD(7:7),'(I1)') NREF
				CALL CCFHEADSET(FITSCARD,
     &                            DBLE(XCAX),HEADBUF(1,IBUF(1)))
				FITSCARD(6:6)= 'Y'
				CALL CCFHEADSET(FITSCARD,
     &                            DBLE(YCAX),HEADBUF(1,IBUF(1)))
			  END IF

C       Display on the output device the location we have found.
C       Show a header only if this is the first star.
			  IF (NSTARS .EQ. 1) THEN
			       WRITE(olu,1001,ERR=9999)
			  END IF
			  WRITE(olu,1002,ERR=9999) NSTARS, YCAX, XCAX

                          IF (KEY .EQ. 'N') THEN
                            DIST  = 1E10
                            ISAVE = 0
                            DO 9711 I=1,NSTARS
                              DR = ABS(YCAX - ROWS(I))
                              IF (DR .LE. 20.0) THEN
                                DC = ABS(XCAX - COLS(I))
                                IF (DC .LE. 20.0) THEN
                                  DS = SQRT(DR**2 + DC**2)
                                  IF (DS .LT. DIST) THEN
                                    DIST  = DS
                                    CALL GETPHOTREC(PHOTLABEL,PHOTDATA,I)
                                    IF (XERR) RETURN
                                    INDEX = I
                                    ISAVE = I
                                    IF (PHOTDATA(19) .NE. 0) 
     &                                ISAVE =NINT(PHOTDATA(19))
                                  END IF
                                END IF
                              END IF
9711                        CONTINUE
			    PHOTDATA(19) = -1
			    CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,INDEX)
			  ELSE IF (OBSNUM) THEN
			    CALL CCINHEAD('OBSNUM',HEADBUF(1,IMTV),MOD(JOBS,1000))
			    ISAVE       = ID*1000 + JOBS
			  ELSE
			    ISAVE       = ID
			  END IF

			  PHOTLABEL = ' '
			  DO 8712 I=1, NUMDATA
			    PHOTDATA(I) = 0.0
8712                      CONTINUE

C       Store the result.
			  PHOTDATA(ROWLOC)        = YCAX
			  PHOTDATA(COLLOC)        = XCAX
			  CALL CCFHEAD('EXPOSURE',
     &                                  HEADBUF(1,IMTV),DVAL)
			  PHOTDATA(EXPOSURETIME) = SNGL(DVAL)
                          IF (HAVEFOCUS) THEN
                            DVAL = FOC(1) + (ID-1)*FOC(2)
                          ELSE
			    CALL CCFHEAD('FOCUS',HEADBUF(1,IMTV),DVAL)
                          END IF
			  PHOTDATA(FOCUS) = SNGL(DVAL)
			  CALL CCFHEAD('GAIN',HEADBUF(1,IMTV),DVAL)
			  PHOTDATA(PHOTGAIN) = SNGL(DVAL)
			  IF (DVAL .EQ. 0) PHOTDATA(PHOTGAIN) = 1.
			  CALL CCFHEAD('RONOISE',HEADBUF(1,IMTV),DVAL)
			  PHOTDATA(PHOTRN) = SNGL(DVAL)
			  PHOTDATA(FRAMERA) = RA
			  PHOTDATA(FRAMEDEC) = DEC
			  PHOTDATA(HOURANGLE) = HA
			  IF (HJD .GT. 0.5) THEN
			    PHOTDATA(JULIAN) = SNGL(HJD - 2444000.D0)
			    PHOTDATA(AIRMASS) = SNGL(AM)
			  ELSE
			    PHOTDATA(JULIAN) = 0.
			    PHOTDATA(AIRMASS) = 0.
			  END IF
			  PHOTDATA(UTMONTH)      = MONTH
			  PHOTDATA(UTDAY)        = DAY
			  PHOTDATA(UTYEAR)       = YEAR
			  PHOTDATA(UTEXPOS)      = UT * 3600.
                          PHOTDATA(NALTER)       = ISAVE

			  IF (.NOT. MULT) ID = ID + 1

C       Enter information for this star.
			  IF (KEY .EQ. 'W' .AND. NSTARS .GT. 0) THEN
				  CALL GETDATA(PHOTLABEL,PHOTDATA)
			  END IF
C       Store the result.
			  CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,NSTARS)
			  IF (XERR) RETURN

C       Draw box showing that the star has been marked.
			  MINCOL = COL - IDEL
			  MAXCOL = COL + IDEL
			  MINROW = ROW - IDEL
			  MAXROW = ROW + IDEL
#ifdef __AED512
			  CALL AEDBOX(MINCOL,MAXCOL,MINROW,MAXROW)
#endif
#ifdef __AED1024
			  CALL QBOX(MINCOL,MAXCOL,MINROW,MAXROW)
#endif
#if defined(__SUNVIEW) || defined(__X11)
			IF (CIRC) THEN
			  CALL TVCIRC(YCAX,XCAX,FLOAT(IDEL),0.,0.,BOXCOLOR)
			ELSE IF (CROSS) THEN
			  CALL TVCROSS(NINT(YCAX),NINT(XCAX))
			ELSE
			  CALL TVBOX(MINCOL,MAXCOL,MINROW,MAXROW,BOXCOLOR)
			END If
#endif
#ifdef __PER
			  CALL PERBOX(MINCOL,MAXCOL,MINROW,MAXROW)
#endif
C       Get ready for next character
#ifdef __AED512
			  CALL AECURSON
			  CALL AEDION
#endif
#ifdef __AED1024
			  CALL QCURSON
#endif
C     If we`ve made it this far, we must have a '0' to '9' to load in variables
			ELSE
			  PARM = FSTRCAT('R',KEY)
			  CALL VARIABLE(PARM,FLOAT(ROW),.TRUE.)
			  PARM = FSTRCAT('C',KEY)
			  CALL VARIABLE(PARM,FLOAT(COL),.TRUE.)
			  PRINT 102, ROW, COL, DATA, 
     &                       FSTRCAT('R',KEY), FSTRCAT('C',KEY)
102                       FORMAT (3X,I5,3X,I5,3X,1PE10.3,3X,A,1X,A)
			END IF
		END IF

C Do 'M' command:  find location of last box drawn. Mark with an "X"
C         and delete star from list.
	ELSE IF (KEY .EQ. 'M') THEN
	    IF (NSTARS .EQ. 0) THEN
		PRINT *,'You have not selected any stars '
	    ELSE
#ifdef __AED512
		CALL AE1BYTE(AED_DJC_INST)      ! cursor off
		CALL AEFLUSH(0)
#endif
#ifdef __AED1024
		CALL QCURSOFF
#endif
		CALL GETPHOTREC(PHOTLABEL,PHOTDATA,NSTARS)
		IF (XERR) RETURN

		COL = INT(PHOTDATA(COLLOC))
		ROW = INT(PHOTDATA(ROWLOC))
		I = (COL - ICTV - 1) / ICOMP + IXLO
		J = (ROW - IRTV - 1) / ICOMP + IYLO
#ifdef __AED512
		CALL AEMOVE(I-7,J-7)
		CALL AEDRAW(I+7,J+7)
		CALL AEMOVE(I-7,J+7)
		CALL AEDRAW(I+7,J-7)
		CALL AECURSON
		CALL AEDION                     ! Reset to expect char.
#endif
#ifdef __AED1024
		CALL QMOV(I-7,J-7)
		CALL QDRAW(I+7,J+7)
		CALL QMOV(I-7,J+7)
		CALL QDRAW(I+7,J-7)
		CALL QCURSON
#endif
#ifdef __SUNVIEW
		CALL TVCROSS(ROW,COL)
#endif
#ifdef __PER
		PRINT *, 'Star ',NSTARS,' deleted'
		PRINT *, 'TVCROSS not working on Peritek currently '
#endif
		NSTARS = NSTARS - 1
		IF (.NOT. MULT) ID = ID - 1
                IF (REFERENCE) NREF = NREF - 1
	    END IF
	END IF

C  Now go back and get new command
#ifdef __DECSTA
	CALL FLUSH(OLU)
#endif
	GOTO 100

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error while writing results')
	XERR = .TRUE.
	RETURN

C       Format statements.

 1001   FORMAT(2X,'Star     Row(y)     Col(x)',/)
 1002   FORMAT(2X,I4,2X,2(F9.2,2X))

	END
