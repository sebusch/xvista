#include "Vista.h"
        SUBROUTINE AEDIT(A,IMSR,IMER,IMSC,IMEC)

C       Routine to edit values in selected image regions.

C       This routine will set all pixels in given image subregions to
C       a constant, flag value.  This can be either done interactively,
C       or through a box number.  This routine generates automatically
C       a hardcopy record of what it did.

C       KEYWORDS:       BOX=n           Only edit the image within box 'n'
C
C                       INT             Interactively determine on the AED
C                                       a number of regions to be edited.
C                                       This keyword overrides BOX=n.
C
C                       SET=x           Flag value to which to set.
C                                       Default = -7777.
C
C                       MASK            Mask only
C
C                       CIRC            This keyword allows editing the image
C                                       within circular apertures (only when
C                                       used with INT or OLD).  It should be
C                                       noted that CIRC overrides BOX.
C
C                       OLD             When this keyword is specified, the
C                                       program reads in the previously zapped
C                                       pixels from the file "filename".  It
C                                       should be noted that OLD overrides INT.
C
C                       filename        Tells AEDIT to write out the edited
C                                       regions into a formatted file (if INT
C                                       is specified) or to read in the regions
C                                       (if OLD is specified).
C                       APPEND          Append new aedited regions to existing aedit 
C                                       file if APPEND and filename are specified


C       Author: Djorgovski 04-Sep-83

C       Modified by Ebneter, 29-Jun-87
C                   Holtzman 8/89  to change interactive TV keys a little
C                   Courteau 12/93 to include append

C       Modifications:  Allow user to 'mask' areas rather than reset the actual
C                       image values to a flag value (i.e., set a logical flag
C                       for the masked pixels).  This can be undone with UNMASK.
C                       NOTE:  Even if the pixel values are actually altered
C                       (so the image can be used in GPROF, for instance), the
C                       program ALSO creates a VISTA logical mask.

C                       If no box is specified and INT is not specified, the
C                       program returns rather than overwriting the entire
C                       image.

C                       If a file name is specified on the command line,
C                       write a file containing a list of the boxes that
C                       the user edited interactively (only works with
C                       the INT option).  If the OLD keyword is specified,
C                       the program reads the boxes in from a file and
C                       edits them.

C       Modified by J. Willick, 12-Sep-87, to work on SUN under UNIX.
C       Not all of Ebneter''s modifications (see above) are implemented
C       at this time, but her file-writing option is.
C
C       Further modified by B. Stroozas, 9/28/88, to allow AEDIT''ing within
C          circular apertures.  Files created with the "filename" option are
C          written to disk as "filename.aedit.box" or "filename.aedit.circ",
C          depending on whether boxes or circles were used to do the editing.
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        INCLUDE 'VINCLUDE:imagelink.inc'                 ! Image parameters
        INCLUDE 'VINCLUDE:MASK.INC'                      ! Max. row, col #s for mask
        INCLUDE 'VINCLUDE:customize.inc'
        INCLUDE 'VINCLUDE:tv.inc'                        ! Added 9/27/88 (JW)
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
        INCLUDE 'vistadisk/source/include/mask.inc'
#endif
C       The array:

        DIMENSION  A(IMSC:IMEC,IMSR:IMER)

        CHARACTER*8 PARM, TSTRING
        CHARACTER*80 EDITFILE, FILE

        INTEGER MIDR, MIDC
        INTEGER UPPER, CEIL
        INTEGER BN, SR, SC, ER, EC
        REAL RADIUS, C(2), C0(2)

        LOGICAL OLD, EOF, WRITEIT, MASKONLY
        LOGICAL INTERACT, CIRCCHECK, KEYCHECK, APPEN, FLIP

        NROW = IMER - IMSR + 1
        NCOL = IMEC - IMSC + 1

C       List allowed keywords.

        CALL KEYINIT
        CALL KEYDEF('OLD')
        CALL KEYDEF('BOX=')
        CALL KEYDEF('SET=')
        CALL KEYDEF('INT')
        CALL KEYDEF('MASK')
        CALL KEYDEF('CIRC')
        CALL KEYDEF('APPEND')
        CALL KEYDEF('C=')
        CALL KEYDEF('C0=')
        CALL KEYDEF('SCALE=')
        CALL KEYDEF('SCALE0=')
        CALL KEYDEF('FLIP')
        CALL KEYDEF('PA=')
        CALL KEYDEF('PA0=')

C       Initialize parameters

	C(1) = 0.
	C(2) = 0.
	C0(1) = 0.
	C0(2) = 0.
        SCALE = 1.
        SCALE0 = 1.
        FLIP = .FALSE.
        PA = 0.  
	PA0 = 0.
        INTERACT = .FALSE.
        MASKONLY = .FALSE.
        APPEN    = .FALSE.
C      Box number
        BN = 0
C      Default flag value.
        FLAGVAL = -7777.
C      # of flagged regions.
        NFLAG = 0
C      New edit.
        OLD = .FALSE.
C      Write to file.
        WRITEIT = .TRUE.
C      Not using circular apertures.
        CIRCCHECK = .FALSE.

C       Note the KEYCHECK normally converts all keyword entries to
C       Upper case.  But since we don''t call KEYCHECK until after
C       we check for keywords, we will have to locally convert to
C       upper case.

C       Check for keywords

        DO 2757 I=1,NCON
                TSTRING = WORD(I)
                L = UPPER(TSTRING)

                IF (TSTRING(1:4) .EQ. 'BOX=') THEN
C                    Fit in box only
                        CALL ASSIGN(WORD(I),F,PARM)
                        BN      =NINT(F)
                        IF (XERR) RETURN
                        WORD(I)=' '
                ELSE IF (TSTRING(1:4) .EQ. 'SET=') THEN
C                    Flag value
                        CALL ASSIGN(WORD(I),F,PARM)
                        FLAGVAL=F
                        WORD(I)=' '
                ELSE IF (TSTRING .EQ. 'INT') THEN
C                    Interactively determin image regions to be used
                        INTERACT=.TRUE.
                        WORD(I)=' '
                ELSE IF (TSTRING .EQ. 'MASK') THEN
                        MASKONLY = .TRUE.
                        WORD(I)=' '
                ELSE IF (TSTRING .EQ. 'CIRC') THEN
C                    Editing with circular apertures
                        CIRCCHECK=.TRUE.
                        BN = 0
C                    CIRC overrides BOX.
                        WORD(I)=' '
                ELSE IF (TSTRING .EQ. 'OLD') THEN
C                    Reading in edited regions from a file.
                        WRITEIT=.FALSE.
                        OLD=.TRUE.
                        INTERACT=.FALSE.
                        WORD(I)=' '
                ELSE IF (TSTRING .EQ. 'APPEND') THEN
C                    Append new aedited regions to old aedit file.
                        INTERACT=.TRUE.
                        WORD(I)=' '
                        APPEN = .TRUE.
		ELSE IF (TSTRING(1:2) .EQ. 'C=') THEN
			CALL ASSIGNV(WORD(I),2,C,N,PARM)
			IF (XERR) RETURN
			IF (N .NE. 2) THEN
			  PRINT *, 'You must specify two values with C='
			  XERR = .TRUE.
			  RETURN
			END IF
		ELSE IF (TSTRING(1:3) .EQ. 'C0=') THEN
			CALL ASSIGNV(WORD(I),2,C0,N,PARM)
			IF (XERR) RETURN
			IF (N .NE. 2) THEN
			  PRINT *, 'You must specify two values with C0='
			  XERR = .TRUE.
			  RETURN
			END IF
		ELSE IF (TSTRING .EQ. 'FLIP') THEN
                        FLIP = .TRUE.
		ELSE IF (TSTRING(1:6) .EQ. 'SCALE=') THEN
			CALL ASSIGN(WORD(I),SCALE,PARM)
			IF (XERR) RETURN
		ELSE IF (TSTRING(1:7) .EQ. 'SCALE0=') THEN
			CALL ASSIGN(WORD(I),SCALE0,PARM)
			IF (XERR) RETURN
		ELSE IF (TSTRING(1:3) .EQ. 'PA=') THEN
			CALL ASSIGN(WORD(I),PA,PARM)
			IF (XERR) RETURN
		ELSE IF (TSTRING(1:4) .EQ. 'PA0=') THEN
			CALL ASSIGN(WORD(I),PA0,PARM)
			IF (XERR) RETURN
                END IF
2757    CONTINUE

C       Set up and check file name.  The file name will be the first
C       non-blank word on the line.

        EDITFILE = ' '
        I    = 1
2758    IF (I .LE. NCON) THEN
                IF (WORD(I) .NE. ' ') THEN
                        EDITFILE = WORD(I)
                        WORD(I) = ' '
                        GO TO 50
                END IF
                I = I + 1
                GO TO 2758
        END IF

C       If the user gave any keywords we don''t know about, exit.

50      IF (.NOT. KEYCHECK()) THEN
                XERR = .TRUE.
                RETURN
        END IF

C       If they didn''t give us a file name and they did specify OLD,
C       ask them for the file name.  Otherwise, don`t create a file.

        IF (EDITFILE .EQ. ' ' ) THEN
                IF (OLD) THEN
C                 Ask for file name
                   PRINT 80, ' Name of file with regions to edit? '
80                 FORMAT (A,$)
                   READ 81, EDITFILE
81                 FORMAT (A)
                ELSE
                   WRITEIT=.FALSE.
                END IF
        END IF

C       If MASK array doesnt exist, create it now
        MISSEDMASK =.FALSE.
        IF (LOCMASK .EQ. 0) CALL NEWMASK(IMER-IMSR+1,IMEC-IMSC+1,IMSR,IMSC)

C       Get array bounds.  If INT is specified, a number of regions will
C       be marked on the AED.  If not, and a BOX is specified, only the
C       box values will be used.

        IF (INTERACT) THEN

C       Check that image is loaded:

                IF (.NOT. TVSTAT) THEN

C               Modified 8-19-87 by KE to NOT attempt to display the image;
C               simply exit gracefully with an error message.
                        PRINT *, 'No image has been displayed.'
                        PRINT *, 'Use TV to display and try again.'
                        XERR = .TRUE.
                        RETURN
                END IF

C       The following is a modification of Ebneter`s code by Willick,
C       9/15/87.  The file is now written to the data directory.
C       A further modification by Stroozas, 9/26/88, has added the option
C          of using circular apertures instead of boxes.

                IF (.NOT.CIRCCHECK) THEN
C                   Use boxes.
                        IF (WRITEIT) THEN
                                CALL FILEDEF(EDITFILE,FILE,DATADIR,
     &                                  '.aedit.box')
                            IF (APPEN) THEN
                                OPEN (97,FILE=FILE,FORM='FORMATTED',
     &                          STATUS='OLD',access='append',ERR=1000)
                            ELSE
                                OPEN (97,FILE=FILE,FORM='FORMATTED',
     &                                  STATUS='unknown',ERR=1000)
                            END IF
                        END IF

C                       Get the box position :

77                      CALL INTRCTVBOX(SR,SC,ER,EC)
                        NR = ER - SR + 1
                        NC = EC - SC + 1

                        IF (SR .EQ. 0 .AND. SC .EQ. 0 .AND.
     &                      ER .EQ. 0 .AND. EC .EQ. 0) GOTO 79

C                       Do the flagging.
                        SR = MAX(SR,IMSR)
			SC = MAX(SC,IMSC)
			ER = MIN(ER,IMER)
			EC = MIN(EC,IMEC)
                        DO 2759 J=SR,ER
                            DO 2760 I=SC,EC
                               CALL MASK(I,J)
                               IF (.NOT. MASKONLY) A(I,J)=FLAGVAL
2760                        CONTINUE
2759                    CONTINUE
                        NFLAG=NFLAG+1

C                       Record what was done :

                        IF (WRITEIT) WRITE (97,*) SR, ER, SC, EC

C                       WRITE (87,*) 'Edited region #', NFLAG
C                       WRITE (87,*) 'Rows from', SR, ' to', ER,
C     &                         '       Columns from', SC, ' to', EC
C                       WRITE (87,*) 'Total # of pixels flagged=', NC*NR
                        PRINT *, 'Total # of pixels flagged =', NC*NR

C                       CALL ASKINT('Enter 1 for another,<CR> to quit:',
C                               36,MORE,1)
C                       IF (MORE.EQ.1) GO TO 77

C  Check for another box
                        GOTO 77

79                      CONTINUE

                        IF (WRITEIT) CLOSE(97)
                ELSE
C                     Use circles.
                        IF (WRITEIT) THEN
                                CALL FILEDEF(EDITFILE,FILE,DATADIR,
     &                                  '.aedit.circ')
                            IF (APPEN) THEN
                                OPEN (97,FILE=FILE,FORM='FORMATTED',
     &                          STATUS='OLD',access='append',ERR=1000)
                            ELSE
                                OPEN (97,FILE=FILE,FORM='FORMATTED',
     &                                  STATUS='unknown',ERR=1000)
                            END IF
                        END IF

C       Call the circle-plotting subroutine.
78                      CALL INTRCTVCIRC(MIDR,MIDC,RADIUS)

                        IF (MIDR .EQ. 0 .AND. MIDC .EQ. 0 .AND.
     &                          RADIUS .EQ. 0) GOTO 76

C       Define a box to inscribe the circle.  Then check each pixel within the
C          box in order to see if a pixel lies within the circle.  If so, set
C          that pixel value to the flag value.
C                 Starting  and ending rows.
                        SR = NINT(MIDR - RADIUS)
                        ER = NINT(MIDR + RADIUS)
C                 Starting and ending columns.
                        SC = NINT(MIDC - RADIUS)
                        EC = NINT(MIDC + RADIUS)

                        SR = MAX(SR,IMSR)
			SC = MAX(SC,IMSC)
			ER = MIN(ER,IMER)
			EC = MIN(EC,IMEC)
                        DO 2766 J=SR,ER
                            DO 2767 I=SC,EC
                               DIS=SQRT(FLOAT((I-MIDC)**2+(J-MIDR)**2))
                               IF (DIS .LE. RADIUS) THEN
                                 CALL MASK(I,J)
                                 IF (.NOT. MASKONLY) A(I,J)=FLAGVAL
                               END IF
2767                        CONTINUE
2766                    CONTINUE

                        IF (WRITEIT) WRITE (97,*) MIDR, MIDC, RADIUS

                        GOTO 78

76                      CONTINUE

                        IF (WRITEIT) CLOSE (97)
                END IF

        ELSE IF (OLD) THEN
C           File-reading mode.

C           Read file from data directory; added 9/15/87.
                EOF = .FALSE.
                IF (.NOT. CIRCCHECK) THEN
C                    Use boxes.
                        CALL FILEDEF(EDITFILE,FILE,DATADIR,
     &                        '.aedit.box')
                        OPEN (97,FILE=FILE,FORM='FORMATTED',
     &                        STATUS='OLD',ERR=1000)
2761                    IF (.NOT. EOF) THEN
                             READ (97,*,END=999) SR, ER, SC, EC
                             SR = MAX(SR,IMSR)
		             SC = MAX(SC,IMSC)
		             ER = MIN(ER,IMER)
		             EC = MIN(EC,IMEC)
                             DO 2762 J=SR,ER
                               DO 2763 I=SC,EC
                                  CALL MASK(I,J)
                                  IF (.NOT. MASKONLY) A(I,J)=FLAGVAL
2763                           CONTINUE
2762                         CONTINUE
                             GO TO 2761
                        END IF
999                     CLOSE(97)
                ELSE
C                   Use circles.
                        CALL FILEDEF(EDITFILE,FILE,DATADIR,
     &                        '.aedit.circ')
                        OPEN (97,FILE=FILE,FORM='FORMATTED',
     &                        STATUS='OLD',ERR=1000)
2768                    IF (.NOT. EOF) THEN
                            READ (97,*,END=998) AMIDR, AMIDC, RADIUS
	                    MIDR = NINT(AMIDR)
                            MIDC = NINT(AMIDC)

C    Do the geometric transformation if required
			    IF (PA .NE. PA0 .OR. SCALE .NE. SCALE0 .OR. FLIP .OR.
     &                          C(1) .NE. C0(1) .OR. C(2) .NE. C0(2)) THEN
                              ROT = PA0-PA
                              MIDR = MIDR - C0(1)
                              MIDC = MIDC - C0(2)
			      IF (FLIP) MIDC = -MIDC
                              NEWC = MIDC*COSD(ROT) - MIDR*SIND(ROT)
                              NEWR = MIDC*SIND(ROT) + MIDR*COSD(ROT)
                              NEWC = NEWC*SCALE0/SCALE
                              NEWR = NEWR*SCALE0/SCALE
			      MIDR = NEWR + C(1)
			      MIDC = NEWC + C(2)
			      RADIUS = FLOAT(CEIL(RADIUS*SCALE0/SCALE))
				
			    END IF
C       As before, define a box that inscribes the circle and check all pixels
C          within the box to see which ones are inside the circle.  Flag those
C          inside the circle.
                            SR = NINT(MIDR - RADIUS)
                            ER = NINT(MIDR + RADIUS)
                            SC = NINT(MIDC - RADIUS)
                            EC = NINT(MIDC + RADIUS)
                            SR = MAX(SR,IMSR)
		            SC = MAX(SC,IMSC)
		            ER = MIN(ER,IMER)
		            EC = MIN(EC,IMEC)
                            DO 3000 J=SR,ER
                              DO 3001 I=SC,EC
                                DIS=SQRT(FLOAT((J-MIDR)**2+(I-MIDC)**2))
                                IF (DIS .LE. RADIUS) THEN
                                  CALL MASK(I,J)
                                  IF (.NOT. MASKONLY) A(I,J)=FLAGVAL
                                END IF
3001                          CONTINUE
3000                        CONTINUE
                        END IF
                        GO TO 2768
998                     CLOSE (97)
                END IF

        ELSE
C            Non-interactive mode.
                IF (BN.LE.0 .OR. BN.GT.20) THEN
                        PRINT *, 'Box number out of range.'
                        XERR=.TRUE.
                        RETURN
                ELSE
                        CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
                        NR = ER - SR + 1
                        NC = EC - SC + 1
                        IF (XERR) RETURN
                END IF

C                       Do the flagging.
                
                SR = MAX(SR,IMSR)
		SC = MAX(SC,IMSC)
		ER = MIN(ER,IMER)
		EC = MIN(EC,IMEC)
                DO 2764 J=SR,ER
                    DO 2765 I=SC,EC
                        CALL MASK(I,J)
                        IF (.NOT. MASKONLY) A(I,J)=FLAGVAL
2765                CONTINUE
2764            CONTINUE


C                       Record what was done :

C               WRITE (87,*) 'Edited box #', BN
C               WRITE (87,*) 'Rows from', SR, ' to', ER, '      Columns from',
C     &                         SC, ' to', EC
C               WRITE (87,*) 'Total # of pixels flagged =', NC*NR
                PRINT *, 'Total # of pixels flagged =', NC*NR

        END IF

        IF (MISSEDMASK) THEN
          PRINT *, 'WARNING: some requested pixels are outside of current MASK'
          MISSEDMASK = .FALSE.
        END IF

        RETURN

1000    PRINT *, ' Cannot open edit file'
        XERR=.TRUE.

        RETURN
        END
