#include "Vista.h"

       SUBROUTINE PLOTTER
C
C   VISTA Version 4.6
C
C                             VISTA PLOT COMMAND
C
C   Routine to make terminal or hardcopy plots with Lick MONGO.
C
C   This routine defines the VISTA PLOT command.
C
C   The PLOT command produces a plot of a row, column or spectrum.  Up to 15
C   images can be plotted in a single screen. The plot can be sent either to
C   the terminal, or to one of the hardcopy devices supported by Lick MONGO.
C
C     ----------------------------------------
C
C   PLOT Command Syntax :
C
C   PLOT BUF1 [BUF2 BUF3...] [R=r1,r2] [C=c1,c2] [CS=c1,c2] [RS=r1,r2]
C       [MIN=f] [MAX=f] [XS=f] [XE=f] [XMIN=f] [XMAX=f] [YMIN=f] [YMAX=f]
C       [NH=n] [NV=n] [LOG] [SEMILOG] [GRID] [INFO] [HARD] [OLD] [PORT]
C       [PIXEL] [NOERASE] [HIST]
C       [USER] [NOLABEL] [NOBOX] [INT] [MACRO=] [PSFILE=] [EPS]
C
C   where:
C
C     BUF1, BUF2,...  are the images being plotted.
C     R=R1,R2         Plot image rows R1 to R2.
C     C=C1,C2         Plot image columns C1 to C2
C     CS=C1,C2        Plot image summed over columns C1 to C2
C     RS=R1,R2        Plot image summed over rows R1 to R2
C     MIN=f           Set Minimum Y value to be 'f'
C     MAX=f           Set Maximum Y value to be 'f'
C     XS=f            Set Starting X value to be 'f'
C     XE=f            Set Ending X value to be 'f'
C     YMIN=f          Set Minimum Y value to be 'f'
C     YMAX=f          Set Maximum Y value to be 'f'
C     XMIN=f          Set Minimum X value to be 'f'
C     XMAX=f          Set Maximum X value to be 'f'
C     NH=nh,NV=nv     Divide the plotting area into nh horizontal and nv
C                       vertical panes (default: all plots in the same pane).
C     SEMILOG         Plot Log(Y) .vs. X
C     LOG             Plot Log(Y) .vs. Log(X)
C     GRID            Draws grid at every tick mark
C     PIXEL           Force plot in pixel space
C     HIST            Make histogram style plot.
C     POINTS=style    Plot points of MONGO style (do not connect points).
C     USER            Scales axes and makes labels according to
C                       contents of a standard FITS header, where
C                       Value = CRVALn + CDELTn*(array-element - CRPIXn)
C     OLD             Use old parameters from last time.
C     NOERASE         Prevents plotting window from being advanced
C                       from previous plot.  On a terminal, it
C                       inhibits erasing the screen.
C     NOLABEL         Do not draw labels.
C     INFO            Puts Obs. #, Date, and Dispersion on plot
C     INT             Connects the user with interactive mongo after
C                       plotting each of the images in the command line.
C                       The xcolumn and ycolumn are loaded for the user.
C     MACRO=file      Execute the MONGO macro 'file' after each plot.
C                       This is simmilar to INT but the mongo commands are
C                       not typed by the user but are read from a macro file.
C                       If MACRO= and INT are used together, the macro is
C                       executed before giving the interaction to the user.
C     NOTE:           All MONGO variables other limits and the expansion
C                       factor are not reset after INT or MACRO=, so the
C                       user can make subsequent Vista plots to have different
C                       margins, line styles, submargins, tick sizes, etc.
C
C    ---------- Hardcopy Direction ----------
C
C     HARD            Directs PLOT to a Hardcopy device
C     PORT            Portrait Mode (Swap X and Y axes on hardcopy plot)
C     LONG=len        (Versatec Output ONLY) Make a Versatec plot which is
C                       "len" inches long.
C     PSFILE=file     Save PostScript output in a file (not a printer).
C                       Inhibit flushing of PostScript metafile to print queue.
C                       The keyword HARD is assumed here and can be omitted.
C     WAIT,NOPRINT    Leaves the hardcopy channel open so subsequent plots
C                       will apear in the same page (or PostScript file)
C     EPS             PostScript file is to follow Encapsulated PostScript
C                        "structuring conventions"
C
C  Comments for PLOT:
C     Default graphics display and hardcopy devices are selected via codes in
C     the VGRAPHICS common block.  Defaults are set in the call to TERM in
C     the VISTA main program, and may be modified by the individual user at
C     startup, or by invoking the TERM command.
C
C     Lick MONGO is the base plotting package which handles all device
C     drivers and hardcopy servers.  Refer to the Lick MONGO Manual for
C     details.
C
C  -- A note about the autolimits routines.
C
C     Basically, the user is allowed to define all 4 axis limits independent
C     of each other.  To specify which limits are user defined, and which
C     are to be found using the default autoscaling arguments, a set of
C     integer flags are set called IXMIN, IXMAX, IYMIN, and IYMAX.  The
C     correspondences are obvious.
C
C          If a flag is set to 0, the default autoscaling value is assigned.
C          If the flag is set to 1, then the user value is to be assigned.
C
C     The values of IXMIN, etc. are carried over if the OLD keyword is used
C     otherwise, they are set to 0 at the start of the program then set as
C     required in the keyword parsing section of the program.
C
C ----------------------------------------------------------------------------
C
C   Author(s) :
C     Rick Pogge, J. Jesus Gonzalez
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C
C   Modification History:
C
C     29/12/88 Heavely modified to allow plotting of sevaral images,
C                to make USER units conform to FITS standards, and to
C                optimize the code a little [JJGG].
C
C     Version 3.5 Written:  1988 June 17
C
C     Last Revision:  1988 October 18 - Port to McDonald Observatory
C
C     7/17/88  Modified for VMS/Sun Fortran f77cvt Compatibility   [RWP]
C
C     1995 Aug 14: added EPS keyword, entailing modification of PLOTSETUP
C                  [rwp/osu]
C
C---------------------------------------------------------------------------

C  Communication with VISTA
#ifdef VMS
	INCLUDE 'VINCLUDE:vistalink.inc'

C  Image Parameters
	INCLUDE 'VINCLUDE:imagelink.inc'

C  Directory and file information
	INCLUDE 'VINCLUDE:customize.inc'

C  Those values falling between the specified (or default) X-axis limits are
C  passed to the arrays plotting XPLOT and YPLOT. NPLOT is the size of the
C  largest number of points that may be passed to the plotting routines.
C
C  WARNING: NPLOT should not exceed the current internal limits of the
C           Lick MONGO subroutines (10000, for the VISTA version of
C           the MONGO routine EXECUTE).

	INCLUDE 'VINCLUDE:work.inc'
	INCLUDE 'VINCLUDE:mult.inc'
	INCLUDE 'VINCLUDE:profile.inc'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/work.inc'
	INCLUDE 'vistadisk/source/include/mult.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'
#endif
	PARAMETER (NPLOT = 30000)
	REAL*4    XPLOT(NPLOT), YPLOT(NPLOT), ZPLOT(NPLOT,2), EPLOT(NPLOT)

	COMMON /WORK/   XPLOT, YPLOT, EPLOT
	EQUIVALENCE ( ZPLOT(1,1), XPLOT(1) )

	PARAMETER (MAXORDER = 4)

	REAL*4  TEMP(3), XPLT, XENDDATA, XENDPHYS

	INTEGER OBSNUM, MAXLENGTH, PMGO(FILEPLOT)
	INTEGER INDEXLIMITS(2), IMAGES(NCON), OLDLAMTYPE
	INTEGER IMG, NIM, IXMIN, IXMAX, IYMIN, IYMAX

	DOUBLE PRECISION LAMPIX, PIXLAM, X0, XSCALE
	REAL*4  YMIN, YMAX, OLDYMIN, OLDYMAX, YMN, YMX
	REAL*4  XMIN, XMAX, OLDXMIN, OLDXMAX, XMN, XMX
	REAL*4  XTEXT, YTEXT

	DOUBLE PRECISION XOFFSET, COEF((MAXORDER+1)**2), EVALELAMBDA
	COMMON /EORDERS/ NX, NM, XOFFSET

	CHARACTER*80 PARM, DATESTRING, TEMPSTRING, FSTRCAT
	CHARACTER*80 TOPLABEL, VERT, HORZ, VPLOTID, PSFILE, CHARS
	CHARACTER*40 MONGOCOM(7), MACROFILE
	CHARACTER*20 NSTR, NSTR1, STATUS
	CHARACTER*30 INFORM

	LOGICAL HARD, DOLABEL, DOXLABEL, DOYLABEL, DOBOX, LGRID, LINFO
	LOGICAL DOXTITLE, DOYTITLE
	LOGICAL FINDXLIMITS, FINDYLIMITS, NEW
	LOGICAL DONE, OLD, ROW, COL, ROWMASH, NORM
	LOGICAL NOERASE, USER, LAND, HIST, POINT, MULT, R4, PROFILE
	LOGICAL LOGLOG, SEMILOG, PIXELS, LAMBDA, ECHELLE
	LOGICAL LONGPLOT, LENGTHGIVEN, INTERACTIVE, WAIT, CLOSED
	LOGICAL KEYCHECK, MACRO, DOEPS, HAVEWINDOW, DOTITLE

        LOGICAL PARSEX, PARSEY, PARSEE, HAVELINES
        CHARACTER*300 XFORMSTR, YFORMSTR, EFORMSTR
        PARAMETER (MAXVAR = 10)
        INTEGER IXVAR(MAXVAR), IYVAR(MAXVAR), XBUF(MAXVAR), YBUF(MAXVAR)
        INTEGER IEVAR(MAXVAR), EBUF(MAXVAR)
        INTEGER LINES(2)
        REAL VALUE(MAXVAR)
        
        INTEGER UPPER, NUMCHAR

C  The current graphics display and hardcopy device options for plot
C  are contained in the VGRAPHICS common block.  VTERM (I*4) is the
C  graphics display terminal type, and VHARD (I*4) is the hardcopy
C  device code.  Consult the TERM.F source code for a complete listing
C  of supported devices.

	INTEGER*4 VTERM, VHARD
	COMMON /VGRAPHICS/ VTERM, VHARD
	DATA DONE /.FALSE./, NOERASE /.FALSE./, CLOSED /.TRUE./
	DATA OLDLAMTYPE /0/
        SAVE

C   List of allowed keywords.

	CALL KEYINIT()

	CALL KEYDEF('R=')
	CALL KEYDEF('C=')
	CALL KEYDEF('CS=')
	CALL KEYDEF('RS=')
	CALL KEYDEF('NORM')

	CALL KEYDEF('XC=')
	CALL KEYDEF('YC=')
	CALL KEYDEF('EC=')
	CALL KEYDEF('LINES=')
	CALL KEYDEF('XLABEL=')
	CALL KEYDEF('YLABEL=')
	CALL KEYDEF('TLABEL=')
	CALL KEYDEF('WIND=')

	CALL KEYDEF('MIN=')
	CALL KEYDEF('MAX=')
	CALL KEYDEF('XMIN=')
	CALL KEYDEF('XMAX=')
	CALL KEYDEF('YMIN=')
	CALL KEYDEF('YMAX=')
	CALL KEYDEF('XS=')
	CALL KEYDEF('XE=')
	CALL KEYDEF('NH=')
	CALL KEYDEF('NV=')
	CALL KEYDEF('OLD')
	CALL KEYDEF('PIXEL')
	CALL KEYDEF('HIST')
	CALL KEYDEF('POINTS=')

	CALL KEYDEF('SEMILOG')
	CALL KEYDEF('LOG')
	CALL KEYDEF('USER')

	CALL KEYDEF('GRID')
	CALL KEYDEF('INFO')
	CALL KEYDEF('NOERASE')
	CALL KEYDEF('NOLABEL')
	CALL KEYDEF('NOXLABEL')
	CALL KEYDEF('NOYLABEL')
	CALL KEYDEF('NOXTITLE')
	CALL KEYDEF('NOYTITLE')
	CALL KEYDEF('NOBOX')
	CALL KEYDEF('NOTITLE')
	CALL KEYDEF('EXPAND=')

	CALL KEYDEF('HARD')
	CALL KEYDEF('PORT')
	CALL KEYDEF('LONG=')
	CALL KEYDEF('PSFILE=')
        CALL KEYDEF('COLOR=')
        CALL KEYDEF('EPS')
	CALL KEYDEF('WAIT')
	CALL KEYDEF('NOPRINT')
        CALL KEYDEF('LWEIGHT=')
        CALL KEYDEF('LTYPE=')

        CALL KEYDEF('MULT=') 
	CALL KEYDEF('R4')
	CALL KEYDEF('PROFILE')
	CALL KEYDEF('PROF')
	CALL KEYDEF('PROF=')

	CALL KEYDEF('INT')
	CALL KEYDEF('MACRO=')

C   Get name of Macro and/or PS files, labels (if any) before capitalizing.

        PSFILE = ' '
        MACROFILE = ' '
        HORZ = ' '
        VERT = ' '
        MACRO = .FALSE.
        DOTITLE = .TRUE.
        DO 10 I=1,NWORD
          PARM(1:7) = WORD(I)(1: )
          L = UPPER(PARM(1:7))
          IF (PARM(1:6) .EQ. 'MACRO=') THEN
            MACRO = .TRUE.
            MACROFILE = WORD(I)(7:46)
            IF (PSFILE.NE.' ') GOTO 20

          ELSE IF (PARM(1:7).EQ.'PSFILE=') THEN
            IF (VHARD.EQ.5) THEN
                PSFILE = WORD(I)(8: )
            ELSE
	        PRINT *, 'Warning: PSFILE option only valid'
	        PRINT *, 'for PostScript hardcopy plots.'
            END IF
            IF (MACRO) GOTO 20

	  ELSE IF (PARM(1:7) .EQ. 'XLABEL=') THEN
                HORZ = WORD(I)(8:)
                IF (HORZ(1:1) .EQ. '''') THEN
                        CHARS   =HORZ(2:80)
                        N       =INDEX(CHARS,'''')
                        HORZ   =CHARS(1:N-1)
                ELSE
                        CALL STRING(HORZ,CHARS,.FALSE.)
                        HORZ = CHARS
                END IF


	  ELSE IF (PARM(1:7) .EQ. 'YLABEL=') THEN
                VERT = WORD(I)(8:)
                IF (VERT(1:1) .EQ. '''') THEN
                        CHARS   =VERT(2:80)
                        N       =INDEX(CHARS,'''')
                        VERT   =CHARS(1:N-1)
                ELSE
                        CALL STRING(VERT,CHARS,.FALSE.)
                        VERT = CHARS
                END IF

         ELSE IF (PARM(1:7) .EQ. 'TLABEL=') THEN
                TOPLABEL = WORD(I)(8:)
                IF (TOPLABEL(1:1) .EQ. '''') THEN
                        CHARS   =TOPLABEL(2:80)
                        N       =INDEX(CHARS,'''')
                        TOPLABEL   =CHARS(1:N-1)
                ELSE
                        CALL STRING(TOPLABEL,CHARS,.FALSE.)
                        TOPLABEL = CHARS
                END IF
	print *, 'toplabel: ', toplabel 


          END IF
10      CONTINUE

C   Check to see that the user did not give any unknown keywords
20	IF (.NOT. KEYCHECK()) THEN
	    XERR = .TRUE.
	    RETURN
	END IF

C   Initialize parameters.  Check first to see if the user wants the
C   parameters from the previous invocation of PPLOT.
	OLD = .FALSE.
	IF (DONE) THEN
	DO 8701 I=1, NCON
	    IF (WORD(I) .EQ. 'OLD') OLD = .TRUE.
8701    CONTINUE
	END IF

	IF (.NOT. OLD) THEN

C           Reset range in X and Y
	    YMIN = 0.0
	    YMAX = 0.0
	    YMN = 0.0
	    YMX = 0.0
	    NV = 1

	    XMIN = 0.0
	    XMAX = 0.0
	    XMN = 0.0
	    XMX = 0.0
	    NH = 1

C           Reset Autolimits flags
	    IYMAX = 0
	    IYMIN = 0
	    IXMAX = 0
	    IXMIN = 0

C           Reset mashing limits
	    INDEXLIMITS(1) = 0
	    INDEXLIMITS(2) = 0
            IVEC1 = 0
            IVEC2 = 0
            NORM = .FALSE.

C           Set default option flags
	    VPLOTID = ' '
	    HARD = .FALSE.
	    ROW  = .FALSE.
	    COL  = .FALSE.
	    LGRID  = .FALSE.
	    LINFO  = .FALSE.
	    LOGLOG = .FALSE.
	    SEMILOG = .FALSE.
	    PIXELS = .FALSE.
	    NOERASE = .FALSE.
	    LAND = .TRUE.
	    HIST = .FALSE.
            POINT = .FALSE.
	    DOBOX = .TRUE.
            DOEPS = .FALSE.
            EXPAND = 0.
            ICOLOR = 1
            WEIGHT = 2.
            LTYPE = 0
	    USER = .FALSE.
            LENGTHGIVEN = .FALSE.

	    LONGPLOT = .FALSE.
	    INTERACTIVE = .FALSE.

	    MULT = .FALSE.
	    R4 = .FALSE.
	    PROFILE = .FALSE.

	END IF

	DOLABEL = .TRUE.
	DOXLABEL = .TRUE.
	DOYLABEL = .TRUE.
	DOXTITLE = .TRUE.
	DOYTITLE = .TRUE.

	HAVELINES = .FALSE.
        HAVEWINDOW = .FALSE.
	PARSEX = .FALSE.
        PARSEY = .FALSE.
        PARSEE = .FALSE.
	WAIT = .FALSE.
        IXCOL = 0
        IYCOL = 0

C   Check for keywords.
	DO 8702 I=1, NCON

C           Plot is to be of data along given image Rows
	    IF (WORD(I)(1:2) .EQ. 'R=') THEN
		CALL ASSIGNV(WORD(I),2,TEMP,NFOUND,PARM)
		IF (XERR) RETURN
		ROW = .TRUE.
                IVEC1 = NINT(TEMP(1))
                IF (NFOUND.EQ.1) THEN
                    IVEC2 = IVEC1
		    CALL NUMBSTR(IVEC1,0,1,NSTR,LNSTR)
		    VPLOTID = FSTRCAT('Plot Along Row ',NSTR(1:LNSTR))
                ELSE
                    IVEC2 = NINT(TEMP(2))
		    CALL NUMBSTR(IVEC1,0,1,NSTR,LNSTR)
		    CALL NUMBSTR(IVEC2,0,1,NSTR1,LNSTR1)
		    VPLOTID = FSTRCAT('Rows ',FSTRCAT(NSTR(1:LNSTR),
     &                    FSTRCAT(' to ',NSTR1(1:LNSTR1))))
                END IF
		INDEXLIMITS(1) = IVEC1
		INDEXLIMITS(2) = IVEC1

C           Plot is to be of data along given image Columns
	    ELSE IF (WORD(I)(1:2) .EQ. 'C=') THEN
		CALL ASSIGNV(WORD(I),2,TEMP,NFOUND,PARM)
		IF (XERR) RETURN
		COL = .TRUE.
                IVEC1 = NINT(TEMP(1))
                IF (NFOUND.EQ.1) THEN
                    IVEC2 = IVEC1
		    CALL NUMBSTR(IVEC1,0,1,NSTR,LNSTR)
		    VPLOTID = FSTRCAT('Plot Along Column ',NSTR(1:LNSTR))
                ELSE
                    IVEC2 = NINT(TEMP(2))
		    CALL NUMBSTR(IVEC1,0,1,NSTR,LNSTR)
		    CALL NUMBSTR(IVEC2,0,1,NSTR1,LNSTR1)
		    VPLOTID = FSTRCAT('Columns ',FSTRCAT(NSTR(1:LNSTR),
     &                    FSTRCAT(' to ',NSTR1(1:LNSTR1))))
                END IF
		INDEXLIMITS(1) = IVEC1
		INDEXLIMITS(2) = IVEC1

C           Plot is to be a sum over a specified range image Columns
	    ELSE IF (WORD(I)(1:3) .EQ. 'CS=') THEN
		CALL ASSIGNV(WORD(I),2,TEMP,NFOUND,PARM)
		IF (XERR) RETURN
		COL = .TRUE.
		IF (NFOUND .NE. 2) THEN
		    PRINT *,'Usage is CS=firstcol,lastcol'
		    XERR = .TRUE.
		    RETURN
		END IF
                IVEC1 = NINT(TEMP(1))
                IVEC2 = IVEC1
		INDEXLIMITS(1) = IVEC1
		INDEXLIMITS(2) = NINT(TEMP(2))

		CALL NUMBSTR(INDEXLIMITS(1),0,1,NSTR,LNSTR)
		CALL NUMBSTR(INDEXLIMITS(2),0,1,NSTR1,LNSTR1)
		VPLOTID = FSTRCAT('Column sum from ',FSTRCAT(NSTR(1:LNSTR),
     &                    FSTRCAT(' to ',NSTR1(1:LNSTR1))))

C           Plot is to be a sum over a specified range image Rows
	    ELSE IF (WORD(I)(1:3) .EQ. 'RS=') THEN
		CALL ASSIGNV(WORD(I),2,TEMP,NFOUND,PARM)
		IF (XERR) RETURN
		ROW = .TRUE.
		IF (NFOUND .NE. 2) THEN
			PRINT *,'Usage is RS=firstrow,lastrow'
			XERR = .TRUE.
			RETURN
		END IF
                IVEC1 = NINT(TEMP(1))
                IVEC2 = IVEC1
		INDEXLIMITS(1) = IVEC1
		INDEXLIMITS(2) = NINT(TEMP(2))

		CALL NUMBSTR(INDEXLIMITS(1),0,1,NSTR,LNSTR)
		CALL NUMBSTR(INDEXLIMITS(2),0,1,NSTR1,LNSTR1)
                VPLOTID = FSTRCAT('Row sum from ',FSTRCAT(NSTR(1:LNSTR),
     &           FSTRCAT(' to ',NSTR1(1:LNSTR1))))

            ELSE IF (WORD(I) .EQ. 'NORM') THEN
                NORM = .TRUE.

	    ELSE IF (WORD(I)(1:3) .EQ. 'XC=') THEN
                CALL ASSIGNS(WORD(I),MAXVAR,XBUF,IXVAR,NXVAR,XFORMSTR,PARM)
                IF (XERR) RETURN
                PARSEX = .TRUE.

	    ELSE IF (WORD(I)(1:3) .EQ. 'YC=') THEN
                CALL ASSIGNS(WORD(I),MAXVAR,YBUF,IYVAR,NYVAR,YFORMSTR,PARM)
                IF (XERR) RETURN
                PARSEY = .TRUE.

	    ELSE IF (WORD(I)(1:3) .EQ. 'EC=') THEN
                CALL ASSIGNS(WORD(I),MAXVAR,EBUF,IEVAR,NEVAR,EFORMSTR,PARM)
                IF (XERR) RETURN
                PARSEE = .TRUE.

	    ELSE IF (WORD(I)(1:6) .EQ. 'LINES=') THEN
                CALL ASSIGNV(WORD(I),2,TEMP,NFOUND,PARM)
                IF (XERR) RETURN
                IF (NFOUND .NE. 2) THEN
                  PRINT *, 'You must specify 2 numbers with LINES='
                  XERR = .TRUE.
                  RETURN
                END IF
                LINES(1) = NINT(TEMP(1))
                LINES(2) = NINT(TEMP(2))
                HAVELINES = .TRUE.

	    ELSE IF (WORD(I)(1:5) .EQ. 'WIND=') THEN
                CALL ASSIGNV(WORD(I),3,TEMP,NFOUND,PARM)
                IF (XERR) RETURN
                IF (NFOUND .NE. 3) THEN
                  PRINT *, 'You must specify 3 numbers with WIND='
                  XERR = .TRUE.
                  RETURN
                END IF
                NH = NINT(TEMP(1))
                NV = NINT(TEMP(2))
                IW = NINT(TEMP(3))
                HAVEWINDOW = .TRUE.

C           Minimum value of X to plot
	    ELSE IF (WORD(I)(1:3) .EQ. 'XS=' .OR.
     &               WORD(I)(1:5) .EQ. 'XMIN=') THEN
		CALL ASSIGN(WORD(I),XMIN,PARM)
		IF (XERR) RETURN
		IXMIN = 1
		OLDXMIN = XMIN

C           Maximum value of X to plot
	    ELSE IF (WORD(I)(1:3) .EQ. 'XE=' .OR.
     &               WORD(I)(1:5) .EQ. 'XMAX=') THEN
		CALL ASSIGN(WORD(I),XMAX,PARM)
		IF (XERR) RETURN
		IXMAX = 1
		OLDXMAX = XMAX

C           Minimum value of Y to plot
	    ELSE IF (WORD(I)(1:4) .EQ. 'MIN=' .OR.
     &               WORD(I)(1:5) .EQ. 'YMIN=') THEN
		CALL ASSIGN(WORD(I),YMIN,PARM)
		IF (XERR) RETURN
		IYMIN = 1
		OLDYMIN = YMIN

C           Maximum Value of Y to plot
	    ELSE IF (WORD(I)(1:4) .EQ. 'MAX=' .OR.
     &               WORD(I)(1:5) .EQ. 'YMAX=') THEN
		CALL ASSIGN(WORD(I),YMAX,PARM)
		IF (XERR) RETURN
		IYMAX = 1
		OLDYMAX = YMAX

C           Number of panes in the horizontal direcction.
	    ELSE IF (WORD(I)(1:3) .EQ. 'NH=') THEN
		CALL ASSIGN(WORD(I),F,PARM)
		IF (XERR) RETURN
		NH = NINT(F)

C           Number of panes in the vertical direcction.
	    ELSE IF (WORD(I)(1:3) .EQ. 'NV=') THEN
		CALL ASSIGN(WORD(I),F,PARM)
		IF (XERR) RETURN
		NV = NINT(F)

C           Plot log(y) vs. log(x)
	    ELSE IF (WORD(I)(1:4) .EQ. 'LOG') THEN
		LOGLOG = .TRUE.
		SEMILOG = .FALSE.

C           Plot log(y) vs. x
	    ELSE IF (WORD(I)(1:7)  .EQ. 'SEMILOG') THEN
		SEMILOG = .TRUE.
		LOGLOG = .FALSE.

C           Plot y vs. x**0.25
	    ELSE IF (WORD(I) .EQ. 'R4') THEN
		 R4 = .TRUE.

C           Plot in Pixel space
	    ELSE IF (WORD(I)(1:6) .EQ. 'PIXEL') THEN
		PIXELS = .TRUE.

C           Overlay a grid
	    ELSE IF (WORD(I)(1:5) .EQ. 'GRID') THEN
		LGRID = .TRUE.

C           Include "INFO" on plots
	    ELSE IF (WORD(I)(1:5) .EQ. 'INFO') THEN
		LINFO = .TRUE.

C           Suppress erasing of plotting device
	    ELSE IF (WORD(I)(1:8) .EQ. 'NOERASE') THEN
		NOERASE = .TRUE.

C           Plot curve as a histogram
	    ELSE IF (WORD(I) .EQ. 'HIST') THEN
		HIST = .TRUE.
		POINT = .FALSE.

C           Plot points of any mongo style (do not connect points)
	    ELSE IF (WORD(I)(1:7) .EQ. 'POINTS=') THEN
		CALL ASSIGN(WORD(I),STYLE,PARM)
                IF (STYLE.LE.10.) THEN
                    PRINT*,'INVALID POINTS STYLE. Use a MONGO style:'
                    PRINT*,'POINTS=Number_of_sides*10+point_style',
     &              '+optional_fractional_expantion_factor'
                    PRINT*,'point_stile = 0 for open points,'
                    PRINT*,'            = 1 for skeletal points,'
                    PRINT*,'            = 2 for stelletal points,'
                    PRINT*,'            = 3 for solid points.'
                    XERR = .TRUE.
                    RETURN
                END IF
		POINT = .TRUE.
		HIST = .FALSE.

C           Suppress labelling of the plot
	    ELSE IF (WORD(I) .EQ. 'NOLABEL') THEN
		DOLABEL = .FALSE.

	    ELSE IF (WORD(I) .EQ. 'NOTITLE') THEN
		DOTITLE = .FALSE.

	    ELSE IF (WORD(I) .EQ. 'NOXLABEL') THEN
		DOXLABEL = .FALSE.
		DOXTITLE = .FALSE.

	    ELSE IF (WORD(I) .EQ. 'NOYLABEL') THEN
		DOYLABEL = .FALSE.
		DOYTITLE = .FALSE.

	    ELSE IF (WORD(I) .EQ. 'NOXTITLE') THEN
		DOXTITLE = .FALSE.

	    ELSE IF (WORD(I) .EQ. 'NOYTITLE') THEN
		DOYTITLE = .FALSE.

            ELSE IF (WORD(I)(1:7) .EQ. 'EXPAND=') THEN
                CALL ASSIGN(WORD(I),EXPAND,PARM)
                IF (XERR) RETURN

            ELSE IF (WORD(I)(1:6) .EQ. 'COLOR=') THEN
                CALL ASSIGN(WORD(I),F,PARM)
                IF (XERR) RETURN
                ICOLOR = NINT(F)

            ELSE IF (WORD(I)(1:8) .EQ. 'LWEIGHT=') THEN
                CALL ASSIGN(WORD(I),F,PARM)
                IF (XERR) RETURN
C                WEIGHT = NINT(F)
                WEIGHT = F

            ELSE IF (WORD(I)(1:6) .EQ. 'LTYPE=') THEN
                CALL ASSIGN(WORD(I),F,PARM)
                IF (XERR) RETURN
                LTYPE = NINT(F)

C           Suppress drawing and the axis
	    ELSE IF (WORD(I) .EQ. 'NOBOX') THEN
		DOBOX = .FALSE.

C           Use fits cards and scale plot with user coordinates
	    ELSE IF (WORD(I) .EQ. 'USER') THEN
		USER = .TRUE.

C           Send PLOT to a hardcopy device.
	    ELSE IF (WORD(I) .EQ. 'HARD') THEN
		 HARD = .TRUE.

C           Leave open the hardcopy file.
	    ELSE IF (WORD(I) .EQ. 'WAIT' .OR. WORD(I) .EQ. 'NOPRINT') THEN
		 HARD = .TRUE.
		 WAIT = .TRUE.

C           Swap X and Y axes for hardcopy plots.
	    ELSE IF (WORD(I)(1:4) .EQ. 'PORT') THEN
		 HARD = .TRUE.
		 LAND = .FALSE.

C           Make a long Versatec Plot
	    ELSE IF (WORD(I)(1:5) .EQ. 'LONG=') THEN
		 CALL ASSIGN(WORD(I),PLLENGTH,PARM)
		 IF (XERR) RETURN
		 LONGPLOT = .TRUE.
		 HARD = .TRUE.

C           Open up an Interactive MONGO command parser after each plot
	    ELSE IF (WORD(I) .EQ. 'INT') THEN
		 INTERACTIVE = .TRUE.

	    ELSE IF (WORD(I) .EQ. 'PROFILE' .OR.
     &               WORD(I) .EQ. 'PROF') THEN
		 PROFILE = .TRUE.
                 IPRFCOL = 1

	    ELSE IF (WORD(I)(1:5) .EQ. 'PROF=') THEN
                 PROFILE = .TRUE.
                 CALL ASSIGN(WORD(I),F,PARM)
                 IF (XERR) RETURN
                 IPRFCOL = NINT(F)

	    ELSE IF (WORD(I)(1:5) .EQ. 'MULT=') THEN
		 CALL ASSIGN(WORD(I),F,PARM)
		 IF (XERR) RETURN
		 MULT = .TRUE.
		 NGMLT = NINT(F)

	    ELSE IF (WORD(I)(1:7) .EQ. 'PSFILE=') THEN
		 HARD = .TRUE.

	    ELSE IF (WORD(I)(1:3) .EQ. 'EPS') THEN
                 DOEPS = .TRUE.

	    END IF

8702    CONTINUE

C   If a length for a long Versatec plot was given, make sure that it is
C   within reasonable limits.  Present limit is MAXLENGTH = 1200 inches.
	MAXLENGTH = 1200
	IF (LENGTHGIVEN) THEN
	    IF (PLLENGTH .LT. 1.0 .OR. PLLENGTH .GT. MAXLENGTH) THEN
		  PRINT *,'Length of long plots must be ',
     &                    'between 1 and',MAXLENGTH,' inches.'
		  XERR = .TRUE.
		  RETURN
	    END IF
	END IF

C   Sort out incompatible plotting options, print warnings if necessary.
       IF (.NOT.LAND .AND. .NOT. HARD) THEN
	    PRINT *, 'PORT only works with HARD keyword'
	    XERR = .TRUE.
	    RETURN

C   Can`t specify both LOGLOG and R4.
       ELSE IF (LOGLOG .AND. R4) THEN
	  PRINT *, 'Can''t have both R4 and LOG X-axis at same time'
	  PRINT *, 'Use R4 and SEMILOG for x**0.25 and log(y)'
	  XERR = .TRUE.
	  RETURN

	END IF

C   Find out what limits need to be calculated for all images,
C   and check that the user did not requested identical limits.
	FINDYLIMITS = .TRUE.
	FINDXLIMITS = .TRUE.
	IF (OLD) THEN
	    FINDYLIMITS = .FALSE.
	    FINDXLIMITS = .FALSE.
	END IF

	IF (IYMIN.EQ.1 .AND. IYMAX.EQ.1) THEN
	    IF (YMAX .EQ. YMIN) THEN
		PRINT *, 'MIN and MAX must be DIFFERENT'
		XERR = .TRUE.
		RETURN
	    END IF
	    FINDYLIMITS = .FALSE.
	END IF

	IF (IXMIN.EQ.1 .AND. IXMAX.EQ.1) THEN
	    IF (XMAX .EQ. XMIN) THEN
		PRINT *, 'XS and XE must be DIFFERENT'
		XERR = .TRUE.
		RETURN
	    END IF
	    FINDXLIMITS = .FALSE.
	END IF

C   Put the mashing limits in increasing order.
	IF (INDEXLIMITS(1) .GT. INDEXLIMITS(2)) THEN
	    INTEGERTEMP    = INDEXLIMITS(1)
	    INDEXLIMITS(1) = INDEXLIMITS(2)
	    INDEXLIMITS(2) = INTEGERTEMP
	END IF

C    Note and count the images to process, mark for plotting only the ones
C    that exist and whose size is within limits. NINTS is the largest number
C    of images we can plot, this is the largest number of integer specifiers
C    we can have in a Vista command.
	NIM = 0
	DO 8703 I = 1, NINTS
	    IMG = IBUF(I)
	    IF (IMG.LT.1 .OR. IMG.GT.MAXIM) THEN
		PRINT *,' WILL NOT PLOT BUFFER ', IMG
		PRINT *,' Buffers run from 1 to ',MAXIM

	    ELSE IF ( OLD .AND. (LAMTYPE(IMG) .NE. OLDLAMTYPE) .AND.
     &          (.NOT. (PIXELS .AND. OLDLAMTYPE .EQ. 0)) ) THEN
		PRINT *,' WILL NOT PLOT BUFFER ', IMG
		PRINT *, ' Physical coordinates of new image does not '
		PRINT *, ' match that of old image. '

	    ELSE IF (.NOT. BUFF(IMG)) THEN
		PRINT *,' WILL NOT PLOT EMPTY BUFFER ', IMG

	    ELSE IF (ISSPECTRUM(IMG)) THEN
		NIM = NIM + 1
		IMAGES(NIM) = IMG

	    ELSE IF (ROW) THEN
		IF (INDEXLIMITS(1) .LT. ICOORD(IYSR,IMG) .OR.
     &              INDEXLIMITS(2) .GT. ICOORD(IYER,IMG)) THEN
		    PRINT *,' WILL NOT PLOT BUFFER ', IMG
		    PRINT *,'Requested rows out of bounds.'
		    PRINT *,'Image has rows from ',ICOORD(IYSR,IMG)
     &                     , ' to ',ICOORD(IYER,IMG)
		ELSE IF (PIXELS) THEN
		    NIM = NIM + 1
		    IMAGES(NIM) = IMG
		ELSE IF (LAMTYPE(IMG).EQ.LAMBDAECHELLE .AND.
     &                   INDEXLIMITS(1).GT.INDEXLIMITS(2)) THEN
		    PRINT *,' WILL NOT PLOT ECHELLE FRAME ', IMG
		    PRINT *,' Can not use RS= in wavelength scale.'
		ELSE
		    NIM = NIM + 1
		    IMAGES(NIM) = IMG
		END IF

	    ELSE IF (COL) THEN
		IF (INDEXLIMITS(1) .LT. ICOORD(IXSC,IMG) .OR.
     &              INDEXLIMITS(2) .GT. ICOORD(IXEC,IMG)) THEN
		    PRINT *,' WILL NOT PLOT BUFFER ', IMG
		    PRINT *,'Requested columns out of bounds.'
		    PRINT *,'Image has columns from ',ICOORD(IXSC,IMG)
     &                     , ' to ',ICOORD(IXEC,IMG)
		 ELSE IF (ISSPECTRUM(IMG)) THEN
		    PRINT *,' WILL NOT PLOT BUFFER ', IMG
		    PRINT *,' Can not plot columns of spectra.'
		 ELSE
		    NIM = NIM + 1
		    IMAGES(NIM) = IMG
		 END IF

	    ELSE IF (.NOT. PARSEX .AND. .NOT. PARSEY) THEN
		 PRINT *,' WILL NOT PLOT BUFFER ', IMG
		 PRINT *,' You need to especify one of these keys:'
		 PRINT *,' R=r          plot row r'
		 PRINT *,' C=c          plot column c'
		 PRINT *,' RS=r1,r2     plot sum of rows r1 to r2'
		 PRINT *,' CS=c1,c2     plot sum of cols c1 to c2'
	    END IF
8703    CONTINUE
        
	IF (PROFILE) THEN

           IF (NPRF(IPRFCOL) .LE. 0) THEN
		PRINT *,'Profile common block has not been loaded...'
		XERR    =.TRUE.
		RETURN
	   END IF
	   NIM = 1

	ELSE IF (MULT) THEN

	     IF (NGMLT .GT. NG) THEN
		PRINT *,'No multiple galaxy photometry...'
	        XERR    =.TRUE.
	        RETURN
	     END IF
	     NIM = 1

        ELSE IF (PARSEX .AND. PARSEY) THEN
             NIM = 1
             IF (HAVELINES) THEN
               ISR = LINES(1)
               IER = LINES(2)
             ELSE IF (NXVAR .GT. 0) THEN
               ISR = ICOORD(IYSR,XBUF(1))
               IER = ICOORD(IYER,XBUF(1))
             ELSE IF (NYVAR .GT. 0) THEN
               ISR = ICOORD(IYSR,YBUF(1))
               IER = ICOORD(IYER,YBUF(1))
             ELSE
               ISR = 1
               IER = 1
             END IF
             DO I=1,NXVAR
	       IF (.NOT. BUFF(XBUF(I))) THEN
	             PRINT *, 'Error. Buffer does not exist: ', XBUF(I)
                     XERR = .TRUE.
               END IF
               IF (IXVAR(I) .GT. ICOORD(IXEC,XBUF(I))) THEN
                     PRINT *, 'Error. Buffer ', XBUF(I), 
     &                        'does not extend to column ', IXVAR(I)
                     XERR = .TRUE.
               END IF
               IF (ICOORD(IYSR,XBUF(I)) .GT. ISR ) THEN
                     PRINT *, 'Error. Buffer ', XBUF(I),
     &                       ' does not have a row ', ISR
                     XERR = .TRUE.
               END IF
               IF (ICOORD(IYER,XBUF(I)) .LT. IER ) THEN
                     PRINT *, 'Error. Buffer ', XBUF(I),
     &                       ' does not have a row ', IER
                     XERR = .TRUE.
               END IF
               IF (XERR) RETURN
             END DO
             DO I=1,NYVAR
	       IF (.NOT. BUFF(YBUF(I))) THEN
	             PRINT *, 'Error. Buffer does not exist: ', XBUF(I)
                     XERR = .TRUE.
               END IF
               IF (IYVAR(I) .GT. ICOORD(IXEC,YBUF(I))) THEN
                     PRINT *, 'Error. Buffer ', YBUF(I), 
     &                        'does not extend to column ', IYVAR(I)
                     XERR = .TRUE.
               END IF
               IF (ICOORD(IYSR,YBUF(I)) .GT. ISR ) THEN
                     PRINT *, 'Error. Buffer ', YBUF(I),
     &                       ' does not have a row ', ISR
                     XERR = .TRUE.
               END IF
               IF (ICOORD(IYER,YBUF(I)) .LT. IER ) THEN
                     PRINT *, 'Error. Buffer ', YBUF(I),
     &                       ' does not have a row ', IER
                     XERR = .TRUE.
               END IF
               IF (XERR) RETURN
             END DO

	END IF

	IF (NIM.EQ.0) THEN
	      PRINT *, ' NO IMAGES TO PLOT.'
	      XERR = .TRUE.
	      RETURN
	END IF

C       Save the coordinate type, force plotting according to FITS cards if 
C       not a VISTA wavelength-scale and the user did not forced PIXEL scale.
	IF (.NOT. MULT .AND. .NOT. PROFILE) 
     &         OLDLAMTYPE = LAMTYPE(IMAGES(1))
	IF (PIXELS) THEN
            OLDLAMTYPE = 0
            USER = .FALSE.
        ELSE IF (OLDLAMTYPE.EQ.0) THEN
            USER = .TRUE.
        END IF

C   Initialize the plotting device. This is plotting package dependent.
C   Lick MONGO (which is operating system independent) is used below.
C   Set the line weight appropriate to the plotting device requested.

	IF (CLOSED) THEN
	    CALL PLOTSETUP(HARD,LAND,PSFILE,OLD,NOERASE,DOEPS,XERR)
	    IF (XERR) RETURN
	    CLOSED = .FALSE.
	END IF
        CALL PMGO(SETLWEIGHT)(WEIGHT)

C       If the user requests a long Versatec plot and the length of the
C       X-axis was given, set the location of the plot. The constants in
C       the call to SETLOC are the default parameters that Lick MONGO uses.
	IF (LONGPLOT) THEN
	    XENDDATA = 250.0 + 200.0 * PLLENGTH
	    XENDPHYS = XENDDATA + 250.0
	    CALL PMGO(SETPHYSICAL)(0.0,0.0,XENDPHYS,2111.0)
	    CALL PMGO(SETLOC)(250.0,150.0,XENDDATA,2000.0)
	END IF

C       Setup tick markrs.
	IF (LOGLOG) THEN
	    CALL PMGO(TICKSIZE)(-1.,1.,-1.,1.)

	ELSE IF (SEMILOG) THEN
	    CALL PMGO(TICKSIZE)(0.,0.,-1.,-1.)

	ELSE
	    CALL PMGO(TICKSIZE)(0.,0.,0.,0.)

	END IF

        CALL PMGO(SETCOLOR)(ICOLOR)

C-------- THIS IS THE START OF THE MULTY-PLOTTER LOOP ---------------
	DO 1000 IMG = 1, NIM, 1

C  First take care of special case of plotting one of the multiple
C    galaxy profiles from the MULT common block (from SNUC), which
C    does not use data from an image buffer at all

	IF (MULT .OR. PROFILE) THEN
	   POINT = .TRUE.
	   NTOPLOT = 0
 
	   K = 1
	   XMIN = 1.E10
	   XMAX = -1.E10
	   YMIN = 1.E10
	   YMAX = -1.E10
           IF (PROFILE) THEN
	      DO 2871 I = 1, NPRF(IPRFCOL)
	        XPLOT(I) = (I-1)*PRFSC(IPRFCOL)
	        YPLOT(I) = PARAMPRF(5,I,IPRFCOL)
	        XMIN = MIN(XMIN,XPLOT(I))
	        XMAX = MAX(XMAX,XPLOT(I))
	        YMIN = MIN(YMIN,YPLOT(I))
	        YMAX = MAX(YMAX,YPLOT(I))
		NTOPLOT = NTOPLOT + 1
2871          CONTINUE
	   ELSE
             DO 2771 I = 1, NGMLT	
	       JS = K
	       DO 2772 J=JS, MISO
	         IF (SURF(1,J) .NE. 0.0) THEN
	            IF (I .EQ. NGMLT) THEN
	              XPLOT(J-JS+1)   = RISO(J-JS+1)*SCALEMLT
	              YPLOT(J-JS+1)   = SURF(1,J)
		      XMIN = MIN(XMIN,XPLOT(J-JS+1))
		      YMIN = MIN(YMIN,YPLOT(J-JS+1))
		      XMAX = MAX(XMAX,XPLOT(J-JS+1))
		      YMAX = MAX(YMAX,YPLOT(J-JS+1))
C                     EPLOT(J-JS+1,1) = SURSIG(1,J)
C                     EPLOT(J-JS+1,2) = SURSIG(1,J)	             
		      NTOPLOT = NTOPLOT + 1
		    END IF
		 ELSE
		    K = J + 1
		    GOTO 2771
		 END IF
2772           CONTINUE
2771         CONTINUE
	   END IF
	   XMIN = XMIN - 0.05*(XMAX-XMIN)
	   XMAX = XMAX + 0.05*(XMAX-XMIN)
	   YMIN = YMIN - 0.05*(YMAX-YMIN)
	   YMAX = YMAX + 0.05*(YMAX-YMIN)
C       Modified to enable plots with OLD limits. SC 3/16/94
           IF (.NOT. OLD) THEN
                OLDXMIN=XMIN
                OLDXMAX=XMAX
                OLDYMIN=YMIN
                OLDYMAX=YMAX
           ELSE
                XMIN    =OLDXMIN
                XMAX    =OLDXMAX
                YMIN    =OLDYMIN
                YMAX    =OLDYMAX
           END IF

	   GOTO 6654
	END IF


        IF (PARSEX .AND. PARSEY) THEN
          XMN = 1.E30
          XMX = -1.E30
          YMN = 1.E30
          YMX = -1.E30
          DO I=ISR, IER
            DO J = 1, NXVAR
              IF (IXVAR(J) .EQ. 0) THEN
                VALUE(J) = I
              ELSE
	        LOCATION = IMLOC(XBUF(J))
	        JSR = ICOORD(IYSR,XBUF(J))
	        JER = ICOORD(IYER,XBUF(J))
	        JSC = ICOORD(IXSC,XBUF(J))
	        JEC = ICOORD(IXEC,XBUF(J))
                CALL CCPOP(LOCATION,JSR,JER,JSC,JEC,I,IXVAR(J),VALUE(J))
              END IF
            END DO
            IF (NXVAR .GT. 0) THEN
              WRITE(TEMPSTRING,XFORMSTR) (VALUE(J),J=1,NXVAR)
            ELSE 
              L = NUMCHAR(XFORMSTR)
              TEMPSTRING = XFORMSTR(3:L-2)
            END IF
            L = NUMCHAR(TEMPSTRING)
            LS = INDEX(TEMPSTRING,' ')
            DO WHILE (LS .GT. 0 .AND. LS. LT. L)
                TEMPSTRING(LS:LS) = '+'
                LS = INDEX(TEMPSTRING,' ')
            END DO
            CALL ASSIGN(TEMPSTRING,XPLOT(I),PARM)

            DO J = 1, NYVAR
              IF (IYVAR(J) .EQ. 0) THEN
                VALUE(J) = I
              ELSE
	        LOCATION = IMLOC(YBUF(J))
	        JSR = ICOORD(IYSR,YBUF(J))
	        JER = ICOORD(IYER,YBUF(J))
	        JSC = ICOORD(IXSC,YBUF(J))
	        JEC = ICOORD(IXEC,YBUF(J))
                CALL CCPOP(LOCATION,JSR,JER,JSC,JEC,I,IYVAR(J),VALUE(J))
              END IF
            END DO
            IF (NYVAR .GT. 0) THEN
              WRITE(TEMPSTRING,YFORMSTR) (VALUE(J),J=1,NYVAR)
            ELSE
              L = NUMCHAR(YFORMSTR)
              TEMPSTRING = YFORMSTR(3:L-2)
            END IF
            L = NUMCHAR(TEMPSTRING)
            LS = INDEX(TEMPSTRING,' ')
            DO WHILE (LS .GT. 0 .AND. LS. LT. L)
              TEMPSTRING(LS:LS) = '+'
              LS = INDEX(TEMPSTRING,' ')
            END DO
            CALL ASSIGN(TEMPSTRING,YPLOT(I),PARM)

            IF (PARSEE) THEN
              DO J = 1, NEVAR
                IF (IEVAR(J) .EQ. 0) THEN
                  VALUE(J) = I
                ELSE
	          LOCATION = IMLOC(EBUF(J))
	          JSR = ICOORD(IYSR,EBUF(J))
	          JER = ICOORD(IYER,EBUF(J))
	          JSC = ICOORD(IXSC,EBUF(J))
	          JEC = ICOORD(IXEC,EBUF(J))
                  CALL CCPOP(LOCATION,JSR,JER,JSC,JEC,I,IEVAR(J),VALUE(J))
                END IF
              END DO
              IF (NEVAR .GT. 0) THEN
                  WRITE(TEMPSTRING,EFORMSTR) (VALUE(J),J=1,NEVAR)
              ELSE
                L = NUMCHAR(EFORMSTR)
                TEMPSTRING = EFORMSTR(3:L-2)
              END IF
              L = NUMCHAR(TEMPSTRING)
              LS = INDEX(TEMPSTRING,' ')
              DO WHILE (LS .GT. 0 .AND. LS. LT. L)
                TEMPSTRING(LS:LS) = '+'
                LS = INDEX(TEMPSTRING,' ')
              END DO
              CALL ASSIGN(TEMPSTRING,EPLOT(I),PARM)
            END IF

            IF ((IXMIN .EQ. 0 .OR. 
     &          (IXMIN .EQ. 1 .AND. XPLOT(I) .GE. XMIN)) .OR.
     &          (IXMAX .EQ. 0 .OR. 
     &          (IXMAX .EQ. 1 .AND. XPLOT(I) .LE. XMAX)) ) THEN
              XMN = MIN(XMN, XPLOT(I))
              XMX = MAX(XMX, XPLOT(I))
            END IF
            IF ((IYMIN .EQ. 0 .OR. 
     &          (IYMIN .EQ. 1 .AND. YPLOT(I) .GE. YMIN)) .OR.
     &          (IYMAX .EQ. 0 .OR. 
     &          (IYMAX .EQ. 1 .AND. YPLOT(I) .LE. YMAX)) ) THEN
              YMN = MIN(YMN, YPLOT(I))
              YMX = MAX(YMX, YPLOT(I))
            END IF
            
          END DO
          XR = XMX - XMN
          IF (ABS(XR) .GT. 0) THEN
            XMN = XMN - 0.1*XR
            XMX = XMX + 0.1*XR
          ELSE
            XMN = 0.9*XMN
            XMX = 1.1*XMX
          END IF
          YR = YMX - YMN
          IF (ABS(YR) .GT. 0) THEN
            YMN = YMN - 0.1*YR
            YMX = YMX + 0.1*YR
          ELSE
            YMN = 0.9*YMN
            YMX = 1.1*YMX
          END IF
	  IF (FINDXLIMITS .AND. IXMIN .EQ. 0) THEN
	    XMIN = XMN
	    OLDXMIN = XMIN
	  END IF
	  IF (FINDXLIMITS .AND. IXMAX .EQ. 0) THEN
	    XMAX = XMX
	    OLDXMAX = XMAX
	  END IF
	  IF (FINDYLIMITS .AND. IYMIN .EQ. 0) THEN
	    YMIN = YMN
	    OLDYMIN = YMIN
	  END IF
	  IF (FINDYLIMITS .AND. IYMAX .EQ. 0) THEN
	    YMAX = YMX
	    OLDYMAX = YMAX
	  END IF
          NTOPLOT = IER - ISR + 1
          GOTO 1001
        END IF

C   Note the buffer number and the pixel limits of the image.
        IMBUF = IMAGES(IMG)
        ISR = ICOORD(IYSR,IMBUF)
        IER = ICOORD(IYER,IMBUF)
        ISC = ICOORD(IXSC,IMBUF)
        IEC = ICOORD(IXEC,IMBUF)

C   -- This section finds x-limits and loads the X-plotting array --

C   Establish units and extraction-orientation (rows or columns).
	LAMBDA = .FALSE.
	ECHELLE = .FALSE.
	IF (ROW .OR. ISSPECTRUM(IMBUF)) THEN
	    ROWMASH = .TRUE.
	    IPMN = ISC
	    IPMX = IEC
	    IF(.NOT.PIXELS .AND. LAMTYPE(IMBUF).NE.0) THEN
		LAMBDA = .TRUE.
		IF (LAMTYPE(IMBUF).EQ.LAMBDAECHELLE) THEN
		    ECHELLE = .TRUE.
		    CALL CCEGETPOLY(HEADBUF(1,IMBUF),
     &                            COEF,NX,NM,XOFFSET)
		    NCOEF = (NX+1)*(NM+1)
		    CALL NUMBSTR(INDEXLIMITS(1),0,1,NSTR,LNSTR)
		    VPLOTID=FSTRCAT('Plot of Echelle Row ',NSTR(1:LNSTR))
		END IF
	    END IF
	ELSE
	    ROWMASH = .FALSE.
	    IPMN = ISR
	    IPMX = IER
	END IF

C   Establish the absolute X-axis limits of the image. If there are
C   dispersion coefficients in the FITS header, scale the X-axis accordingly.
	IF (FINDXLIMITS) THEN

	    IF (USER) THEN
C               FITS standard cards are relative to array element
C               numbers, not to VISTA pixels.
		IF (ROWMASH) THEN
		   XMN = SNGL( CRVAL1(IMBUF) + CDELT1(IMBUF)*
     &                   (1.0D0-CRPIX1(IMBUF)) )
		   XMX = XMN + SNGL(CDELT1(IMBUF)*DBLE(IPMX-IPMN))
		ELSE
		   XMN = SNGL( CRVAL2(IMBUF) + CDELT2(IMBUF)*
     &                   (1.0D0-CRPIX2(IMBUF)) )
		   XMX = XMN + SNGL(CDELT2(IMBUF)*DBLE(IPMX-IPMN))
		END IF

	    ELSE IF (.NOT. LAMBDA) THEN
		XMN = FLOAT(IPMN)
		XMX = FLOAT(IPMX)

	    ELSE IF (.NOT. ECHELLE) THEN
		XMN = LAMPIX(1.D0,IMBUF)
		XMX = LAMPIX(DBLE(IPMX-IPMN+1),IMBUF)

	    ELSE IF (ECHELLE) THEN
		XMN = EVALELAMBDA(DBLE(FLOAT(IPMN)),
     &                 INDEXLIMITS(1), COEF, NCOEF, XX, 0)
		XMX = EVALELAMBDA(DBLE(FLOAT(IPMX)),
     &                 INDEXLIMITS(1), COEF, NCOEF, XX, 0)

	    END IF

C           Put the absolute X-limits in increasing order.
	    IF (XMN .GT. XMX) THEN
		TMP = XMN
		XMN = XMX
		XMX = TMP
	    END IF

C   Establish the X-axis plotting limits. If not user defined, set them
C   equal to their default values.  Trap invalid values of the limits.
	    IF (IXMIN .EQ. 0) THEN
		XMIN = XMN
		OLDXMIN = XMIN
	    ELSE IF (XMIN .GT. XMX) THEN
		PRINT *,'XS is out of range for image ',IMBUF
		GO TO 1000
	    END IF

	    IF (IXMAX .EQ. 0) THEN
		XMAX = XMX
		OLDXMAX = XMAX
	    ELSE IF (XMAX .LT. XMN) THEN
		PRINT *,'XE is out of range for image ',IMBUF
		GO TO 1000
	    END IF

	ELSE
	    XMIN = OLDXMIN
	    XMAX = OLDXMAX

	END IF

C   Find the pixels that correspond to the plotting X-axis limits, and
C   load the abscisae array only with those pixels within limits.
	IF (.NOT. LAMBDA .OR. USER) THEN
C           Units are scaled pixels.
	    IF (USER) THEN
		IF (ROWMASH) THEN
		   PMIN = SNGL( CRPIX1(IMBUF) +
     &             (DBLE(XMIN)-CRVAL1(IMBUF))/CDELT1(IMBUF) )
		   PMAX = SNGL( CRPIX1(IMBUF) +
     &             (DBLE(XMAX)-CRVAL1(IMBUF))/CDELT1(IMBUF) )
		   XSCALE = CDELT1(IMBUF)
		   X0 = CRVAL1(IMBUF)
		   IREF = CRPIX1(IMBUF)
		ELSE
		   PMIN = SNGL( CRPIX2(IMBUF) +
     &             (DBLE(XMIN)-CRVAL2(IMBUF))/CDELT2(IMBUF) )
		   PMAX = SNGL( CRPIX2(IMBUF) +
     &             (DBLE(XMAX)-CRVAL2(IMBUF))/CDELT2(IMBUF) )
		   XSCALE = CDELT2(IMBUF)
		   X0 = CRVAL2(IMBUF)
		   IREF = CRPIX2(IMBUF)
		END IF
C               Remember that FITS standard cards are relative to
C               array element numbers, not to VISTA pixels.
		PMIN = PMIN + FLOAT(IPMN-1)
		PMAX = PMAX + FLOAT(IPMN-1)

		IF (PMIN .LT. PMAX) THEN
		    IXS = MAX0(IPMN,MIN0(IPMX,NINT(PMIN)))
		    IXE = MIN0(IPMX,MAX0(IPMN,NINT(PMAX)))
		    XDELTA = XSCALE
		ELSE
		    IXS = MIN0(IPMX,MAX0(IPMN,NINT(PMIN)))
		    IXE = MAX0(IPMN,MIN0(IPMX,NINT(PMAX)))
		    XDELTA = -XSCALE
		END IF
		XPLOT(1) = X0 + XSCALE*(FLOAT(IXS-IPMN+1)-IREF)

            ELSE IF (R4) THEN
                XMINTMP = XMIN
                XMAXTMP = XMAX
                IF (IXMIN .GT. 0) XMIN = XMIN**4
                IF (IXMAX .GT. 0) XMAX = XMAX**4
                IF (XMIN .LT. XMAX) THEN
                    IXS = MAX0(IPMN,MIN0(IPMX,NINT(XMIN)))
                    IXE = MIN0(IPMX,MAX0(IPMN,NINT(XMAX)))
                    XDELTA = 1.0
                ELSE
                    IXS = MIN0(IPMX,MAX0(IPMN,NINT(XMIN)))
                    IXE = MAX0(IPMN,MIN0(IPMX,NINT(XMAX)))
                    XDELTA = -1.0
                END IF
                XPLOT(1) = IXS
                XMIN = XMINTMP
                XMAX = XMAXTMP

C           Units are unscaled pixels.
	    ELSE
		IF (XMIN .LT. XMAX) THEN
		    IXS = MAX0(IPMN,MIN0(IPMX,NINT(XMIN)))
		    IXE = MIN0(IPMX,MAX0(IPMN,NINT(XMAX)))
		    XDELTA = 1.0
		ELSE
		    IXS = MIN0(IPMX,MAX0(IPMN,NINT(XMIN)))
		    IXE = MAX0(IPMN,MIN0(IPMX,NINT(XMAX)))
		    XDELTA = -1.0
		END IF
		XPLOT(1) = IXS
	    END IF
	    NTOPLOT = MIN0(IABS(IXE-IXS)+1,NPLOT)
	    DO 8704 I = 2, NTOPLOT, 1
		XPLOT(I) = XPLOT(I-1) + XDELTA
8704        CONTINUE

C       Wavelength units. Use inverse dispersion-relation
C       to find the pixels corresponding to the X-axis limits.
	ELSE IF (.NOT. ECHELLE) THEN
	    PMIN=SNGL(PIXLAM(DBLE(XMIN),IMBUF))+FLOAT(IPMN-1)
	    PMAX=SNGL(PIXLAM(DBLE(XMAX),IMBUF))+FLOAT(IPMN-1)

	    IF (XMIN .LT. XMAX) THEN
		IXS = MAX0(IPMN,MIN0(IPMX,NINT(PMIN)))
		IXE = MIN0(IPMX,MAX0(IPMN,NINT(PMAX)))
		XDELTA = 1.0
	    ELSE
		IXS = MIN0(IPMX,MAX0(IPMN,NINT(PMIN)))
		IXE = MAX0(IPMN,MIN0(IPMX,NINT(PMAX)))
		XDELTA = -1.0
	    END IF

	    NTOPLOT = MIN0(IABS(IXE-IXS)+1,NPLOT)
	    X = FLOAT(IXS-IPMN+1) - XDELTA
	    DO 8705 I = 1, NTOPLOT
		X = X + XDELTA
		XPLOT(I) = SNGL(LAMPIX(DBLE(X),IMBUF))
8705        CONTINUE

C       X-units are Echelle-wavelength units. Since there is no explicit
C       inverse-dispersion-relation we have to proceed pixel-by-pixel.
	ELSE IF (ECHELLE) THEN
	    IF (XMIN .LT. XMAX) THEN
		IXS = IPMN
		IXE = IPMX
	    ELSE
		IXS = IPMX
		IXE = IPMN
	    END IF

	    JJ = 0
	    IXS = IPMN
	    IXE = IPMX
	    DO 8706 I = IPMN, IPMX
		XPLT = EVALELAMBDA(DBLE(FLOAT(I)),
     &                    INDEXLIMITS(1), COEF, NCOEF, XX, 0)
		IF (XMIN .GT. XMAX) THEN
		  IF (XPLT .GT. XMIN) THEN
		      IXS = I + 1
		      GO TO 902
		  ELSE IF (XPLT .LT. XMAX) THEN
		      IXE = I - 1
		  ELSE IF (XPLT.GE.XMAX.AND.XPLT.LE.XMIN) THEN
		      JJ = JJ + 1
		      XPLOT(JJ) = XPLT
		  END IF
		ELSE
		  IF (XPLT .GT. XMAX) THEN
		      IXE = I - 1
		      GO TO 902
		  ELSE IF (XPLT .LT. XMIN) THEN
		      IXS = I + 1
		  ELSE
		      JJ = JJ + 1
		      XPLOT(JJ) = XPLT
		  END IF
		END IF
C               Make sure not to plot more points than allowed
		IF (JJ .EQ. NPLOT) GO TO 902
8706        CONTINUE

C           Note the size of the plot.
902         NTOPLOT = JJ
	END IF

C--- In this section, we load the Y plotting array and find Y-limits. -----

C--  Load the Y-plotting array with the pixels within the X-axis limits.
        IVEC = IVEC1
800	IF (ROWMASH) THEN
	    IC1 = IXS
	    IC2 = IXE
	    IF (ISSPECTRUM(IMBUF)) THEN
		IR1 = ISR
		IR2 = IR1
	    ELSE IF (IVEC1.EQ.IVEC2) THEN
		IR1 = INDEXLIMITS(1)
		IR2 = INDEXLIMITS(2)
            ELSE
                IF (IVEC.GT.IER) GOTO 850
		IR1 = MAX0(IVEC,ISR)
		IR2 = IR1
	    END IF
	ELSE
	    IR1 = IXS
	    IR2 = IXE
	    IF (IVEC1.EQ.IVEC2) THEN
	        IC1 = INDEXLIMITS(1)
	        IC2 = INDEXLIMITS(2)
            ELSE
                IF (IVEC.GT.IEC) GOTO 850
	        IC1 = MAX0(IVEC,ISC)
	        IC2 = IC1
	    END IF
	END IF

	LOCATION = IMLOC(IMBUF)
	CALL CCLOADVECTOR(LOCATION,ISR,IER,ISC,IEC,YPLOT,
     &                  NTOPLOT,IR1,IR2,IC1,IC2,ROWMASH,NORM)

C   The default Y-axis limits are taken to span the full range of the Y values
C   Extra space is added above and below amounting to 10% of the full Y value
C   range to keep points off the coordinate axes as much as possible. YMX and
C   YMN are the maximum and minimum Y data values within the X-axis limits.
	IF (FINDYLIMITS) THEN

	    YMN = YPLOT(1)
	    YMX = YPLOT(2)
	    DO 8707 I = 2, NTOPLOT
		YMX = AMAX1 ( YMX, YPLOT(I) )
		YMN = AMIN1 ( YMN, YPLOT(I) )
8707        CONTINUE

C   If YMN = YMX, then pick reasonable limits around that value and plot
C   the straight line, rather than crashing the plotting routines.
	    IF (YMN .EQ. YMX) THEN
		IF (YMN .EQ. 0.0) THEN
		    YMN = -1.0
		    YMX = 1.0
		ELSE
		    YMN = 0.9*YMN
		    YMX = 1.1*YMX
		END IF
	    END IF

C   If user has not specified a particular Y limit, set it to the default
C   autoscaling value and save it in OLDMIN or OLDMAX as appropriate.
	    IF (IYMIN .EQ. 0) THEN
		YMIN = YMN - 0.1*(YMX-YMN)
		OLDYMIN = YMIN
	    ELSE IF (YMIN .GT. YMX) THEN
		PRINT *,'MIN is out of range for image ',IMBUF
		GO TO 900
	    END IF

	    IF (IYMAX .EQ. 0) THEN
		YMAX = YMX + 0.1*(YMX-YMIN)
		OLDYMAX = YMAX
	    ELSE IF (YMAX .LT. YMN) THEN
		PRINT *,'MAX is out of range for image ', IMBUF
		GO TO 900
	    END IF

C    Use the plotting limits from the previous plot.
	ELSE
	    YMIN = OLDYMIN
	    YMAX = OLDYMAX

	END IF

C   -- In this section, we set the arrays for the various plotting options

6654	IF (R4.AND.IVEC.EQ.IVEC1) THEN
	   DO 4456 I = 1, NTOPLOT
	     XPLOT(I) = XPLOT(I) ** 0.25
4456       CONTINUE
	END IF

	IF (SEMILOG) THEN
C       If a semi-log plot is requested, adjust the Y limits appropriately
C       and load the logarithm into the YPLOT array. If the YLIM keyword was
C       set equal to a negative value, inform the user but do not plot.
C       If all of the Y values are <= 0.0, do not plot but inform the user.
	    IF (IYMIN .EQ. 1 .AND. YMIN .LE. 0.0) THEN
		PRINT *,'MIN must be > 0.0 for Semi-Log Plots'
		GO TO 900
	    ELSE IF (IYMAX .EQ. 1 .AND. YMAX .LE. 0.0) THEN
		PRINT *, 'MAX must be > 0.0 for Semi-Log Plots'
		GO TO 900
	    ELSE IF (YMAX .LE. 0.0) THEN
		PRINT *,'All Y values are <= 0.0'
		PRINT *,'Cannot do a Semi-Log Plot'
		GO TO 1000
	    ELSE IF (YMIN .LE. 0.0) THEN
C           Use as limit the minimum positve value in array YPLOT.
		YMIN = YMAX
		DO 8708 I = 1, NTOPLOT
		      IF (YPLOT(I).GT.0.0) YMIN = AMIN1(YPLOT(I),YMIN)
8708            CONTINUE
	    END IF

	    OLDYMAX = YMAX
	    OLDYMIN = YMIN
	    YMAX = ALOG10(YMAX)
	    YMIN = ALOG10(YMIN)

C           Load the logarithms of the Y values into the YPLOT array.
C           If a value of YPLOT <= 0.0, then YPLOT = (YMIN - 4) in
C           order to keep things from blowing up in the user`s face.
	    DO 8709 I = 1, NTOPLOT
		IF ( YPLOT(I) .GT. 0.0 ) THEN
		    YPLOT(I) = ALOG10(YPLOT(I))
		ELSE
		    YPLOT(I) = YMIN - 4.0
		END IF
8709        CONTINUE

	ELSE IF (LOGLOG) THEN
C       If the user has requested a Log-Log plot, then adjust the X and Y
C       limits appropriately and load the logarithm into the data arrays.
C           Check that the values of YMIN and YMAX are positive. If not, adjust
C           appropriately using only the extremes of positive Y values only.
C           Do not plot if the user tried to impose a negative limit.
	    IF (IYMIN .EQ. 1 .AND. YMIN .LE. 0.0) THEN
		PRINT *,'MIN must be > 0.0 for Log-Log Plots'
		GO TO 900
	    ELSE IF (IYMAX .EQ. 1 .AND. YMAX .LE. 0.0) THEN
		PRINT *, 'MAX must be > 0.0 for Log-Log Plots'
		GO TO 900
	    ELSE IF (YMAX .LE. 0.0) THEN
		PRINT *,'All Y values are <= 0.0'
		PRINT *,'Cannot do a Log-Log Plot'
		GO TO 1000
	    ELSE IF (YMIN .LE. 0.0) THEN
C               Use as limit the minimum positive value in array YPLOT.
		YMIN = YMAX
		DO 8710 I = 1, NTOPLOT
		      IF (YPLOT(I).GT.0.0) YMIN = AMIN1(YPLOT(I),YMIN)
8710            CONTINUE
		OLDYMIN = YMIN
	    END IF

	    YMAX = ALOG10(YMAX)
	    YMIN = ALOG10(YMIN)

C           Check the values of XMIN and XMAX. If one is negative, adjust
C           it appropriately using the extremes of positive X values.
	    IF (IXMIN .EQ. 1 .AND. XMIN .LE. 0.0) THEN
		PRINT *,'XS must be > 0.0 for Log-Log Plots'
		GO TO 900
	    ELSE IF (IXMAX .EQ. 1 .AND. XMAX .LE. 0.0) THEN
		PRINT *, 'XE must be > 0.0 for Log-Log Plots'
		GO TO 900
	    ELSE IF (XMAX .LE. 0.0) THEN
		PRINT *,'All X values are <= 0.0'
		PRINT *,'Cannot do a Log-Log Plot'
		GO TO 1000
	    ELSE IF (XMIN .LE. 0.0) THEN
C               Adjust XMIN to be the minimun positive X-value.
		DO 8711 I = 1, NTOPLOT, 1
		    XMIN = XPLOT(I)
		    IF(XPLOT(I) .GT. 0.0) GO TO 102
8711            CONTINUE
	    END IF

102         OLDXMIN = XMIN
	    OLDXMAX = XMAX
	    XMIN = ALOG10(XMIN)
	    XMAX = ALOG10(XMAX)

C           Load the logarithms of the X and Y values into plotting arrays
C           If a value of YPLOT <= 0.0, then YPLOT=(YMIN-4) in order to
C           keep things from blowing up in the user`s face.
            IF (IVEC.EQ.IVEC1) THEN
	      DO 8712 I = 1, NTOPLOT
                IF (XPLOT(I) .GT. 0) THEN
		  XPLOT(I) = ALOG10(XPLOT(I))
                ELSE
		  XPLOT(I) = XMIN - 4.0
                END IF
		IF ( YPLOT(I) .LE. 0.0 ) THEN
		    YPLOT(I) = YMIN - 4.0
		ELSE
		    YPLOT(I) = ALOG10(YPLOT(I))
		END IF
8712          CONTINUE
            ELSE
	      DO 8713 I = 1, NTOPLOT
		IF ( YPLOT(I) .LE. 0.0 ) THEN
		    YPLOT(I) = YMIN - 4.0
		ELSE
		    YPLOT(I) = ALOG10(YPLOT(I))
		END IF
8713          CONTINUE
            END IF
        ELSE IF (R4) THEN
C     Adjust the X-limits for R4 law
            IF (IXMIN .EQ. 0) XMIN = XMIN**0.25
            IF (IXMAX .EQ. 0) XMAX = XMAX**0.25

            OLDXMIN = XMIN
            OLDXMAX = XMAX

	ELSE
C       The plot is to be a Linear-Linear plot (default).
C       If the default YMIN is < 0, but the array values are all >0, set
C       YMIN = 0 (makes plots look good).  Do not do if YMIN user defined.
C       Save new value in OLDYMIN just incase user wants it later on.
	    IF (YMIN .LT. 0.0 .AND. YMN .GT. 0.0 .AND.
     &              IYMIN .EQ. 0) YMIN = 0.0
	    OLDYMIN = YMIN

	END IF

C---        ----- In this section, we actually make the plot -----

1001    CONTINUE
C   Reset the line style
	CALL PMGO(SETLTYPE)(LTYPE)

C   Setup the plotting window. Panes go from left-to-right and
C   from top-to-buttom (the opposite way of Mongo`s default).
	NW = NH*NV
        IF (.NOT. HAVEWINDOW) THEN
	  J = IMG - NW*IFIX(FLOAT(IMG-1)/FLOAT(NW))
	  IW = NH*(NV-1) + J - 2*NH*IFIX(FLOAT(J-1)/FLOAT(NH))
        END IF

	CALL PMGO(WINDOW)(NH,NV,IW)
        IF (XMIN .EQ. XMAX .OR. YMIN .EQ. YMAX) THEN
          PRINT *, 'Limits are in error: ', XMIN, XMAX, YMIN, YMAX
          XERR = .TRUE.
          RETURN
        END IF
	CALL PMGO(SETLIM)(XMIN,YMIN,XMAX,YMAX)

C   Overlay a grid on the plot if desired
	IF (LGRID) THEN
	    CALL PMGO(SETLWEIGHT)(0.5)
	    CALL PMGO(GRID) (1)
	    IF (HARD) THEN
		IF (VHARD .EQ. 5) THEN
		    CALL PMGO(SETLWEIGHT)(1.)
		ELSE
		    CALL PMGO(SETLWEIGHT)(WEIGHT)
		END IF
	    END IF
	END IF

C   Plot the data.
	IF (HIST) THEN
	    CALL PMGO(HISTOGRAM)(XPLOT,YPLOT,NTOPLOT)
	ELSE IF (POINT) THEN
	    CALL PMGO(POINTS)(STYLE,1,XPLOT,YPLOT,NTOPLOT)
        ELSE
	    CALL PMGO(CONNECT)(XPLOT,YPLOT,NTOPLOT)
	END IF
  
        IF (PARSEE) THEN
	    CALL PMGO(SETEXPAND)(0.)
            CALL PMGO(ERRORBAR)(2,XPLOT,YPLOT,EPLOT,NTOPLOT)
            CALL PMGO(ERRORBAR)(4,XPLOT,YPLOT,EPLOT,NTOPLOT)
        END IF

C   Go and plot the next row or column when necessary
        IF (IVEC1.NE.IVEC2 .AND. IVEC.LT.IVEC2) THEN
            IVEC = IVEC + 1
            GOTO 800
        END IF

C   Draw and label the plotting window when necessary. If the NOLABEL
C   option has been requested, draw a box without ticks or labels.
C   If units and pan are the same from the previous image in the
C   command line, do no bother drawing the box and labels again.
850	NEW = .TRUE.
        IF (IMG.GT.NW .AND. IXMIN.EQ.1 .AND. IXMAX.EQ.1 .AND.
     &      IYMIN.EQ.1 .AND. IYMAX.EQ.1) NEW = .FALSE.

	IF (DOLABEL.AND.NEW) THEN
C           Set the top label.
	    IF (MULT) THEN
	      CALL CHEAD('OBJECT',HEADMULT,TOPLABEL)
	    ELSE IF (PROFILE) THEN
	      CALL CHEAD('OBJECT',PRFHEAD(IPRFCOL),TOPLABEL)
	    ELSE IF (.NOT. PARSEX) THEN
	      CALL CCCHEAD('OBJECT',HEADBUF(1,IMBUF),TOPLABEL)
	      IF (VERT .EQ. ' ') CALL CCCHEAD('BUNIT',HEADBUF(1,IMBUF),VERT)
	      CALL CCCHEAD('STATUS',HEADBUF(1,IMBUF),STATUS)
	    END IF

C         X and Y labels
            IF (PARSEX .OR. HORZ .NE. ' ') THEN
                CONTINUE

	    ELSE IF (MULT .OR. PROFILE) THEN
		HORZ = 'Semi-major Axis (arcsec)'
		VERT = 'Surface brightness'

	    ELSE IF (USER) THEN
C               Try to label axis according to the FITS header.
		IF (ROWMASH) THEN
		    HORZ = CTYPE1(IMBUF)
                    VERT = CTYPE2(IMBUF)
		ELSE
		    HORZ = CTYPE2(IMBUF)
                    VERT = CTYPE1(IMBUF)
		END IF

		IF (LAMBDA.OR.ECHELLE) THEN
                    HORZ = 'Wavelength [mts]'
                ELSE IF (HORZ.EQ.' '.OR.HORZ(1:5).EQ.'PIXEL'
     &                   .OR.HORZ(1:3).EQ.'RAW') THEN
		    IF (ISSPECTRUM(IMBUF)) THEN
			HORZ = 'Pixel Number'
		    ELSE IF (ROW) THEN
			HORZ = 'Column Number'
		    ELSE IF (COL) THEN
			HORZ = 'Row Number'
		    END IF
		END IF

	    ELSE IF (LAMBDA) THEN
		HORZ = 'Wavelength [angs]'
#ifdef VMS
	        IF (VERT.EQ. ' ') VERT = 'F\d\gl'
#else
	        IF (VERT.EQ. ' ') VERT = 'F\\d\\gl'
#endif

	    ELSE IF (STATUS(1:7) .EQ. 'PROFILE') THEN
		HORZ = 'Semi-major axis'
		VERT = 'Mean surface brightness'

	    ELSE IF (ISSPECTRUM(IMBUF)) THEN
		HORZ = 'Pixel Number'

	    ELSE IF (COL) THEN
		HORZ = 'Row'

	    ELSE IF (ROW) THEN
		HORZ = 'Column'
		IF (ECHELLE) HORZ = 'Pixel Number'

	    END IF
	    IF (R4) THEN
              L = NUMCHAR(HORZ)
	      TEMPSTRING = FSTRCAT(HORZ(1:L),'\\u1/4')
	      HORZ = TEMPSTRING
	    END IF

            IF (PARSEY) THEN
                CONTINUE

	    ELSE IF (VERT.EQ. ' ') THEN
                VERT = 'Intensity'

            END IF

            IF (EXPAND .GT. 0) THEN
                CONTINUE
            ELSE IF (NW .GT. 4) THEN
                EXPAND = 0.4
            ELSE IF (NW .GT. 2) THEN
                EXPAND = 0.45
            ELSE
                EXPAND = 1.001
            END IF
	    CALL PMGO(SETEXPAND)(EXPAND)
            IF (DOXLABEL .OR. ((NH .GT. 1  .OR. NV .GT. 1) .AND. IW .LE. NH)) THEN
              IXLABEL = 1
            ELSE 
              IXLABEL = 0
            END IF
            IF (DOYLABEL) THEN
              IF (HARD) THEN
                IYLABEL = 2
              ELSE
                IYLABEL = 1
              END IF
            ELSE
              IYLABEL = 0
            END IF

            CALL PMGO(SETCOLOR)(1)
            IF (DOBOX) CALL PMGO(BOX)(IXLABEL,IYLABEL)
            CALL PMGO(SETCOLOR)(ICOLOR)

            IF (DOXTITLE)
     &	      CALL PMGO(XLABEL)(NUMCHAR(HORZ),HORZ(1:NUMCHAR(HORZ)))

            IF (DOYTITLE)
     &	      CALL PMGO(YLABEL)(NUMCHAR(VERT),VERT(1:NUMCHAR(VERT)))

            IF (DOTITLE)
     &	      CALL PMGO(TLABEL)(NUMCHAR(TOPLABEL),TOPLABEL(1:NUMCHAR(TOPLABEL)))

	    XTEXT = XMIN
	    YTEXT = YMAX + 0.025*(YMAX - YMIN)

	    CALL PMGO(SETEXPAND)(0.5)
	    CALL PMGO(RELOCATE) ( XTEXT, YTEXT )
	    IF (DOTITLE) 
     &        CALL PMGO(LABEL) (NUMCHAR(VPLOTID),VPLOTID(1:NUMCHAR(VPLOTID)))
	    CALL PMGO(SETEXPAND)(1.001)

	ELSE IF (DOBOX.AND.NEW) THEN
	    CALL PMGO(BOX)(0,0)

	END IF

C   Print wavelength calibration info if desired.  Info text is right justified
C   from 10% above and to the left of the lower right hand corner of the
C   plotting window.
	IF (LINFO .AND. .NOT. MULT .AND. .NOT. PROFILE) THEN
	    CALL CCINHEAD('OBSNUM',HEADBUF(1,IMBUF),OBSNUM)
	    CALL CCCHEAD ('DATE-OBS', HEADBUF(1,IMBUF),DATESTRING)
	    IF (ROWMASH) THEN
		DISPER = SNGL(CDELT1(IMBUF))
	    ELSE
		DISPER = SNGL(CDELT2(IMBUF))
	    END IF

	    IF (ABS(DISPER) .EQ. 1.) DISPER = 0.
	    IF (ABS(DISPER) .LT. 1.0E-4) DISPER = 1.0E10*DISPER

	    I = 1
110         IF  (I .LE. 80 .AND. DATESTRING(I:I) .EQ. ' ') THEN
		I = I+1
		GOTO 110
	    END IF

	    INFORM(1:1)  = '#'
	    WRITE(INFORM(2:6),'(I5)') OBSNUM
	    INFORM(7:8)  = '  '
	    INFORM(9:)   = DATESTRING(I:NUMCHAR(DATESTRING)-I+1)
	    WRITE(INFORM(20:25),'(F6.3)') DISPER
	    INFORM(26:30)= ' A/px'

	    XTEXT = XMAX - 0.10*(XMAX - XMIN)
	    YTEXT = YMIN + 0.10*(YMAX - YMIN)
	    CALL PMGO(SETEXPAND) (0.75)
	    CALL PMGO(RELOCATE) (XTEXT, YTEXT)
	    CALL PMGO(PUTLABEL) (NUMCHAR(INFORM), 
     &                          INFORM(1:NUMCHAR(INFORM)), 4)
	    CALL PMGO(SETEXPAND) (1.01)
	END IF

C--     Allow user to issue interactive MONGO commands (INT keyword),
C--     after plotting the present image. (must END mongo to continue)
C--     Execute the named MONGO macro if any.
900     IF (INTERACTIVE .OR. MACRO) THEN
	    MONGOCOM(1) = 'DELETE 1 1000'
	    IF (HARD) THEN
		MONGOCOM(2) = ' '
	    ELSE
		CALL PMGO(TIDLE)
		MONGOCOM(2) = 'TERMINAL   '
		WRITE(MONGOCOM(2)(10:11), '(I2)') VTERM
	    END IF
	    WRITE (MONGOCOM(3),'(''LINES 1 '',I5)') NTOPLOT
	    MONGOCOM(4) = 'XCOLUMN 1'
	    MONGOCOM(5) = 'YCOLUMN 2'
            IF (MACRO) THEN
              MONGOCOM(6) = FSTRCAT( 'INPUT ',MACROFILE)
            ELSE
              MONGOCOM(6) = ' '
            END IF
            IF (INTERACTIVE) THEN
              MONGOCOM(7) = ' '
            ELSE
              MONGOCOM(7) = 'END'
            END IF
	    CALL PMGO(MONGO)(7,MONGOCOM,NPLOT,2,ZPLOT)
	END IF

1000    CONTINUE
C-------- THIS WAS THE END OF THE MULTY-PLOTTER LOOP ----------------

C   Flush out remaining plot requests to complete current plotting task.
	IF (HARD .AND. .NOT. WAIT) THEN
	    N = PMGO(FILEPLOT)(0)
	    CLOSED = .TRUE.
	    IF (VHARD .NE. 5) PRINT *, 
     &          'Number of vectors plotted:', N

	ELSE IF (.NOT. HARD) THEN
	    CALL PMGO(TIDLE)
	    CLOSED = .TRUE.

	END IF

	DONE = .TRUE.
	RETURN
	END

C------------------------------------------------------------------C
	SUBROUTINE LOADVECTOR(A,ISR,IER,ISC,IEC,V,NV,
     &                         IR1,IR2,IC1,IC2,ROWMASH,NORM)
C------------------------------------------------------------------C
C Extracts columns, rows, a sum of rows or a sum of columns from   C
C 2D array A. If ROWMASH true, mashes columns IC1 to IC2 from row  C
C IR1 to IR2. Otherwise mashes rows ISR to IR2 from column IC1 to  C
C IC2. On output, array V returns the mashed vector.               C
C If NORM is true, normalize by number of rows/columns summed      C
C------------------------------------------------------------------C
	REAL*4 A(ISC:IEC,ISR:IER), V(NV)
	LOGICAL ROWMASH, NORM

C       Find the direction of the loops counters.
	ISIGNR = 1
	IF (IR1.GT.IR2) ISIGNR = -1
	ISIGNC = 1
	IF (IC1.GT.IC2) ISIGNC = -1

C       Mashing loops. Minimize page-faulting.
	K = 0
	IF (ROWMASH) THEN
           IF (NORM) THEN
             ANORM = ABS((IR2-IR1+ISIGNR))
           ELSE
             ANORM = 1.
           END IF
	   DO 8713 IC = IC1, IC2, ISIGNC
	       K = K + 1
	       V(K) = A(IC,IR1) / ANORM
8713       CONTINUE
	   DO 8714 IR = IR1+ISIGNR,IR2,ISIGNR
	       K = 0
	       DO 8715 IC = IC1, IC2, ISIGNC
		   K = K + 1
		   V(K) = V(K) + A(IC,IR) / ANORM
8715           CONTINUE
8714       CONTINUE

	ELSE
           IF (NORM) THEN
             ANORM = ABS(IC2-IC1+ISIGNC)
           ELSE
             ANORM = 1.
           END IF
	   DO 8716 IR = IR1, IR2, ISIGNR
		K = K + 1
		V(K) = 0.0
		DO 8717 IC = IC1, IC2, ISIGNC
		   V(K) = V(K) + A(IC,IR) / ANORM
8717            CONTINUE
8716       CONTINUE

	END IF
	RETURN
	END

C---------------------------------------------------------------------------

      SUBROUTINE PLOTSETUP(HARD,LAND,PSFILE,OLD,NOERASE,DOEPS,XERR)

C
C  PLOTSETUP - Generic LickMongo plotting device setup
C
C  Arguments:
C     hard (input, logical): if TRUE, plot is a hardcopy,
C     land (input, logical): if TRUE, plot orientation is landscape.
C                            Default orientation is portrait.
C     psfile (input, char): name of the output PostScript file
C     old (input, logical): if TRUE, use the old setup parameters
C     noerase (input, logical): if TRUE, do not erase on setup, 
C                               Default is to erase on setup.
C     doeps (input, logical): if TRUE, create an Encapsulated PostScript file
C                             Default is standalone PostScript.
C     xerr (output, logical): returns TRUE if an error was encountered on
C                             device startup.
C
C  Dependences:
C
C     This routine relies on the LickMongo subroutine package for
C     all plot-related calls.
C
C     PLOTSETUP is called by these subroutines:
C        source/display/plotter.F
C        source/display/conplot.F
C        source/display/histogram.F
C        source/display/overlay.F
C        source/display/radplot.F
C        source/display/threed.F
C        source/starphot/combine.F
C        source/starphot/fitstar.F
C        source/starphot/magaver.F
C        source/starphot/plotap.F
C
C  Authors:
C     J.J. Gonzalez & R.W. Pogge, Lick Observatory, 
C     1988 Dec 29
C
C  Modification History:
C
C     1995 Aug 14: Added the DOEPS logical & related code [rwp/osu]
C
C---------------------------------------------------------------------------

      LOGICAL HARD, LAND, OLD, NOERASE, DOEPS, XERR
      CHARACTER PSFILE*(*)
#ifdef __SUNVIEW
      CHARACTER TRANSLATION*80
#endif

      INTEGER VTERM, VHARD
      COMMON /VGRAPHICS/ VTERM, VHARD
      EXTERNAL TESTMON

      IF (HARD) THEN
C     Initialize hardcopy devices

C     Versatec V80 (note: Versatec acts backwards wrt PORT)
         IF (VHARD .EQ. 1) THEN
#ifdef _VersV80
            IF (LAND) THEN
               CALL PMGO(VTSWAPXY)
            ELSE
               CALL PMGO(VTSETUP)
            END IF
            IF (.NOT. OLD) CALL PMGO(RESET)
            CALL PMGO(SETLWEIGHT)(1.)
            CALL PMGO(FILEINIT)
#endif

C     Printronix (Note: "SWAPXY" in PRSWAPXY setup is a curious artifact)
         ELSE IF (VHARD .EQ. 2) THEN
#ifdef _Printronix
            CALL PMGO(PRSWAPXY)
            IF (.NOT. OLD) CALL PMGO(RESET)
            CALL PMGO(SETLWEIGHT)(1.)
            CALL PMGO(FILEINIT)
#endif

C     Imagen 8/300
         ELSE IF (VHARD .EQ. 3) THEN
#ifdef _Imagen
            IF (LAND) THEN
               CALL PMGO(IMSETUP)
            ELSE
               CALL PMGO(IMSWAP)
            END IF
            IF (.NOT. OLD) CALL PMGO(RESET)
            CALL PMGO(SETLWEIGHT)(1.)
            CALL PMGO(FILEINIT)
#endif

C     PostScript Metafile
         ELSE IF (VHARD .EQ. 5) THEN
            IF (DOEPS) THEN
               CALL PMGO(EPSPLOT)(LAND,PSFILE,' ')
            ELSE
               CALL PMGO(PSPLOT)(LAND,PSFILE,' ')
            END IF
            IF (.NOT. OLD) CALL PMGO(RESET)
            CALL PMGO(SETPSFMODE)(2)
            CALL PMGO(SETLWEIGHT)(0.5)

C     Tektronix Metafile (VMS only)
         ELSE IF (VHARD .EQ. 6) THEN
            CALL PMGO(DEVICE)(6)
            CALL PMGO(TSETUP)
            IF (.NOT. OLD) CALL PMGO(RESET)
            CALL PMGO(SETLWEIGHT)(1.)

         END IF

C     Initialize graphics display device
      ELSE
#ifdef __SUNVIEW
         IF (VTERM .EQ. 7) THEN
            CALL GETENV('WINDOW_ME',TRANSLATION)
            IF (NUMCHAR(TRANSLATION) .EQ. 0) THEN
               PRINT *, 'Could not open channel to Frame...'
               XERR = .TRUE.
               RETURN
            END IF
         END IF
#endif

         CALL PMGO(DEVICE) (VTERM)
         CALL PMGO(TSETUP)
#ifdef __VMS43
         IF (.NOT. OLD .AND. VTERM .NE. 12) CALL PMGO(RESET)
#endif
         IF (.NOT. OLD) CALL PMGO(RESET)
         IF (.NOT. NOERASE) CALL PMGO(ERASE)
         CALL PMGO(SETLWEIGHT)(1.)
#ifdef __SUNVIEW
         IF (VTERM .EQ. 7) CALL PMGO(SETEXPAND)(0.5)
#endif

         IF (VTERM .EQ. 11) THEN
           L = 1
           ISTAT = PTUV(regkdn)(l,testmon)
         END IF
         CALL PMGO(TIDLE)

      END IF

      RETURN
      END

        SUBROUTINE TESTMON(MWX, STATE, X, MWY, XPOS, YPOS, CHR)
        CHARACTER*(*) CHR

        IF (CHR(1:1) .GE. '1' .AND. CHR(1:1) .LE. '9') THEN
          NS = ICHAR(CHR(1:1)) - ICHAR('0')
          CALL SUBVAR('X',NS,XPOS,.TRUE.)
          CALL SUBVAR('Y',NS,YPOS,.TRUE.)
        END IF

C       print *, chr(1:1), xpos, ypos
C       print *, mwx, state, x, mwy, xpos, ypos, chr
        return
        end

