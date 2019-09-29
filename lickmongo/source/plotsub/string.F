#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PMGO(gstring)(n,string)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External variables
	integer         n
	character*(*)   string
C       Parameter
	logical         DRAWIT
	parameter       (DRAWIT = .TRUE.)
C       Internal variables
	real            slength, xi, yi
C       External Functions
#ifdef  CHAR_NOT_CHAR
	character*1     char
#endif  /* CHAR_NOT_CHAR */
C       Executable Code
	if (expand .eq. 1. .and. angle .eq. 0. .and. termout
#ifdef  _AED512
     &  .and. numdev .ne. AED512
#endif  /* _AED512 */
#ifdef  _PostScript
     &  .and. (numdev .ne. PostScript .or. psfmode .ne. 0)
#endif  /* _PostScript */
     &  .and. index(string(1:n),char(92)) .eq. 0) then
	    xi = max(float(lx1),min(float(lx2),xp))
	    yi = max(float(ly1),min(float(ly2),yp+coff))
	    call PTUV(treloc)(xi,yi)
	    call PTUV(tchar)(n,string)
	    xp = xp + n * cwidth
	else
	    call PTUV(vstring)(n,string,drawit,slength)
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(gstrlen)(n,string,slength,sheight)
C       Find the length and height of a given string.
C       Please note that this routine does not really work if the
C       output device is a PostScript device.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External variables
	integer         n
	character*(*)   string
	real            slength,sheight
C       Parameter
	logical         DRAWIT
	parameter       (DRAWIT = .False.)
C       Internal variables
C       External Functions
#ifdef  CHAR_NOT_CHAR
	character*1     char
#endif  /* CHAR_NOT_CHAR */
C       Executable Code
	if (expand .eq. 1 .and. angle .eq. 0 .and. termout
#ifdef  _PostScript
     &  .and. (numdev .ne. PostScript .or. psfmode .ne. 0)
#endif  /* _PostScript */
     &  .and. index(string(1:n),char(92)) .eq. 0) then
#ifdef      _X11
	    if (numdev .eq. X11) then
		call PTUV(mx11txtwid)(n,string,slength)
	    else
#endif      /* _X11 */
		slength = n * cwidth
#ifdef          _PostScript
		slength = slength * expand
#endif          /* _PostScript */
#ifdef      _X11
	    endif
#endif      /* _X11 */
	else
	    call PTUV(vstring)(n,string,drawit,slength)
	endif
	sheight = cheight * expand
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(vstring)(n,string,drawit,slength)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGOFIL.inc'
C       External variables
	integer         n
	character*(*)   string
	logical         drawit
	real            slength
C       Parameters
c                       the escape character which signals a change in
c                       font or subscript or superscript
	character*1     ESC
	parameter       (ESC = AsciiBackSlash)
c                       This controls the amount of shear which is used to
c                       create pseudo-italic characters.
	real            SLANT
	parameter       (SLANT = 0.2)
c                       The fraction by which each level of subscript or
c                       superscript is shrunk.
	real            SUPFRAC
	parameter       (SUPFRAC = 0.6)
C       Internal variables
	integer         i, j
c                       has the binary Hershey font file been read yet?
	logical         indata
c                       should the "pen be lifted" while moving to next point?
	logical         relo
c                       number of consecutive backslashes encountered so far
	integer         icomm
c                       ASCII value of current character
	integer         jchar
c                       the character after a backslash
	character       escseq
c                       the line weight in effect when this routine was called
	real            lwsave
c                       used in computing shifts for super/sub-scripts
	real            hTB
c               Variables which affect the transformation from char to dev
c                       left and right character bearings
	integer         iladj, iradj
c                       linear ratio of (size [dev. coord])/(size [char. coord])
	real            qdef
c                       position and size of super/sub-script characters
	real            supshift,       shsave
	real            supfactor,      fasave
c               Variables indicating the state of the character generator
	integer         isuper,         issave
	integer         ifont,          ifsave
	logical         italic,         itsave
c               Character Coordinates (origin within each char)
	integer         ix, iy
c               Device Coordinates (also include XP, YP)
c                       position of current stroke w.r.t. Concatenation Pt.
	real            x, y
c                       width of current character
	real            xa
c                       coordinates of a Stroke Endpoint
	real            xc, yc
c                       coordinates of String Concatenation Point
	real            x0, y0
C       External Function
	logical         PTUV(readbfont)
	integer         PTUV(keyfont)
C       Common Blocks
	include 'fonts.par'
	include 'fonts.inc'
	include 'fonts.cmn'
C       Save statements
	save    indata, hTB
C       Data statements
	data    indata  /.FALSE./
C       Executable Code
	slength = 0.
	if (expand .le. 0.) return
c       -----------------------------------------------------------------------
	if (.not. indata) then
c           read from the binary font file FONTBIN
	    indata = PTUV(readbfont)()
	    hTB = (vline(TOP,0) - vline(BOT,0)) * 0.5 / (1. - SUPFRAC)
	endif
c       -----------------------------------------------------------------------
	lwsave = lweight
	x0 = xp
	y0 = yp
c       initialize the character generator
	icomm = 0
	italic = .false.
	itsave = italic
	ifont = 0
	ifsave = ifont
	isuper = 0
	issave = isuper
	supfactor = 1.
	fasave = supfactor
	supshift = 0.
	shsave = supshift
c       ****************************************
c       * \\x - set mode x for rest of string  *
c       * \x  - set mode x for next char only  *
c       ****************************************
c       * \r  - roman font                     *
c       * \g  - greek font                     *
c       * \s  - script font                    *
c       * \t  - tiny font                      *
c       * \i  - toggle italics                 *
c       ****************************************
c       * \u  - superscript                    *
c       * \d  - subscript                      *
c       * \b  - backspace                      *
c       * \e  - end string                     *
c       * \\\ - plot a backslash               *
c       ****************************************
	do 10 j = 1,n
	  if (string(j:j) .eq. ESC) then
	      icomm = icomm + 1
	      if (icomm .eq. 3) then
c                 we got 3 backslashes in a row, so actually plot a backslash
		  icomm = 0
		  goto 5
	      else
c                 go directly to look at the next character and act on it
c                 do not determine the size of or plot anything
		  goto 10
	      endif
	  endif
	  if (icomm .gt. 0) then
c             this is the character immediately following a backslash
c             figure out what kind of action it tells us to take
	      escseq = string(j:j)
	      call PTUV(lower)(escseq)
	      i = PTUV(keyfont)(escseq)
	      if (i .ge. numfont) then
		  write(STDERR,*) ' Sorry, # of fonts is only ', numfont
		  ifont = 0
	      elseif (i .ge. 0) then
		  ifont = i
c             elseif (escseq .eq. 'i') then
c                 italic = .not. italic
	      elseif (escseq .eq. 'u') then
		  isuper = isuper + 1
	      elseif (escseq .eq. 'd') then
		  isuper = isuper - 1
	      elseif (escseq .eq. 'e') then
		  goto 30
	      endif
	      if (escseq .eq. 'u' .or. escseq .eq. 'd') then
		  supfactor = SUPFRAC**iabs(isuper)
		  if (isuper .eq. 0) then
		      supshift = 0
		  else
		      supshift = hTB * ((1./supfactor) - 1.)
		      if (isuper .lt. 0) then
			  supshift = -SUPFRAC * supshift
		      endif
		  endif
	      endif
	      if (icomm .eq. 2) then
c                 the mode is being permanently reset, save this new setting
		  itsave = italic
		  ifsave = ifont
		  issave = isuper
		  fasave = supfactor
		  shsave = supshift
	      endif
c             reset the ESC counter to zero
	      icomm = 0
c             go directly to look at the next character and act on it
c             do not determine the size of or plot anything
	      goto 10
	  endif
c         control reaches here when we are actually going to determine the size
c         of a character and possibly plot it
5         continue
	  jchar = ichar(string(j:j))
c         find the left  sidebearing of the character
	  iladj = ichar(ladj(jchar,ifont))
	  if (iladj .gt. 127) iladj = iladj - 256
c         find the right sidebearing of the character
	  iradj = ichar(radj(jchar,ifont))
	  if (iradj .gt. 127) iradj = iradj - 256
	  qdef = expand * supfactor * cheight /
     &    (vline(TOP,ifont) - vline(BOT,ifont))
	  if (drawit) then
#           ifdef _PostScript
	    if (ipsopen) call PTUV(psclip)(.false.)
#           endif /* _PostScript */
	    relo = .true.
	    if (autolweight)
     &      call PMGO(setlweight)(qdef + lwsave)
	    do 20 i = 0,ichar(nstroke(jchar,ifont)) - 1
c             get the x position of this vertex
	      ix = ichar(font(pointer(jchar,ifont) + 2*i))
	      if (ix .gt. 127) ix = ix - 256
	      if (ix .eq. EOS) then
c                 the last point was the end of a stroke, now "lift the pen"
		  relo = .true.
	      else
c                 get the y position of this vertex
		  iy = ichar(font(pointer(jchar,ifont) + 2*i + 1))
		  if (iy .gt. 127) iy = iy - 256
c                 Transform this stroke from Character Coords to Device Coords
		  x = ix - iladj
		  if (italic) x = x + SLANT *
     &            (iy + (vline(HLF,ifont) - vline(BAS,ifont)))
		  x = qdef * x
		  y = qdef * (iy + supshift)
		  xc = cosang*x - sinang*y + x0
		  yc = sinang*x + cosang*y + y0
c                 Draw this stroke
		  if (relo) then
		      relo = .false.
		  else
		      call PMGO(line)(xp,yp,xc,yc)
		  endif
		  xp = xc
		  yp = yc
	      endif
20          continue
	  endif
c         Setup transformation for next character in the string
	  xa = qdef * (iradj - iladj)
	  slength = slength + xa
	  x0 = x0 + cosang*xa
	  y0 = y0 + sinang*xa
c         Because we may have just done a single character mode change,
c         we restore the saved permanent mode.
	  italic = itsave
	  ifont = ifsave
	  isuper = issave
	  supfactor = fasave
	  supshift = shsave
10      continue
30      continue
	if (drawit) then
c           Set current position to the end of the string
	    xp = x0
	    yp = y0
	    if (autolweight) call PMGO(setlweight)(lwsave)
	endif
	return
	end
C==============================================================================
C==============================================================================
