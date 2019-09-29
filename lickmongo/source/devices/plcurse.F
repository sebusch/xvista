#include "Config.h"
C       These are interactive cursor routines
C==============================================================================
C==============================================================================
	subroutine PMGO(plcurse)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
C       Parameters
	integer         NPMX
	parameter       (NPMX=100)
#ifdef  VMS
	integer         INVRS, LINE1, COL40
c       Inverse Video for LIB$PUT_SCREEN, and also...
	parameter       (INVRS = 2)
c       Put text on line 1 of terminal
	parameter       (LINE1 = 1)
c       Put text in column 40 of terminal
	parameter       (COL40 = 40)
#endif  /* VMS */
C       Internal Variables
	logical         error
	character*40    line
	integer         key, keyc
	real            ux, vy
	integer	        ierr
C       Common Block
c       PL_CUR contains an array (CURPOS) with cursor positions. So if you
c       call PLCURSE from PLSUB, you must have this common in your main program,
c       if you want the cursor positions to be passed back
c
	integer         ncpos
	integer         curpos(2,NPMX)
	character*1     skeys(NPMX)
	common  /plcur/ ncpos, curpos, skeys
C       External Functions
#ifdef  CHAR_NOT_CHAR
	character*1     char
#endif  /* CHAR_NOT_CHAR */
	integer PMGO(mongohairs)
C       Executable Code
	if (.not. termout) then
	    write(STDERR,*) ' No cursor for this device'
	    return
	endif
	ncpos = 0
	keyc = -1
c
	if ( .false.
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640 */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603 */
#ifdef  _GSI
     &  .or. (numdev .eq. GSI)
#endif  /* _GSI */
#ifdef  _AED512
     &  .or. (numdev .eq. AED512)
#endif  /* _AED512 */
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010 */
#ifdef  _ISIOVW
     &  .or. (numdev .eq. ISIOVW)
#endif  /* _ISIOVW */
#ifdef  _uVAXUIS
     &  .or. (numdev .eq. uVAXUIS)
#endif  /* _uVAXUIS */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140 */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx */
#ifdef  _X11
     &  .or. (numdev .eq. X11)
#endif  /* _X11 */
#ifdef  _SunView
     &  .or. (numdev .eq. SunView)
#endif  /* _SunView */
     &  ) then
	  if (.false.) then
C             continue
#ifdef    _uVAXTEK
	  elseif (numdev .eq. uVAXTek) then
	      write(STDERR,*) ' Use cycle <F5> (^[ to Exit)'
c             write(STDERR,*) ' or hit Mouse Button to Exit'
#endif    /* _uVAXTEK */
#ifdef    _ISIOVW
	  elseif (numdev .eq. ISIOVW) then
	      write(STDERR,*)
     &        ' <ESC> to exit'
#endif    /* _ISIOVW */
#ifdef    _GSI
	  elseif (numdev .eq. GSI) then
	      write(STDERR,*)
     &        ' <ESC> to exit, do not use trackball buttons!'
#endif    /* _GSI */
#ifdef    _X11
	  elseif (numdev .eq. X11) then
	      write(STDERR,*)
     &        ' <ESC> to exit, you ain''t seen nothin'' yet'
#endif    /* _X11 */
#ifdef    _SunView
	  elseif (numdev .eq. SunView) then
	      write(STDERR,*)
     &        ' <ESC> to exit'
#endif    /* _SunView */
	  else
	      write(STDERR,*) ' <ESC> or ^[ TO EXIT'
	  endif
10        if (keyc .ne. 27) then
	    ierr = PMGO(mongohairs)(key,ux,vy)
c           MONGOHAIRS may return a value > 127 from some workstations
c           but the low order 2 bytes will be some kind of key code
c           of which ASCII is a subset
	    keyc = key
	    if (.false.
#           ifdef _uVAXUIS
     &      .or. numdev .eq. uVAXUIS
#           endif /* _uVAXUIS */
#           ifdef _SunView
     &      .or. numdev .eq. SunView
#           endif /* _SunView */
     &      ) then
#               ifdef VMS_LOG_OPS
		keyc = keyc .and. 65535
#               else /* VMS_LOG_OPS */
#               ifdef Unix_LOG_OPS
		keyc = and(keyc,65535)
#               else /* Unix_LOG_OPS */
		keyc = iand(keyc,65535)
C		insert code for bitwise AND here
#               endif /* Unix_LOG_OPS */
#               endif /* VMS_LOG_OPS) */
	    endif
	    if (keyc .ne. 27) then
		line(1:8) = ' Cursor:'
		if (keyc .ge. 32 .and. keyc .lt. 127) then
		    line( 9:10) = ' `'
		    line(11:11) = char(keyc)
		    line(12:13) = ''' '
		elseif (keyc .ge. 0) then
		    write(line(9:13),'(i5.5)') keyc
		else
		    write(line(9:13),'(i5)') keyc
		endif
		write(line(14:38),'('' '',G11.4,''  '',G11.4)') ux,vy
c               REPLACE this line with 'if (ANSI) then' when ANSI flag is done
		if ( .false.
#ifdef          _GO140
     &          .or. (numdev .eq. GO140)
#endif          /* _GO140 */
#ifdef          _GO2xx
     &          .or. (NUMDEV .eq. GO2xx)
#endif          /* _GO2xx */
#ifdef          _X11
     &          .or. (numdev .eq. X11)
#endif          /* _X11 */
#ifdef          _Tk4010
     &          .or. (numdev .eq. Tk4010)
#endif          /* _Tk4010 */
#ifdef          _AED512
     &          .or. (numdev .eq. AED512)
#endif          /* _AED512 */
     &          ) then
		    write(STDERR,*) line
		else
#ifdef              _xterm
		    call PMGO(tidle)
#endif              /* _xterm */
#ifdef              VMS
		    call LIB$PUT_SCREEN(line,LINE1,COL40,INVRS)
#else               /* VMS */
		    write(STDERR,*) line
#endif              /* VMS */
		endif
c               Now send position to common block PL_CUR
		ncpos = ncpos+1
		if (ncpos .gt. NPMX) then
		    keyc = 27
		    write(STDERR,*)
     &              'No more than 100 cursor positions allowed'
		else
		    skeys(ncpos) = char(keyc)
		    curpos(1,ncpos) = ux
		    curpos(2,ncpos) = vy
		endif
	    elseif ( .false.
#ifdef      _GO140
     &      .or. (numdev .ne. GO140)
#endif      /* _GO140 */
     &      ) then
#ifdef          VMS
		call LIB$ERASE_LINE(LINE1,COL40)
		call LIB$SET_CURSOR(23,1)
#endif          /* VMS */
	    endif
	    goto 10
	  endif
	else
	    write(STDERR,*)
     &      ' Sorry, only for termtypes 1,3,4,7,8,10,11,12,13,14'
	endif
c
c       Finally, relocate to last cursor position (and put this command into
c       command buffer for use by PLAYBACK)
	call PMGO(relocate)(ux,vy)
	line(1:11) = 'relocate   '
	write(line(12:40),'(G11.4,''  '',G11.4,''     '')') ux,vy
	call PTUV(saveline)(line,error)
	return
	end
C==============================================================================
C==============================================================================
	integer function PMGO(mongohairs)(ichr,xu,yu)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       External Variables
c                       ASCII char used to terminate crosshair mode
	integer         ichr
c                       user coordinates of crosshair
	real            xu,yu
c       Internal Variables
c                       needed for compatibility with X11 extra info
	integer         ista
	integer         idn
	integer         ierr
C	External Functions
	integer		PTUV(mgocursor)
c       Executable Code
	PMGO(mongohairs) = 0
	idn = 1
	ierr = PTUV(mgocursor)(ichr,xu,yu,ista,idn)
	return
	end
C==============================================================================
C==============================================================================
	integer function PTUV(mgocursor)(ichr,xu,yu,istate,idown)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
c       External Variables
c                       ASCII char used to terminate crosshair mode
	integer         ichr
c                       user coordinates of crosshair
	real            xu,yu
c                       state info from X11 xbutton or xkey event structure
	integer         istate
c                       input:  nonzero if we want only key down events
c                       return: nonzero if this was a key down event
	integer         idown
c       Internal Variables
c                       position of crosshairs in Tektronix units
	integer         ix,iy
c                       some of the low-level routines return character
	character*1     achar
	integer         ierr
C	External Functions
	integer		PTUV(mcrosshair)
c       Executable Code
	PTUV(mgocursor) = 0
	if (.false.) then
c           continue
#       ifdef _NDPGREX
	elseif (numdev .eq. NDPGREX) then
c           80386 pc with NDP-386 fortran
	    call PTUV(pccur)(achar,ix,iy)
	    ichr = ichar(achar)
#       endif /* _NDPGREX */
#       ifdef _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(mgsicur)(ichr,ix,iy)
#       endif /* _GSI */
#       ifdef _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aecursor)(achar,ix,iy)
	    iy = 511 - iy
	    ichr = ichar(achar)
#       endif /* _AED512 */
#       ifdef _ISIOVW
	elseif (numdev .eq. ISIOVW) then
	    call PTUV(iswindowcur)(ichr,ix,iy)
#        endif /* _ISIOVW */
#       ifdef _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
	    call PTUV(uiscur)(ichr,ix,iy)
#       endif /* _uVAXUIS */
#       ifdef _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11curs)(ichr,ix,iy,istate,idown)
#       endif /* _X11 */
#       ifdef _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(suncurs)(ichr,ix,iy)
#       endif /* _SunView */
	elseif (.false.
#       ifdef _DR640
     &  .or. numdev .eq. DR640
#       endif /* _DR640 */
#       ifdef _Tk4010
     &  .or. numdev .eq. Tk4010
#       endif /* _Tk4010 */
#       ifdef _Vis603
     &  .or. numdev .eq. Vis603
#       endif /* _Vis603 */
#       ifdef _xterm
     &  .or. numdev .eq. xterm
#       endif /* _xterm */
#       ifdef _uVAXTek
     &  .or. numdev .eq. uVAXTek
#       endif /* _uVAXTek */
#       ifdef _GO2xx
     &  .or. numdev .eq. GO2xx
#       endif /* _GO2xx */
#       ifdef _GO140
     &  .or. numdev .eq. GO140
#       endif /* _GO140 */
     &  ) then
	    ierr = PTUV(mcrosshair)(ichr,ix,iy)
	else
c           this device does not have crosshairs, just read term for a char
	    call PTUV(mhair)(achar,1)
	    ichr = ichar(achar)
	endif
c       convert back from Tektronix units into true terminal units
	if (.false.) then
c           continue
#       ifdef _NDPGREX
	elseif (numdev .eq. NDPGREX) then
c           80386 pc with NDP-386 fortran
	    xp = ix
	    yp = LY2 - iy
#       endif /* _NDPGREX */
#       ifdef _DR640
	elseif (numdev .eq. DR640) then
c           DEI Retro-Graphics VT100
	    xp = ix * (5. / 8.)
	    yp = iy * (8. / 13.)
#       endif /* _DR640 */
#       ifdef _GO140
	elseif (numdev .eq. GO140) then
c           GraphOn-140
	    xp = ix / 2.
	    yp = iy / 2.
#       endif /* _GO140 */
#       ifdef _Vis603
	elseif (numdev .eq. Vis603) then
	    xp = ix
	    yp = iy / 2.
#       endif /* _Vis603 */
	else
c           true Tektronix emulator
	    xp = ix
	    yp = iy
	endif
	xu = (xp-gx1)*(x2-x1)/(gx2-gx1) + x1
	yu = (yp-gy1)*(y2-y1)/(gy2-gy1) + y1
	return
	end
C==============================================================================
C==============================================================================
	integer function PTUV(mcrosshair)(ascchar,ix,iy)
c       enters and exits crosshair mode getting the character and position
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
c       External Variables
c                       ASCII value of character struck to exit xhair mode
	integer         ascchar
c                       device coordinates of hairs (0<=ix<=1023,0<=iy<=779)
	integer         ix,iy
c       Internal Variables
c                       place to receive crosshair response
	character*5     chr
	character       vtcur(3)
#ifdef VMS_CHAR_INIT
        vtcur(1) = char(GS__DATA)
        vtcur(2) = char(ESC_DATA)
        vtcur(3) = char(SUB_DATA)
#else
	data    vtcur   /GS__DATA, ESC_DATA, SUB_DATA/
#endif

c       Executable Code
	PTUV(mcrosshair) = 0
#ifdef  _xterm
	if (numdev .eq. xterm) call PTUV(xmode)(1)
#endif  /* _xterm */
#ifdef  _Vis603
	if (numdev .eq. vis603) call PTUV(visual)(1)
#endif  /* _Vis603 */
	call PTUV(ttwrite)(3,vtcur)
#ifdef  BROKEN_ENTRY
	call PTUV(ttflush)(3,vtcur)
#else   /* BROKEN_ENTRY */
	call PTUV(ttflush)
#endif  /* BROKEN_ENTRY */
	call PTUV(mhair)(chr,5)
	call PMGO(tidle)
#ifdef  BROKEN_ENTRY
	call PTUV(ttflush)(3,vtcur)
#else   /* BROKEN_ENTRY */
	call PTUV(ttflush)
#endif  /* BROKEN_ENTRY */
c       dissect the string received from the terminal
	ascchar = ichar(chr(1:1))
#ifndef ISI_gf77
	ix = mod(ichar(chr(2:2)),32)*32 + mod(ichar(chr(3:3)),32)
	iy = mod(ichar(chr(4:4)),32)*32 + mod(ichar(chr(5:5)),32)
#else   /* ISI_gf77 */
C       several of the ISI compilers have had trouble with MOD(,)
	ix = and(ichar(chr(2:2)),31)*32 + and(ichar(chr(3:3)),31)
	iy = and(ichar(chr(4:4)),31)*32 + and(ichar(chr(5:5)),31)
#endif  /* ISI_gf77 */
	return
	end
C==============================================================================
C==============================================================================
	integer function PMGO(sethairs)(xu,yu)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
c       External Variables
c                       world coordinates of crosshair
	real            xu,yu
c       Internal Variables
	integer*4       ix,iy
	character       bytes(16)
	character*14    string
	integer*4       i
C       External Functions
#       ifdef  CHAR_NOT_CHAR
	character*1     char
#       endif  /* CHAR_NOT_CHAR */
C       Executable Code
	PMGO(sethairs) = 0
	if ( .true.
#       ifdef  _DR640
     &  .and. numdev .ne. DR640
#       endif  /* _DR640 */
#       ifdef  _Vis603
     &  .and. numdev .ne. Vis603
#       endif  /* _Vis603 */
#       ifdef  _GO2xx
     &  .and. numdev .ne. GO2xx
#       endif  /* _GO2xx */
#       ifdef  _X11
     &  .and. numdev .ne. X11
#       endif  /* _X11 */
     &  ) return
c       compute device coordinates (and relocate by so doing)
	xp = (xu-x1)*(gx2-gx1)/(x2-x1) + gx1
	yp = (yu-y1)*(gy2-gy1)/(y2-y1) + gy1
c       convert to integer and clip
	ix = min(max(nint(xp),lx1),lx2)
	iy = min(max(nint(yp),ly1),ly2)
c       convert device coordinates into Tektronix coordinates
	if ( .false. ) then
	    continue
#       ifdef  _DR640
	elseif (numdev .eq. DR640) then
	    ix = (16*ix+5)/10
	    iy = (13*iy+4)/8
#       endif  /* _DR640 */
#       ifdef  _Vis603
	elseif (numdev .eq. Vis603) then
C           Ix = ix
	    Ix = 2 * iy
#       endif  /* _Vis603 */
	endif
	if ( .false. ) then
	    continue
#       ifdef _X11
	elseif (numdev .eq. _X11) then
	    call PTUV(mx11warp)(ix,iy)
#       endif /* _X11 */
#       ifdef  _GO2xx
	elseif (numdev .eq. GO2xx) then
	    write(string,'(a5,i4,a1,i3,a1)')
     &      char(27)//'[10;',ix,';',iy,'v'
	    do 10 i = 1,14
	      bytes(i) = string(i:i)
10          continue
	    call PTUV(ttwrite)(14,bytes)
#       endif  /* _GO2xx */
	else
	    bytes(1) = char(29)
c           split up into high and low order bytes in Tektronix format
	    bytes(4) = char(IX/32 + 32)
	    bytes(2) = char(IY/32 + 32)
#           ifdef      ISI_gf77
C           several of the ISI compilers have had trouble with MOD(,)
	    bytes(5) = char(and(IX,31) + 64)
	    bytes(3) = char(and(IY,31) + 96)
#           else       /* ISI_gf77 */
	    bytes(5) = char(MOD(IX,32) + 64)
	    bytes(3) = char(MOD(IY,32) + 96)
#           endif      /* ISI_gf77 */
c           add code to command Retro to set crosshair location
c           <ESC>
	    bytes(6) = char(27)
	    bytes(7) = '/'
	    bytes(8) = 'f'
	    call PTUV(ttwrite)(8,bytes)
	endif
	call PMGO(tidle)
	return
	end
C==============================================================================
C==============================================================================
