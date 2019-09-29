#include "Config.h"
C===============================================================================
	subroutine PTUV(mtmpfile)(name,ext)
c       create a unique file name with extension = ext
c       the directory in which the file is put is determined by MONGOSCR
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGOFIL.inc'
C       External Variables
c                       a (hopefully) unique file name with extension
	character*(*)   name
c                       the desired extension for the file name
	character*(*)   ext
C       Internal Variables
	integer         lenext, lendir
	character*24    dtstr
C       External Functions
	integer         PMGO(lenc)
C       Executable Code
c       find actual length of extension
	lenext = PMGO(lenc)(ext)
c       get the directory name & length
	lendir = PMGO(lenc)(MONGOSCR)
c       Check to see if the name will fit
	if (len(name) .lt. (lendir + 9 + 1 + lenext)) then
	    write(STDERR,*) ' Scratch file name does not fit.'
	    return
	endif
c       get the date and time in standard Lick Mongo format
	call PTUV(mdatim)(dtstr)
c       start assembling the file name
	name(1:lendir) = MONGOSCR
c       Get a (hopefully) unique filename from the system clock.
c       This is actually a rather dumb scheme.  On large systems with many
c       users we have already experienced collisions as two users begin a
c       hardcopy plot at the same second.
#ifdef  unix
	name(lendir+1:lendir+3) = '#mg'
#else   /* unix */
	name(lendir+1:lendir+3) = 'mgo'
#endif  /* unix */
	name(lendir+ 4:lendir+ 5) = dtstr(17:18)
	name(lendir+ 6:lendir+ 7) = dtstr(20:21)
	name(lendir+ 8:lendir+ 9) = dtstr(23:24)
c       finish assembling the filename
	name(lendir+10:lendir+10) = '.'
	name(lendir+11:lendir+11+lenext) = ext(1:lenext)
	return
	end
C===============================================================================
C===============================================================================
	subroutine PTUV(mdatim)(string)
c       fills string with the date and time expressed in a standard format
c       standard format is 'www YYYY MMM DD hh:mm:ss'
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variable
c                       a place to return the result
c                       should be at least 24 characters long
	character*(*)   string
C       Internal Variables
	character*32    tbuf
C       External Functions
#ifdef  unix
#ifndef Fctime
	character*24    ctime
	integer         time
c                       This is a feature(?!) required because the MIPS compiler
c                       is trying to use the VMS Fortran Intrinsic TIME()
c                       Gosh, thanks, guys, I really wanted that.
#ifndef GFORT
	external        ctime
	external        time
#endif
#endif  /* Fctime */
#else   /* unix */
#ifndef VMS
	you figure out how to get the date and time on your system
#endif  /* VMS */
#endif  /* unix */
C       Executable Code
#ifdef  unix
#ifdef  Fctime
	call PTUV(fctime)(tbuf)
#else   /* Fctime */
#ifdef  CVX
C       This module was modified by MB, 20-May-1992, to add Convex
C       functionality.  mb1@cfacx1.harvard.edu
	call fdate(tbuf)
#else   /* CVX */
	tbuf = ctime(time())
#endif  /* CVX */
#endif  /* Fctime */
	string = tbuf
	string(5: 8) = tbuf(21:24)
	string(9:24) = tbuf( 4:19)
#else   /* unix */
#ifdef  VMS
	string = '                        '
c       what time is it?
	call time(string(17:24))
	call date(tbuf)
c       here is a piece of code that will fail, but long after I am dead
c       (you do not honestly expect they will still be using VMS, do you?)
c       what century is it?
	if (tbuf(8:8) .ge. '7') then
	    string(5:6) = '20'
	else
	    string(5:6) = '19'
	endif
c       what year is it?
	string( 7: 8) = tbuf(8:9)
c       what month is it?
	string(10:12) = tbuf(4:6)
c       what date is it?
	string(14:15) = tbuf(1:2)
#else   /* VMS */
	again, you figure a way to get date and time
#endif  /* VMS */
#endif  /* unix */
	return
	end
C===============================================================================
C===============================================================================
