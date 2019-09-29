#include "Config.h"
C=======================================================================
C=======================================================================
	Subroutine PTUV(muserid)(string)
C       Get the username and hostname and return them in string.
c       This code was "based on an idea from Rick Pogge" and written by
c       Mark E. Cornell, Ph.D.            Internet: cornell@astro.utexas.edu
c       Software Support                            cornell@puck.as.utexas.edu
c       McDonald Observatory
c       University of Texas, RLM 15.308   Voice:    (512) 471-3423 or 471-3000
c       Austin, TX  78712-1083            FAX:      (512) 471-6016
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External variables
	Character       String*(*)
C       Parameters
	Include   '($JPIDEF)'
	Include   '($SYIDEF)'
C       Internal variables
	Integer         Ier
	Integer         LIB$GETJPI
	Integer         LIB$GETSYI
	Integer         Nchars1
	Integer         Nchars2
	Character       Buffer*256
	Character       Host*15
	Character       User*12
C       Executable Code
c       get the host
	Host = ' '
	Ier = LIB$GETSYI (SYI$_NODENAME, , Buffer, Nchars1, , )
	If (.NOT. Ier) Then
	    Host = 'NONE'
	    Nchars1 = 4
	Else
	    Host = Buffer(1:15)
	EndIf
c       get the user
	User = ' '
	Ier = LIB$GETJPI (JPI$_USERNAME, , , , Buffer, Nchars2)
	If (.NOT. Ier) Then
	    User = 'NONE'
	    Nchars2 = 4
	Else
	    User = Buffer(1:12)
	EndIf
c       put them together
	String = Host(1:Nchars1) // '::' // User(1:Nchars2)
	Return
	End
C=======================================================================
C=======================================================================
