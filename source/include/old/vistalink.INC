#include "Vista.h"
C       ***     VISTA KEYWORDS, PARAMETERS, AND CONTROL         ***

C       This include file controls communication between VISTA and
C       its subroutines.

C       Parameters:     NCON            The maximum length of the VISTA
C                                       integer, constant, and keyword buffers.

	INTEGER NCON
	PARAMETER (NCON=200)

C       These variables pass constants and keywords to the subroutines.

C       Variable:       CONST           Floating constants buffer
C                       IBUF            Integer constants buffer
C                       WORD            Keywords and alphanumeric buffer
C                       COM             Current VISTA command
C                       COMMAND         Full input command line
C                       CURRENTLINE    Remaining commands to process before
C                                       seeking new command
	REAL            CONST(NCON)
	INTEGER         IBUF(NCON)
	COMMON /VISCON/ CONST, IBUF

	CHARACTER*2000  WORD(NCON), COM
	CHARACTER*2000 CURRENTLINE, COMMAND, PARAMLIST
	COMMON /VCOMMAND/ COMMAND, COM, WORD, CURRENTLINE, PARAMLIST

C       Number of integer, floating point, and alphabetic parameters found
C       on the command line:
	INTEGER         NINTS
	INTEGER         NFLTS
	INTEGER         NWORD
	COMMON /VPCOUNTS/ NINTS , NFLTS, NWORD

C       These logical variables control subroutine program flow.

C       Variable:       GO      Set by VISTA to start the subroutine
C                       XERR    Set .TRUE. in subroutines when an error
C                               occurs during execution.  If the error
C                               is severe enough, the routine should
C                               imediately return to VISTA.
C                       NOGO    Set .TRUE. when Ctrl-C is entered from
C                               the keyboard.  Subroutines can check
C                               this at any desired point, and return
C                               if set.
C                       PROC    Set .TRUE. if the command is being
C                               run from a procedure.
C                       XEOF    Set .TRUE. when an EOF occurs on an external
C                               data file read.
	LOGICAL GO, XERR, NOGO, PROC, XEOF
	COMMON /CON/ GO, XERR, NOGO, PROC, XEOF

C       Finally, we have a common which carries around the number of the
C       logical unit opened for writing. We need to do this to get things
C       to work properly in batch mode on the Lick SUNs. Holtz. 3/89

	INTEGER OLU
	COMMON /OUTPUTUNIT/ OLU

C       Bell with prompt
        CHARACTER*1      BELL
        COMMON /VISBELL/ BELL

C       Author: Tod R. Lauer            11/5/82
C               Donald Terndrup         2/21/85         Version 3
C               Richard Stover          10/10/85        XEOF added
