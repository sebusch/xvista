C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **


C       Define where input, output, and errors come and go
	integer         STDIN,STDOUT,STDERR
	parameter       (STDIN = 5, STDOUT = 6)

	parameter       (STDERR = 0)




	character*(*)   ALINE
	parameter       (ALINE = '(a)')
	character*(*)   PROMPT
	parameter       (PROMPT = '(a,$)')

C       Define Fortran logical units Lick Mongo uses for various things
	integer         LUHELP,LUWRITMAC,LUREADMAC,LUREADATA,LUFONTS
	integer         LUPSOUT, LUPSIN
	parameter       (LUHELP    =  1)
	parameter       (LUWRITMAC =  1)
	parameter       (LUREADMAC =  1)
	parameter       (LUFONTS   = 17)
	parameter       (LUPSIN    = 17)
	parameter       (LUREADATA = 18)
	parameter       (LUPSOUT   = 19)

C       Define maximum number of 'input' recursion level allowed
	integer         MAXINPLEV
	parameter       (MAXINPLEV = 4)

C       Define fortran logical units Lick Mongo uses for 'input' recursion.
C       Lick Mongo does not use LUINPBASE, but it does use the
C       MAXINPLEV logical units greater than LUINPBASE.
C       See readinput.F for details.
	integer         LUINPBASE
	parameter       (LUINPBASE = 8)

C       Define the "modes" of operation of Lick Mongo.
C                       0 = Terminate Lick Mongo now!
	integer         TERMINATE
	parameter       (TERMINATE = 0)
C                       1 = Normal command input mode
	integer         NORMALCOM
	parameter       (NORMALCOM = 1)
C                       2 = Define_Macro mode
	integer         DEFINEMAC
	parameter       (DEFINEMAC = 2)
C                       3 = Insert_Commands mode
	integer         INSERTCOM
	parameter       (INSERTCOM = 3)

C                       Fortran I/O Status symbolic variables
	integer         SUCCESS
	parameter       (SUCCESS = 0)
	integer         EOF
	parameter       (EOF = -1)

