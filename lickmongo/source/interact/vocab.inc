C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C------ MXCMBF is the compiled in limit on the number of characters in COMBUF
	integer         MXCMBF
	parameter       (MXCMBF = 50000)
C------ This is the number of built-in (compiled in) commands.
C       This should be the same as ((the number of non-blank initializations
C       for VOCAB) minus 1).  See the vocabdat.inc file.
	integer         NBUILTIN
	parameter       (NBUILTIN = 114)
C------ USERDEF is the number of user-definable macros that Lick MONGO allows.
	integer         USERDEF
	parameter       (USERDEF = 64)
C------ MAXCOM is the compiled in limit on the total number of built-in
C       commands plus macros.
C       Here we allow for USERDEF macros + the `all' macro.
	integer         MAXCOM
	parameter       (MAXCOM = NBUILTIN + USERDEF + 1)
C------ COMBUF is the buffer containing all of the commands and macros which
C       have been typed or read into Lick MONGO during an interactive session.
C       COMBUF contains the characters comprising each line, and also
C       stores one byte for each line to tell how long that line is.
C       Note that this implies that the maximum length of a Lick MONGO
C       command line is 126 characters.  This allows for a total length
C       of the segment of COMBUF (characters + the one byte giving the length)
C       to be 127--the largest number storeable in a signed byte.
C
C       The lower part of COMBUF contains the interactive commands
C       and this part grows upward like a stack.  When commands are
C       INSERTed or DELETEd all of the appropriate range of COMBUF
C       is recopied further up or down in memory to make or close up space.
C
C       The upper part of COMBUF contains the macros DEFINEd or read
C       into interactive Lick Mongo.  Although each line of any given macro
C       is stored in increasing order in memory, this upper part of COMBUF
C       grows downward line by line like a heap.
	character*1     combuf(MXCMBF)
C------ MACLEN(I) is the number of lines in macro #I.
C       macro #1 is the `ALL' macro, which is equivalent to the command buffer..
C       MACLEN(1) is the number of interactive commands in the command buffer.
	integer         maclen(MAXCOM)
C------ MACBEG(I) is the location in COMBUF at which macro #I begins.
C       Recall that the macros grow downward.
	integer         macbeg(MAXCOM)
C------ BUFEND is the location in COMBUF at which the next command will be put.
C       Recall that the command buffer grows up from BUFEND.
	integer         bufend
C------ MACEND is the location in COMBUF at which the next macro will be put.
C       Recall that the macros grow down from MACEND.
	integer         macend
C------ NMAC is the number of currently defined built-in commands plus
C       The number of macros that have been defined.
c>>>>>> actually there may be a coding error herein, NMAC is initialized to 1,
c>>>>>> and MAXCOM macros are allowed even though there is not enough room
c>>>>>> in VOCAB to hold all of the names.
	integer         nmac
C------ VOCAB is the array of understood commands, both built-in and macros.
	character       vocab(MAXCOM)*(LENCOM)
C------
	common /combuf/ macbeg, maclen, bufend, macend, nmac, combuf
	common /vocabcom/ vocab
