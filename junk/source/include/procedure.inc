


C       This include file defines the procedure buffer.

C      Length of procedure buffer:
	INTEGER         MAXPRO
	PARAMETER       (MAXPRO = 2000)
C      Maximum CALL depth:
	INTEGER         MAXSUB
	PARAMETER       (MAXSUB=10)
	CHARACTER*200   PROCEDURE(MAXPRO)
	COMMON  /VISTAPROCEDURE/        PROCEDURE

