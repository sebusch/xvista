#include "Vista.h"
C       There are three logical variables which define how output has
C       been redirected.  Routines can check these variables and
C       modify their output according to the destination.  Only one of
C       the variables will be .TRUE. at a time.

C                       .TRUE. if a file is OPENed
	LOGICAL         ISOPEN
C                       .TRUE. if LPA0: file is open
	LOGICAL         ISLPT
C                       .TRUE. if TT: is OPENed
	LOGICAL         ISTT
	COMMON  /OUTPUTFLAGS/   ISOPEN, ISLPT, ISTT

