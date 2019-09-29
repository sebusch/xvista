C       SAVED EXPRESSION PARAMETERS

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

	INTEGER         MAXEXPR
	PARAMETER       (MAXEXPR=20)

	CHARACTER*10    EXPRNAME(MAXEXPR)
	CHARACTER*256   EXPRSTRING(MAXEXPR)

	COMMON  /EXPRESSIONDATA/        EXPRNAME,EXPRSTRING


