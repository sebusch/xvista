#include "Vista.h"
	SUBROUTINE SIGCHANGE(IFLAG)

	EXTERNAL ESCAPE
#ifndef GFORT
	INTEGER SIGNAL
#endif

	IF (IFLAG .EQ. 0) THEN
#ifdef GFORT
 	  CALL SIGNAL(2,0)
#else
 	  ISTAT=SIGNAL(2,ESCAPE,1)
#endif
	ELSE
#ifdef GFORT
	   CALL SIGNAL(2,ESCAPE)
#else
	  ISTAT=SIGNAL(2,ESCAPE,-1)
#endif
	END IF

C	print *, 'sigchange, istat, iflag: ', istat, iflag

	RETURN
	END

	
