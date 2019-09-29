#include "Vista.h"
C       Include stuff for PSFLIB software

#ifdef VMS
	INCLUDE '[-.starphot]daophot.inc'
#else
	INCLUDE '../starphot/daophot.inc'
#endif
