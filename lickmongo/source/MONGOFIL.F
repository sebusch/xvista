C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
#include "Config.h"

C       Define strings containing names of required files
	character       FONTBIN*(MXPATH)
	character       FONTDAT*(MXPATH)
	character       HELPFILE*(MXPATH)
	character       MONGOSCR*(MXPATH)
	character       MONGOPS*(MXPATH)
#ifdef  VMS
c       under VMS these are typically logical names
	parameter       (FONTBIN =  _FONTBIN    )
	parameter       (FONTDAT =  _FONTDAT    )
	parameter       (HELPFILE = _HELPFILE   )
	parameter       (MONGOSCR = _MONGOSCR   )
	parameter       (MONGOPS =  _MONGOPS    )
#else
c       under unix these are initialized in initializ.F
	common  /mongofiles/    FONTBIN, FONTDAT, HELPFILE, MONGOSCR
     &                          ,MONGOPS
#endif  /* VMS */
