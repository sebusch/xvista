/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyrighted software.
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
#include "Config.h"
#ifndef units_missing
UTUV(mongoedit)(fstring,fstrlen,junk)
char    *fstring;
int     *fstrlen;
int     junk;                   /* Unix Fortran calling standard        */
{
	char    command[80];
	char    *getenv();
	char    *mongoedit;

	/*      Search the environment for the editor of choice.        */
	if ((mongoedit = getenv("VISUAL")) == 0) {
		/* The default editor is vi.    */
		strcpy(command,"vi");
	} else {
		strcpy(command,mongoedit);
	}
	strcat(command," ");
	strncat(command,fstring,*fstrlen);
	system(command);
}
#else   /* units_missing */
	/* ar and ranlib get unhappy if this isn't done */
	UTUV(mongoedit)(){}
#endif  /* units_missing */
