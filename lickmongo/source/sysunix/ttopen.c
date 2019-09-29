/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyrighted software.
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
#include "Config.h"
#include        <sgtty.h>
UTUV(cttopen)(file,fl)
char *file;
int fl;
{
	int i;
	char *cend;
	struct sgttyb *arg;

	cend = file;
	for(i=0; i<fl; cend++,i++)
		if(*cend == ' ') {
			*cend = '\0';
			break;
		}
	i = open(file,1);
/*      if this is to be exclusive use then     */
/*        if (i>=0 && exclu) ioctl(i,TIOCEXCL,arg);     */
	return(i);
}

UTUV(ttput)(fd,c,lc,csize)
int *fd;
char *c;
int *lc;
int csize;
{
	if ( write(*fd,c,*lc) != *lc ) {
		perror("incomplete write to ttchn");
	}
}

UTUV(ccclose)(ttchn)
int *ttchn;
{
	if ( close(*ttchn) ) perror("could not close ttchn");
}

