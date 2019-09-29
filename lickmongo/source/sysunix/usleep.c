/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyrighted software.
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
#include "Config.h"
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

/* sleep for a given number of microseconds */
UTUV(usleep)(utime)
float *utime;
{
#ifdef  usleep_exists
	usleep((unsigned)(*utime));
#else
	sleep((unsigned)(*utime / 1000000.));
#endif  /* usleep_exists */
}


#ifdef  IBMAIX
int ATUS(flush)(Funit)
int *Funit;
{
}

#endif /* IBMAIX */

#ifdef Fctime
/* replacement for Fortrans which have no hooks into Unix ctime or time */
/* calling a function which returns a string from XLFortran seems impossible */
void UTUV(fctime)(buf,lbuf)
char *buf;
int lbuf;
{
    time_t t;

    t = time(&t);
    strncpy(buf,ctime(&t),lbuf);
}
#endif  /* Fctime */

#ifdef Fperror
/*  replacement for Fortrans which have no hook into Unix perror */
void ATUS(perror)()
{
	perror(" ");
}
#endif  /* Fperror */

#ifdef Fchdir

/* replacement for Fortrans which have no hook into Unix chdir */
int ATUS(chdir)(buf,lbuf)
char *buf;
int lbuf;
{
    char path[1024];    /* assume this is long enough */

    strncpy(path,buf,lbuf);
    path[lbuf] = '\0';
    return chdir(path);
}
#endif /* Fchdir */
