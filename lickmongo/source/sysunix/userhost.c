/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyrighted software.
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
#include "Config.h"
#include <pwd.h>
#ifdef SUNWspro
#include <sys/systeminfo.h>
#endif

UTUV(muserid)(string,lstr)
char    *string;
int     lstr;
{
    struct passwd *userpwd;
    int i;

    /* nullify the entire string, we are passing this back to Fortran */
    for (i=0; i<lstr; i++) {string[i] = '\0';}
    /* get this user's passwd structure */
    userpwd = getpwuid(getuid());
    /* copy the username into the output string */
    strcpy(string,userpwd->pw_name);
    /* find out how much room is taken in output string */
    i = strlen(string);
    /* put the '@' into the output string */
    string[i++] = '@';
    /* find out how much room is left in output string */
    lstr -= i;
    /* get the hostname and put it into the output string */
#ifdef SUNWspro
    sysinfo(SI_HOSTNAME,string+i,lstr);
#else
    gethostname(string+i,lstr);
#endif
    /* bye bye */
}
