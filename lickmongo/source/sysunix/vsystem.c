/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyrighted software.
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
/*      Do a vfork() instead of a fork() so that the entire     */
/*      process space of Mnogo does not have to be duplicated.  */
/*      However, since the child is using the parents actual    */
/*      process space we can not close any of Mongo's open      */
/*      files until we do an execve().  Therefore, we execve()  */
/*      a little routine (dosystem) which closes all files,     */
/*      reopens the stdio files descriptors (0,1, and 2) to     */
/*      /dev/tty, and then execve's the routine we really want. */
#include "Config.h"
#include <stdio.h>

UTUV(vsystem)(file,lf)
char *file;                     /* Name of program to call      */
long lf;                        /* Size of the f77 character    */
				/* array.                       */
{
	int pid,child;
	char *argv[3];
	extern char **environ;
	static char *vista_system = "/mongodir/dosystem";
	char mycopy[100];
	int last,i;

/*      First make a local, null-terminated, copy of the file   */
/*      to be executed.                                         */

	last = lf;
	while(last > 0) {
		last--;
		if(file[last] == ' ')
			continue;
		else
			break;
	}
	if(last >= sizeof(mycopy)) {
		fprintf(stderr,"Command %30s too long\n",file);
		return(0);
	}
	for(i=0; i<=last; i++)
		mycopy[i] = file[i];
	mycopy[last+1] = '\0';          /* Null terminate the copy      */

/*      Now fork and execv()                                            */
	if((child=vfork()) == 0) {
		argv[0] = "dosystem";
		argv[1] = mycopy;
		argv[2] = (char *)0;
/*              Now overlay with desired file                   */
		execve(vista_system,argv,environ);
		fprintf(stderr,"CAN'T EXECVE %s to run %s\n",
			vista_system,mycopy);
		_exit(1);       /* Fatal if can't overlay       */
	}
	while(((pid = wait(0)) >= 0) && (pid != child)) {};

	return(child);
}

