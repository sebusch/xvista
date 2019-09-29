#include "Vista.h"
/*	RCS log information:

	$Header: /home/ganymede/holtz/CVS/xvista/source/sysdep/unix/system.c,v 1.2 2001/05/28 20:17:53 holtz Exp $
	$Log: system.c,v $
	Revision 1.2  2001/05/28 20:17:53  holtz
	Fix in xprof for header, first attempt to make annulus work with mask and output
	profiles, other minor mods
	
	Revision 1.1.1.1  2001/02/14 04:31:08  holtz
	Starting CVS repository for xvista, source from avalon with executables,
	libraries, latex and latex2html output removed.
	
 * Revision 1.3  88/10/20  10:04:48  ccdev
 * Now use variable containing pathname of the dosystem program
 * instead of an expliciti path name.  The Variable is set in
 * initial.f.
 * R. Stover
 * 
 * Revision 1.2  87/11/04  12:54:52  ccdev
 * fixed RCS header
 * 
 * Revision 1.1  87/11/04  09:54:43  ccdev
 * Initial revision
 * 

*/


/*	Do a vfork() instead of a fork() so that the entire	*/
/*	process space of VISTA does not have to be duplicated.	*/
/*	However, since the child is using the parents actual	*/
/*	process space we can not close any of VISTA's open	*/
/*	files until we do an execve().  Therefore, we execve()	*/
/*	a little routine (dosystem) which closes all files,	*/
/*	reopens the stdio files descriptors (0,1, and 2) to	*/
/*	/dev/tty, and then execve's the routine we really want.	*/

/*	Added #ifdef FORT statements, JAH 11/88   */
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>

extern int is_tv_open;

#ifdef FORT
ATUS(systemc)(file,lf)
#else
systemc(file,lf)
#endif
char *file;			/* Name of program to call	*/
#ifdef FORT
int *lf;			/* Size of the f77 character	*/
#else
int lf;
#endif
				/* array.			*/
{
#ifdef __alpha
	pid_t pid,child;
#else
	int pid,child;
#endif
	char *argv[3];
	extern char **environ;
	extern char vistasystem_[];
	char mycopy[2000];
	int last,i,ATUS(loopdisp)();

/*	First make a local, null-terminated, copy of the file	*/
/*	to be executed.						*/
#ifdef FORT
	last = *lf;
#else
	last = lf;
#endif
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
	mycopy[last+1] = '\0';		/* Null terminate the copy	*/

/*	Now fork and execv()						*/
	if((child=vfork()) == 0) {
/*          Cancel the CTRL-C trap */
	        i = 0;
	        ATUS(sigchange)(&i);
		argv[0] = "dosystem"; 
		argv[1] = mycopy;
		argv[2] = (char *)0;
/*		Now overlay with desired file			*/
		execve(vistasystem_,argv,environ);
		fprintf(stderr,"CAN'T EXECVE %s to run %s\n",
			vistasystem_,mycopy);
		_exit(1);	/* Fatal if can't overlay	*/
	} 
        if (child == -1) return(-1);

/*	while(((pid = wait(0)) >= 0) && (pid != child)) {}; */
	while(((pid = waitpid(child,0,WNOHANG)) >= 0) && (pid != child)) {
          ATUS(loopdisp)() ;  
#ifdef usleep_exists
          usleep(10); 
#else
          sleep(1);
#endif

        };
	i = 1;
	ATUS(sigchange)(&i);
/* #endif */
	return(child);
}

