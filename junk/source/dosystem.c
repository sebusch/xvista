/*
	RCS log information:

	$Log: dosystem.c,v $
	Revision 1.1.1.1  2001/02/14 04:30:56  holtz
	Starting CVS repository for xvista, source from avalon with executables,
	libraries, latex and latex2html output removed.
	
 * Revision 1.3  88/09/01  15:40:40  ccdev
 * Now open terminals with name returned by termname(0) instead of
 * using /dev/tty.  This makes the unix talk program happier.
 * R. Stover
 * 
 * Revision 1.2  87/11/04  13:01:24  ccdev
 * fixed RCS bug
 * 
 * Revision 1.1  87/11/04  09:54:20  ccdev
 * Initial revision
 * 
*/
#ifdef NOTDEF
#include <sys/errno.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <fcntl.h>
static char *rcsid=
	"$Header: /home/ganymede/holtz/CVS/xvista/source/dosystem.c,v 1.1.1.1 2001/02/14 04:30:56 holtz Exp $";

/*	Call a program for VISTA.  Since this routine is execved	*/
/*	after a vfork() all of the VISTA opened files are still		*/
/*	open here.  So we close them all down and reopen ones to	*/
/*	our terminal.							*/
main(argc,argv)
int argc;
char **argv;
{
	int i,nfd,i0,i1,i2;
	char *mytty,*ttyname();
        struct rlimit rlp;
#ifdef NOTDEF
        struct timeval ttout;  
        fd_set readfds[1024],writefds[1024],exceptfds[1024]; 
#endif

	argv++;
	mytty = ttyname(0);

#ifdef NOTDEF
        ttout.tv_sec = 0;
        i = select(nfd,readfds,writefds,exceptfds,&ttout); 
#endif

/*  Get number of possible file descriptors   */
#ifdef __DECSTA
        nfd = getdtablesize();		
#else
        getrlimit(RLIMIT_NOFILE,&rlp);
        nfd = rlp.rlim_cur;
#endif

	for(i=3; i<nfd; i++)
		close(i);		/* close all files		*/ 

/*	open(mytty,2);*/		/* Open 0 to tty		*/
/*	dup(0);	*/			/* Also 1			*/
/*	dup(0);	*/			/* Also 2			*/
/*for (i=0; i<argc; i++)
 fprintf(stderr,"%s\n",argv[i]); */
	system(*argv);			/* Make the call		*/
}

