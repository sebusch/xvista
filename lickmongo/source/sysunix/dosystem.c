/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyrighted software.
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
/*
	RCS log information:

	$Log: dosystem.c,v $
	Revision 1.1.1.1  2001/02/14 04:31:22  holtz
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
static char *rcsid=
	"$Header: /home/ganymede/holtz/CVS/xvista/lickmongo/source/sysunix/dosystem.c,v 1.1.1.1 2001/02/14 04:31:22 holtz Exp $";

/*      Call a program for VISTA.  Since this routine is execved        */
/*      after a vfork() all of the VISTA opened files are still         */
/*      open here.  So we close them all down and reopen ones to        */
/*      our terminal.                                                   */
main(argc,argv)
int argc;
char **argv;
{
	int i,nfd;
	char *mytty,*ttyname();

	argv++;
	mytty = ttyname(0);
	nfd = getdtablesize();          /* Get number of possible       */
					/* file descriptors.            */
	for(i=0; i<nfd; i++)
		close(i);               /* close all files              */
	open(mytty,2);                  /* Open 0 to tty                */
	dup(0);                         /* Also 1                       */
	dup(0);                         /* Also 2                       */
	system(*argv);                  /* Make the call                */
}

