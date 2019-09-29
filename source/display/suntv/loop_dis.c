/*	Routine to dispatch window manager from VISTA			*/

/*	This routine writes to the internal pipe and immediately reads
	from it.  During the read, the window manager will service any
	screen inputs.  This permits the window manager to fix up the
	display screen while VISTA is CPU bound				*/

/*	In actual use, this routine should be inserted in all programs
	that are CPU bound so that window fixup can occur.  In image
	processing subroutines, it is typically called once for
	each ROW index DO loop, which means that the process is never
	more than one ROW processing time away from fixxing up the
	window.								*/

/*	Author:	Tod R. Lauer	4/11/86
	Princeton University Observatory				*/

loopdisp_()

{
	extern int to_vista,
		   from_screen;
	char buf[2];

	if (to_vista && from_screen) {
		write (to_vista,buf,1);
		read (from_screen,buf,1);
	}
}
