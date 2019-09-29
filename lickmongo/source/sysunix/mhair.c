/*
 *      This C file is Copyright (c) 1991,1994 Steven L. Allen

C  Permission is granted to anyone to make or distribute verbatim copies
C  of this source code as received, in any medium, provided that the
C  copyright notice and permission notice are preserved, and that the
C  distributor grants the recipient permission for further redistribution
C  as permitted by this notice.  Permission is granted to modify this
C  source code or any portion of it under the terms above, provided that
C  the nature of the modification is documented in the source with this
C  notice, and that all rights as permitted by this notice are granted
C  to subsequent recipients for similar modification of the source code.

This code is used by the Lick Mongo plotting package and by other
applications to read back the cursor position in Tektronix-compatible manner.
It resets the terminal characteristics such that the system should be
able to grab characters without the user needing to hit return.

These work on over a dozen different (and some aged) variants of Unix.
There are definitely better ways of couching the #ifdefs, and if I were
making Unix terminal driver hacking my specialty, I'd rewrite them.

It is up to the user of a particular system to determine which of the
#defines need to be turned on.  This would be done in "Config.h"

The interesting procedure name UTUV(mhair) is the Lick Mongo method for
avoiding namespace conflicts and appending underscores for Fortran as
is needed by a particular platform by using the CPP to rewrite the name.
This macro definition would also be done in "Config.h"

 */
#include "Config.h"
#include <stdio.h>
#include "termio.h"     /* you probably ought to look at this file */


UTUV(mhair)(chr,numtoget)
char    *chr;           /* a place to return characters obtained from the tty */
int     *numtoget;      /* how many characters to wait for before returning */
{
    static int fildes = -1;
    struct termios arg;                  /* initial state of terminal */
    static struct termios save_arg;      /* saved value of arg */
    int    numtot,numgot,i;

	if((fildes = open("/dev/tty",2)) < 0) {
	    fprintf(stderr,"Can't open /dev/tty in mhair.\n");
	    *chr = 27;
	} else {
	    /* find the current state of the tty */
	    tcgetattr(fildes,&arg);
	    /* save the current state of the tty */
	    save_arg = arg;
	    /* prepare the new state of the tty */
#ifdef      Modern
	    arg.c_iflag &= ~(IXON);
	    arg.c_lflag &= ~(ICANON | ISIG | ECHO);
	    arg.c_cc[VTIME] = 0;
	    arg.c_cc[VMIN] = 0;
#else       /* Modern */
	    arg.sg_flags |= RAW;
	    arg.sg_flags &= ~(ECHO);
#endif      /* Modern */
	    /* flush any pending input */
	    tcflush(fildes,TCIFLUSH);
	    /* set the new state of the tty (raw mode) */
	 /*
	  *     The problem here is finding something that really works
	  *     everywhere.  If the outgoing characters have not been
	  *     completely flushed, the terminal may return a control-S
	  *     as part of its serial communications protocol saying
	  *     "wait a minute, I gotta catch up".  When the new attributes
	  *     get set before the terminal has finished, and a control-S
	  *     is sent, it usually causes an apparent complete hangup of
	  *     the terminal, and incomplete graphics.
	  *
	  *     Suggestions are welcome.
	  *
	  *     Neither FLUSH nor DRAIN seems to do the job right, but
	  *     it is reported that DRAIN does much worse.
	  */
	    tcsetattr(fildes,TCSAFLUSH,&arg);
	 /* tcsetattr(fildes,TCSADRAIN,&arg); */
	    /* read the characters returned by crosshair keystroke */
	    numtot  = 0;
	    while (numtot < *numtoget) {
		numgot = read(fildes,chr+numtot,*numtoget-numtot);
		if (numgot < 0) break;
		numtot += numgot;
	    }
	    for(i = 0; i < *numtoget; i++) {
		chr[i] &= 0177;
	    }
	    /* reset the old state of the tty */
	    tcsetattr(fildes,TCSAFLUSH,&save_arg);
	    close(fildes);
	    fildes = -1;
	}
}
