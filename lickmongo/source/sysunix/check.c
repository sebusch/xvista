/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyright (c) 1991 Steven L. Allen
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
#include "Config.h"
#include <stdio.h>
#include "termio.h"

#define NUMWANT 1

UTUV(check)(ichr)

int     *ichr;
{
    static int fildes = -1;
    struct termios arg;                  /* initial state of terminal */
    static struct termios save_arg;      /* saved value of arg */
    int    numgot, i;
    char   chr[NUMWANT];

	if(fildes < 0) {
	    if ((fildes = open("/dev/tty",2)) < 0) {
		fprintf(stderr,"Can't open /dev/tty in check.\n");
		*ichr = -1;
	    }
	} else {
	    tcgetattr(fildes,&arg);
	    save_arg = arg;                     /* save the orig. mode */
#ifdef      Modern
	    arg.c_lflag &= ~ICANON;
	    arg.c_cc[VTIME] = 0;
	    arg.c_cc[VMIN] = 0;
#else       /* Modern */
	    arg.sg_flags |= CBREAK;
#endif      /* Modern */
	    /* set the new state of the tty (raw mode) */
	    tcsetattr(fildes,TCSADRAIN,&arg);
	    numgot = read(fildes,chr,NUMWANT);
	    if (numgot <= 0) {
		*ichr = -1;
	    } else {
		for(i = 0; i < NUMWANT; i++) {
		    chr[i] &= 0177;
		}
		*ichr = *chr;
	    }
	    /* reset the old state of the tty */
	    tcsetattr(fildes,TCSAFLUSH,&save_arg);
#ifdef      CLOSE_EVERY_TIME
	    close(fildes);
	    fildes = -1;
#endif      /* CLOSE_EVERY_TIME */
	}
	return *ichr;
}
