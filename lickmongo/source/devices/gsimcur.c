/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyrighted software.
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
#include "Config.h"
/*      This routine reads the cross hair position from the GSI display */
/*      It is device and system dependent, but so is all the GSI code   */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sgtty.h>
#include <sys/file.h>
#include <sys/time.h>
#include <setjmp.h>
#include <stdio.h>
/* #include "/u/ccdev/isi/term/msgs.h" */
#include "/u/unix/gsi/socket/trackmon.h"

#define VMEM_SIZE 1024          /* Size of GSI video memory     */
static int coli,rowi;           /* Image offsets                */

static int lastr, lastc, frstr, frstc;
char cbuf[256];
static int funix = -1;

UTUV(mgsicur)(ichr,ix,iy)
int *ichr,*ix,*iy;
{
	short shorty;
	int one = 1, half_k = 512;
	int nr,nc;
	struct tv_picture tvp;
	int i,n,nw;
	struct sockaddr_un sun;
	char *cp;
	int y,x;

	if (funix < 0) {
		funix = socket(AF_UNIX,SOCK_STREAM,0);
		if(funix < 0) {
			perror("Can't get socket");
			exit(1);
		}
		sun.sun_family = AF_UNIX;
		strcpy(sun.sun_path,"/u/ccd/gsisocket");
		if(connect(funix,&sun,strlen(sun.sun_path)+2) < 0) {
			perror("Can't connect");
			exit(2);
		}
	}

	nr = 1024;                       /* # rows               */
	nc = 1024;                       /* # columns            */

	coli = (VMEM_SIZE-nc)/2;
	if(coli & 01)
		coli++;
	rowi = (VMEM_SIZE-nr)/2;

	tvp.pic_rowi = rowi;
	tvp.pic_coli = coli;
	tvp.pic_frstr = -479;
	tvp.pic_frstc = 0;
	tvp.pic_row = nr;
	tvp.pic_col = nc;
	tvp.pic_comprs = 1;
	tvp.pic_origin = 'D';

	cbuf[0] = G_NEW_IMAGE;
	write(funix,cbuf,1);            /* Send new image command       */
	write(funix,(char *)&tvp,sizeof(tvp));

	cbuf[0] = G_SHOW_CURSOR;
	write(funix,cbuf,1);

	/*      Wait until the user hits a key on the keyboard  */
	UTUV(mhair)(cbuf,&one);
	*ichr = *cbuf;

	/*      Then read the cursor position and leave         */
	cbuf[0] = G_REPORT_CURSOR;
	write(funix,cbuf,1);    /* Ask for cursor position      */
	read(funix,(char *)&shorty,sizeof(short));
	PTUV(getinput)(funix,1);      /* Get the response             */
	sscanf(cbuf,"%d %d",iy,ix);
	*iy = -(*iy);

	cbuf[0] = G_REMOVE_CURSOR;
	write(funix,cbuf,1);

	gsizoom_(&one,&half_k,&half_k);

}

PTUV(getinput)(f,nlflag)
int f;          /* Input file descriptor                        */
int nlflag;     /* If set then read until \n, otherwise read one*/
		/* character and return.                        */
{
	char *cp;
	int n;

	cp = cbuf;
	do {
		if(cp >= &cbuf[sizeof(cbuf)-1]) {
/*                      Input line too long                     */
			return(-2);
		}
		if((n=read(f,cp,1)) != 1) {
			if(n < 0) {
/*                              Connection broken!              */
				return(-3);
			}
/*                      End of file.                            */
			if(n == 0) {
				return(-1);
			}
		}
	} while( nlflag && (*cp++ != '\n'));
	if(nlflag)
		*--cp = '\0';   /* Replace \n with null byte    */
	return(cp-cbuf);
}
