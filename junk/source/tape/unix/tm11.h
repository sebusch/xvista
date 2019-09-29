/*	The tape routines can be compiled for either	*/
/*	FORTRAN programs or C programs.  If the symbol	*/
/*	FORT is defined (-DFORT in the compile line)	*/
/*	then the FORTRAN version is produced.  Other-	*/
/*	wise the C version is produced.			*/

/*	Define FORTRAN subroutine names			*/
#ifdef FORT

typedef long	TAPE;	/* FORTRAN uses long's		*/
#define ASTAPE	astape_
#define ENDFIL	endfil_
#define LTAPCH	ltapch_
#define LTAPOS	ltapos_
#define LTAPST	ltapst_
#define RTAPE	rtape_
#define RWND	rwnd_
#define SNTAPE	sntape_
#define SPACEB	spaceb_
#define SPACEF	spacef_
#define WTAPE	wtape_

#else

typedef int	TAPE;	/* C uses int's			*/
#define ASTAPE	astape
#define ENDFIL	endfil
#define LTAPCH	ltapch
#define LTAPOS	ltapos
#define LTAPST	ltapst
#define RTAPE	rtape
#define RWND	rwnd
#define SNTAPE	sntape
#define SPACEB	spaceb
#define SPACEF	spacef
#define WTAPE	wtape

#endif

/*	hardware status word bit definitions		*/

#ifdef ISICODE
/* Error codes in xstat 0 */
/*	These are the codes returned by the ISI tape drive interface	*/
/*	Not all of them are needed, but I've defined them all for	*/
/*	completeness.							*/
/*	The only place this group of 16 defines is used is in the	*/
/*	next group of defines beginning with "#define TUR ..."		*/

#define	TS_TMK	0100000		/* tape mark detected */
#define	TS_RLS	0040000		/* record length short */
#define	TS_LET	0020000		/* logical end of tape */
#define	TS_RLL	0010000		/* record length long */
#define	TS_WLE	0004000		/* write lock error */
#define	TS_NEF	0002000		/* non-executable function */
#define	TS_ILC	0001000		/* illegal command */
#define	TS_ILA	0000400		/* illegal address */
#define	TS_MOT	0000200		/* capstan is moving */
#define	TS_ONL	0000100		/* on-line */
#define	TS_IES	0000040		/* interrupt enable status */
#define	TS_VCK	0000020		/* volume check */
#define	TS_PED	0000010		/* phase-encoded drive */
#define	TS_WLK	0000004		/* write locked */
#define	TS_BOT	0000002		/* beginning of tape */
#define	TS_EOT	0000001		/* end of tape */

/*	These defines are the ones actually used by the source code.	*/
/*	Some are commented out because they apply only to the LSI-11	*/
/*	tape drive interface.						*/

#define	TUR	TS_ONL	/* Tape unit ready		*/
#define	RWS	TS_MOT	/* Rewind status (1=rewinding)	*/
#define	WRL	TS_WLK	/* Write locked			*/
/*#define SDWN	010	/* Tape slowing down		*/
#define	BOT	TS_BOT	/* Beginning of Tape		*/
#define OFFL	TS_VCK	/* Offline			*/
#define ONL	TS_ONL	/* On line			*/
/*#define	NXM	0200	/* Non-existant memory		*/
#define	RLE	TS_RLL	/* Record length error		*/
#define	EOT	TS_EOT	/* End of Tape			*/
/*#define	BGL	04000	/* Bus grant late		*/
/*#define	HE	010000	/* Hard error			*/
#define	ENOF	TS_TMK	/* End of file			*/
#define	ILCM	TS_ILC	/* Illegal command		*/
#endif

/*	The sun-4 xylogics controller status and error codes	*/
#ifdef sun4
#ifdef solaris
#include <sys/xtreg.h>
#else
#include <sundev/xtreg.h>
#endif
#define TUR	XTS_RDY	/* Tape unit ready		*/
#define RWS	XTS_REW	/* Tape is rewinding		*/
#define WRL	XTS_FPT	/* Write locked			*/
#define	BOT	XTS_BOT	/* Beginning of Tape		*/
#define ONL	XTS_ONL	/* On line			*/
#define	EOT	XTS_EOT	/* End of Tape			*/
#ifdef solaris
#define RLE     XTE_REC_LENGTH_LONG
#define ENOF    XTS_FMK
#define DLATE   XTE_DATA_LATE_DETECTED
#define MTIMEOUT XTE_OPERATION_TIMEOUT
#else
#define	RLE	XTE_LONGREC	/* Record length error	*/
#define	ENOF	XTE_EOF	/* End of file			*/
#define DLATE	XTE_DLATE /* Data late			*/
#define MTIMEOUT XTE_TIMEOUT /* Hdw or software timeout	*/
#endif
#endif

/*	software status word bit definitions		*/

#define	SOFFL	01	/* Off line			*/
#define SNORM	02	/* Normal successful completion	*/
#define SOPIN	04	/* Operation incomplete		*/
#define SBOT	010	/* Beginning of tape		*/
#define SEOT	020	/* End of tape			*/
#define SEOV	040	/* End of volume		*/
#define SDEN	0100	/* Density 0=1600 1=800		*/
#define SPRTY	0200	/* Parity 0=even 1=odd		*/
#define SWRL	0400	/* Write locked			*/
#define SHDWRE	01000	/* Hardware error		*/
#define	SEOF	02000	/* End of file			*/

/*	Commands for the Command register (MTC)		*/
/*	They are used in the ioctl() call to get various*/
/*	tape drive functions.  These are interface	*/
/*	specific.					*/
#define UOFFL	6		/* Go off line		*/
#define	WREOF	0		/* Write EOF		*/
#define SPF	3		/* Space forward record	*/
#define SPR	4		/* Space reverse reverse*/
#define SPFF	1		/* Space forward file	*/
#define SPRF	2		/* Space reverse file	*/
#define	RWD	5		/* Rewind		*/
#define NOP	7		/* No operation		*/

/*	Other command register bits			*/

#define GO	01		/* Execute command	*/
#define INTEN	0100		/* Interrupt enable	*/
#define CURDY	0200		/* Control unit ready	*/

/*	Define global variables:			*/
struct mt {
int	status[5];
			/* Software status words for each of 4	*/
			/* possible units. Note that status[0]	*/
			/* is not used.				*/
int	unit[5];
			/* Physical mag tape units assigned to	*/
			/* the 4 possible channels.  Again,	*/
			/* unit[0] is not used.			*/
int	mtfdr[5];
			/* File descriptors for the 4 tape	*/
			/* channels.  These are used to read	*/
			/* and write tape using the standard	*/
			/* read(...) and write(...) system	*/
			/* calls and to execute commands	*/
			/* through the ioctl() system call.	*/
int	file[5];
			/* The current file numbers for the 4	*/
			/* possible channels, with file[0] not	*/
			/* used.  If file[i] is -1 then the	*/
			/* channel has not been opened.		*/
			/* If file[i] is 0 then the channel is	*/
			/* open but the tape position has not	*/
			/* been established.  The first file	*/
			/* on a tape is file 1.			*/
};

