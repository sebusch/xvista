/*	Sense the tape status and set up the relevent bits	*/
/*	in the software status word for the specified channel	*/
/*	Written by Richard Stover				*/
/*	Date:	May 25, 1982					*/

#include	<sys/types.h>
#include	"tm11.h"
#include	<sys/ioctl.h>
#include	<sys/mtio.h>

#define CHECKED (~(SOFFL+SNORM+SOPIN+SBOT+SEOT+SWRL+SHDWRE+SEOF))

TAPE SNTAPE(imt,icharac)
#ifdef FORT
TAPE *imt,*icharac;
#else
TAPE imt,*icharac;
#endif

/*	imt = tape channel					*/
/*	icharac = returned tape status word.  This is also	*/
/*		  saved in status[imt].				*/
{
	int i1,i2,i3,i4,i5;
	int ioc;
	int com,istat;
	register unsigned short smt,rstat,stat;
	extern struct mt mt_;
	struct mtget mtstat;

#ifdef FORT
	smt=(int)(*imt);
#else
	smt = imt;
#endif

/*	Check channel number	*/
	if(uchk(smt))
		return(0);
/*	See if unit is opened	*/
	if(mt_.file[smt]==-1)
		return(0);
/*	Read current hardware status word	*/
	ioc=ioctl(mt_.mtfdr[smt],MTIOCGET,(char *)&mtstat);
/*	Mask out the software bits we're going to check	*/
	stat=mt_.status[smt]&CHECKED;

#ifdef ISICODE
	rstat=mtstat.mt_erreg;
/* Check the hardware bits and set the software bits accordingly.	*/
	if((rstat&ONL)==0)
		stat+=SOFFL;
	if((rstat&RLE) || (rstat&RWS))
		stat+=SOPIN;
	if(rstat&BOT)
		stat+=SBOT;
	if(rstat&EOT)
		stat+=SEOT;
	if(rstat&WRL)
		stat+=SWRL;
/*
	if((rstat&BGL)||(rstat&NXM))
		stat+=SHDWRE;
*/
	if(rstat&ENOF)
		stat+=SEOF;
	if(((rstat&ILCM)==0)&&((rstat&TUR) !=0)&&((stat&SEOV)==0)&&
		((rstat&RLE)==0)&&((stat&SHDWRE)==0))
		stat+=SNORM;
#endif
#ifdef sun4
#define mtstatus (mtstat.mt_dsreg)
#define mterr  (mtstat.mt_erreg)
	if((mtstatus&ONL)==0)
		stat+=SOFFL;
	if((mterr&RLE) || (mterr&DLATE) || (mterr&MTIMEOUT))
		stat+=SOPIN;
	if(mtstatus&BOT)
		stat+=SBOT;
	if(mtstatus&EOT)
		stat+=SEOT;
	if(mtstatus&WRL)
		stat+=SWRL;
	if(mterr&ENOF)
		stat+=SEOF;
	if(((mtstatus&TUR) !=0)&&((stat&SEOV)==0)&&
		((mterr&RLE)==0)&&((stat&SHDWRE)==0)&&((stat&SOPIN)==0))
		stat+=SNORM;
#endif
/*	Save the new status word	*/
	mt_.status[smt]=stat;

/*	Return it to the caller		*/
	(*icharac)=(TAPE)stat;
	return(*icharac);

}

