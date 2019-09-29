/*
GRYKEY() -- ROUTINE TO ALLOW CONTROL OF THE GREY SCALE FROM THE KEY PAD

JJH, Caltech 1986
Version 1.5b -- JJH, CIT  14 August 1986
	Purge typeahead buffer
	Don't initially select overlay on
	Add Default scale command
*/

#include stdio
#include "fortrancompat"
#include "qiodef"
#include iodef
#include ssdef

#define GREY	gryvlt(low,high,btow,table);	
#define BW	gryvlto(low,high,255,-1);	\
   		gryvlto(low,high,0,-2);	\
   		gryvlto(low,high,255,-4)

grykey(ilow, ihigh, elowptr, ehighptr, table)
int ilow;			/* Initial low for vlt */
int ihigh;			/* Initial high for vlt */
int *elowptr;			/* Final low for vlt */
int *ehighptr;			/* Final high for vlt */
int table;			/* What table to control */
{
char c;
int low, high, step=1, status, temp;
int btow;
static int lastbtow=128;
struct iostat ret;
struct shortint chan, ufnc;
int vlt=0;

low=ilow;
high=ihigh;
btow=lastbtow;

if (table == 0) {
  gryvlto(ilow, ihigh, 255, -1);
  gryvlto(ilow, ihigh, 0, -2);
  gryvlto(ilow, ihigh, 255, -4);
  }
else {
  gryvlt(ilow, ihigh, -1, table);
  }

vltsel(0);

printf("Beginning interactive grey scale control.\n");
printf("Hit '?' for instructions.\n");

status=sys$assign(strdes("SYS$INPUT:"),&(chan.x),0,0);
if(status!=SS$_NORMAL) return(-1);

ufnc.x=IO$_READVBLK;
ufnc.x |= IO$M_NOECHO;		/* Don't echo the input 	*/
ufnc.x |= IO$M_NOFILTR;		/* Don't filter the input 	*/
ufnc.x |= IO$M_PURGE;		/* Purge the typeahead buffer	*/

while (1) {
   status=sys$qiow(0,chan,ufnc,&ret,0,0,&c,1,0,0,0,0);
   if(status!=SS$_NORMAL) return(-1);
   
   switch (c) {
      case 'f' :				/* Speed up */
      case 'F' : step = 5*step > 25 ? 25 : 5*step;
		 printf("Step = %d\n",step);
      		 break;
      case 's' :				/* Slow down */
      case 'S' : step = step/5 < 1 ? 1 : step/5;
		 printf("Step = %d\n",step);
      		 break;
      case '1' : low -= step;			/* Black down */
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
      		 break;
      case '3' : low += step;			/* Black up */
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
      		 break;
      case '4' : high -= step;			/* White down */
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
      		 break;
      case '6' : high += step;			/* White up */
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
      		 break;
      case '8' : high += step;			/* Slide the whole scale up*/
      		 low += step;
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
      		 break;
      case '2' : high -= step;			/* Slide the whole scale down*/
      		 low -= step;
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
      		 break;
      case 'i' :				/* Invert the vlt */
      case 'I' : temp = low;
		 low = high;
		 high = temp;
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
		 break;
      case 'o' :
      case 'O' : vlt = vlt ? 0 : 1;
		 vltsel(vlt);			/* Turn overlay on/off */
		 /* printf("Overlay now %s.\n",vlt ? "off" : "on");  */
                 break;
      case 'r' :
      case 'R' : low=ilow;			/* Reset to initial scale */
      		 high=ihigh;
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
      		 break;
      case '?' :				/* Print instructions */
      case 'h' :
      case 'H' : grey_help();
      		 break;
      case 'd' :				/* Default table	*/
      case 'D' : low = 127;
		 high = 1;
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
		 break;
      case '+' : if (btow>254) break;		/* Move overlay break up */
                 btow++;
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
		 break;
      case '-' : if(btow<1) break;		/* Move break down	*/
		 btow--;
		 if (table==0) {  BW;   }
		 else {	   GREY;   }
		 break;
      case '0' :				/* Quit */
      case 'q' :
      case 'Q' : *elowptr=low;
      		 *ehighptr=high;
		 lastbtow=btow;
      		 sys$dassgn(chan);
      		 return(0);
      		 break;
      }
   }
}


grey_help()
{
printf("Interactive control of the grey scale is centered around the\n");
printf("KEYPAD.  Controls are:\n");
printf(" 4 = Move white end down      6 = Move white end up\n");
printf(" 1 = Move black end down      3 = Move black end up\n");
printf(" 2 = Slide the scale down     8 = Slide the scale up\n");
printf(" 0 (=q) = Quit\n");
printf("\nOther controls:\n");
printf(" R = Reset to initial display parameters\n");
printf(" D = Set default display parameters (full range scale)\n");
printf(" F = Speed up the step rate\n");
printf(" S = Slow down the step rate\n");
printf(" O = Turn overlay on/off\n");
printf(" I = Invert the vlt\n");
printf(" +/- = Move break in overlay up or down.\n\n");
printf(" ? (=h) = Print these instructions.\n");
return;
}
 

/*
FORTRAN INTERFACES
*/


fgrykey(black, white, endlow, endhigh, table)
int *black, *white, *endlow, *endhigh, *table;
{
grykey(*black, *white, endlow, endhigh, *table);
}

fbwkey(black, white, endlow, endhigh)
int *black, *white, *endlow, *endhigh;
{
grykey(*black, *white, endlow, endhigh, 0);
}


