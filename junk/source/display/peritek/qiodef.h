/* Structures to aid in using VMS SYS$QIO calls */
/* Jeff Hester, CIT				*/

struct shortint {
   short int x;
   };

struct iostat {
   short int offset;
   short int stat;
   short int size;
   short int terminator;
   };

/*
EXAMPLE USING QIO

#include fortrancompat
#include qiodef

struct iostat ret;
struct shortint chan, ufnc;

main()
{
char str[10];
int length;

sprintf(str,"TEST\n");
length=strlen(str);

sys$assign(strdes("SYS$OUTPUT"),&(chan.x),0,0);

ufnc.x = IO$_WRITEPBLK;

sys$qiow(0,chan,ufnc,&ret,0,0,str,length,0,0,0,0);
}
*/
