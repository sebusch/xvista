#include "Vista.h"
/*Subroutines to aid in interactive flow control*/
/* modified Feb 27, 1984 to use fprintf(stderr, instead of printf
	rlp
   Modified to use stdout if VAX to overcome problem with \n at end of line
	jjh, November 1985
*/
#include <stdio.h>

askyn(string_yn)
char *string_yn;
{
char ans_yn[5];
while (1) {
#ifndef __VAX
   fprintf(stderr,"%s (y,n,!):",string_yn);
#else
   printf("%s (y,n,!):",string_yn);
#endif
   scanf("%s",ans_yn);
   getchar();
   switch (ans_yn[0]) {
#ifndef __VAX
      case '!' : system("PS1='% '; export PS1; sh");
                 break;
#else
      case '!' : lib$spawn(0,0,0,0,0,0,0,0,0,0,0,0);
      		 break;
#endif
      case 'Y' : 
      case 'y' : return(1);
      case 'N' :
      case 'n' : return(0);
      }
   }
return;
}



askintc(string_int,n_int,a_1,a_2,a_3,a_4,a_5,a_6,a_7,a_8)
char *string_int;
int n_int;
int *a_1,*a_2,*a_3,*a_4,*a_5,*a_6,*a_7,*a_8;
{
int i;
int *int_arr[8];
int_arr[0]=a_1; int_arr[1]=a_2; int_arr[2]=a_3; int_arr[3]=a_4;
int_arr[4]=a_5; int_arr[5]=a_6; int_arr[6]=a_7; int_arr[7]=a_8;
#ifdef __VAX
printf("%s (%d values):",string_int,n_int);
#else
fprintf(stderr,"%s (%d values):",string_int,n_int);
#endif
for(i=0;i<n_int;i++) scanf("%d",int_arr[i]);
getchar();
return;
}




askdbl(string_dbl,n_dbl,a_1,a_2,a_3,a_4,a_5,a_6,a_7,a_8)
char *string_dbl;
int n_dbl;
double *a_1,*a_2,*a_3,*a_4,*a_5,*a_6,*a_7,*a_8;
{
int i;
double *dbl_arr[8];
dbl_arr[0]=a_1; dbl_arr[1]=a_2; dbl_arr[2]=a_3; dbl_arr[3]=a_4;
dbl_arr[4]=a_5; dbl_arr[5]=a_6; dbl_arr[6]=a_7; dbl_arr[7]=a_8;
#ifdef __VAX
printf("%s (%d values):",string_dbl,n_dbl);
#else
fprintf(stderr,"%s (%d values):",string_dbl,n_dbl);
#endif
for(i=0;i<n_dbl;i++) scanf("%lf",dbl_arr[i]);
getchar();
return;
}



askline(string_line,get_line)
char *string_line, *get_line;
{
int i;
char c;
#ifdef __VAX
printf("%s\n",string_line);
#else
fprintf(stderr,"%s\n",string_line);
#endif
i=0;
do {
   c=get_line[i]=getchar();
   i++;
   } while(c!='\n' && c!='\0' && c!='\r');
get_line[i-1]='\0';
return(i);
}


askstr(string_str, get_str)
char *string_str, *get_str;
{
#ifdef __VAX
printf("%s ",string_str);
#else
fprintf(stderr,"%s ",string_str);
#endif
scanf("%s",get_str);
getchar();
return;
}
