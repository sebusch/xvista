#include "Config.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
int fromcom;

char *readpipe();

void ATUS(setpipe)(char *file,int *n)
{
  file[*n] = 0;
  fromcom = open(file,O_RDWR); 
  if (fromcom > 0) 
    UTUV(mx11register)(&fromcom,readpipe); 
  else
    fprintf(stderr,"Error opening: %s\n", file);
}

char *readpipe()
{
#define MAXCOMMAND 300 
   int n, nread, status;
   char command[MAXCOMMAND];
   char *s;

   nread = 0;
   memset(command,0,MAXCOMMAND);
   /* If we've read anything, keep reading until we encounter a newline */
   while ( strstr(command,"\n") == NULL ) {
        if ( (n = read(fromcom,command+nread,MAXCOMMAND-nread)) > 0)  {
          nread += n;
        }
   }
   command[nread-1] = '\0';
   s= command;
   return(s);

 
}
