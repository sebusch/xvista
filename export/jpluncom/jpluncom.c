#include "Vista.h"
#include <stdio.h>
#define MAXLINE 80

main(argc,argv)
int argc;
char *argv[];
{
  char junk[80];
  char file[80], indir[80], outdir[80];
  char header[57600];
  int i, l, ifile, ierr, ncol, nrow, bitpix;
  int zero = 0;
  int one = 1;
  int istat,nhead, nbyte, npix;
  char *a, *malloc();

/* arguments are root name, input directory, output director */
  strcpy(file,argv[1]); 
  strcpy(indir,argv[2]); 
  strcpy(outdir,argv[3]); 

/* strip off extension from root name if given */
  l = index(file,".");
  if (l>0) file[l] = '\0';

/* make the .hdr file name and read header */
  sprintf(junk,"%s/%s%s",indir,file,".hdr\0");
  rdhead(header,junk,&ifile,&nhead);  

  if (ierr!=0) {
     printf("Error opening file: %s", strcat(junk,".hdr"));  
  }
  ncol = ATUS(inhead)("NAXIS1",header,6,57600);
  nrow = ATUS(inhead)("NAXIS2",header,6,57600); 
  bitpix = ATUS(inhead)("BITPIX",header,6,57600); 
  nbyte = abs(bitpix)/8*ncol*nrow;
  npix = nrow * ncol;
  a = malloc(nbyte);

/* make the .img file name and read data */
  sprintf(junk,"%s/%s%s",indir,file,".img");
  ifile = open(junk,0);

  ATUS(chead)("COMPRSN",header,junk,7,57600,80);
  if (junk[0] == 'T' || junk[0] == 't' || junk[0] == 'Y' || junk[0] == 'y') {

    ATUS(chead)("HIBYTEHI",header,junk,8,57600,80);
    if (junk[0] == 'T' || junk[0] == 't' || junk[0] == 'Y' || junk[0] == 'y') {
      ATUS(sethibytehi)(&one);
    } else{
      ATUS(sethibytehi)(&zero);
    }
    if (bitpix ==16) {
      ATUS(expand_file_short)(ifile,npix,a);
#ifdef MSBFirst
      ATUS(packfit)(a,a,&nbyte);
#endif
    } else if (bitpix==32) {
      ATUS(expand_file_int_unshort_base)(ifile,nrow*ncol,a);
#ifdef MSBFirst
      ATUS(packfit4)(a,a,&nbyte);
#endif
    }
    ATUS(unfit)("COMPRSN",header,7,57600);
    ATUS(unfit)("HIBYTEHI",header,8,57600);

    sprintf(junk,"%s/%s%s",outdir,file,".hdr");
    istat = ATUS(openc)(&ifile,junk,&one);
    sprintf(junk,"\n");
    for (i=0;i<nhead;i=i+80) {
      write(ifile,&header[i],80); 
      write(ifile,junk,1); 
    }
    close(ifile);

    sprintf(junk,"%s/%s%s",outdir,file,".img");
    istat = ATUS(openc)(&ifile,junk,&one);
    write(ifile,a,nbyte); 
    close(ifile);
  } else {

    read(ifile,a,nbyte);

    sprintf(junk,"%s/%s%s",outdir,file,".hdr");
    istat = ATUS(openc)(&ifile,junk,&one);
    sprintf(junk,"\n");
    for (i=0;i<nhead;i=i+80) {
      write(ifile,&header[i],80); 
      write(ifile,junk,1); 
    }
    close(ifile);

    sprintf(junk,"%s/%s%s",outdir,file,".img");
    istat = ATUS(openc)(&ifile,junk,&one,80);
    write(ifile,a,nbyte); 
    close(ifile);
  }
} 

rdhead(header,file,ifile,ierr)
char *header;
char *file;
int *ifile, *ierr;
{
  FILE *fp;
  int ls, le, lh, ind, i;
  char card[82];

  fp = fopen(file,"r");

  ls = 1;
  le = ls + 79;
  lh = 57600;

  while( ls <= lh && strncmp(card,"END ",4)!=0) {
    fgets(card, 82, fp);
    for (i=0;i<80;i++) *header++ = card[i];
    ls+=80;
    le+=80;
  }
  *ierr=le;
  fclose(fp);
} 

index(s,t)
char s[], t[];
{
  int i,j,k;

  for (i=0;s[i]!='\0';i++) {
     for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++,k++)
       ;
     if (t[k] =='\0')
        return(i);
  }
  return(-1);
}

