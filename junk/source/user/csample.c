/*  Here is a sample C subroutine for VISTA, demonstrating some of */
/*  common subroutine calls, etc                                   */

#include "Vista.h"
#include <stdio.h>

#define TRUE 1
#define FALSE 0
#define EPS 1.e-6

ATUS(csample)(a,isr,ier,isc,iec)
int *isr, *isc, *ier, *iec;
float **a;
{
#ifdef VMS
#include "vinclude:vistalink.h"
#else
#include "vistadisk/source/include/vistalink.h"
#endif

  int row, col, silent, i;
  int ATUS(keyinit)(), ATUS(keycheck)(), ATUS(keydef)();
  int ATUS(assign)();
  int ncol, nrow;
  float data, tmp;
  char parm[8];

  ncol = *iec - *isc + 1;
  nrow = *ier - *isr + 1;

  ATUS(keyinit)();
  ATUS(keydef)("SILENT",6);
  ATUS(keydef)("R=",2);
  ATUS(keydef)("C=",2);

  if (ATUS(keycheck)() != TRUE) {
    con_.xerr = TRUE;
    return(-1);
  } 

  silent = FALSE;
  for (i=0;i<NCON;i++) {

    if (strncmp(vcommand_.word[i],"SILENT",6)==0) {
      silent = TRUE;
      printf("at 1\n");
    }
    
    else if (strncmp(vcommand_.word[i],"R=",2)==0) {
      ATUS(assign)(vcommand_.word[i],&tmp,parm,80,8);
      if (con_.xerr) return(-1);
      printf("at 2\n");
      row = tmp + EPS;
    }

    else if (strncmp(vcommand_.word[i],"C=",2)==0) {
      ATUS(assign)(vcommand_.word[i],&tmp,parm,80,8);
      if (con_.xerr) return(-1);
      printf("at 3\n");
      col = tmp + EPS;
    }
  }

  data = *(*a+(row-*isr)*ncol+(col-*isc));
  printf("%d %d %f\n",row, col, data);

}
