#include "Vista.h"

/*      Routine to do tv scaling                    */

ATUS(tvscale)(a,ncola,b,ncolb,sr,er,sc,ec,icomp,iexp,zero,span,i_f,clip)

float   *a;             /* Image data */
char    *b;             /* Output byte array */

int     *ncola,         /* No. of columns in a */
        *ncolb,         /* No. of columns in b */
        *sr,*er,        /* Starting, ending rows */
        *sc,*ec,
        *icomp, *iexp,
        *i_f,
        *clip;

float   *zero, *span;

{
        register float *aim;
        register char *bim;
        register int i, row, col;

        int nrcomp, nccomp;
        int izero, ispan;
        int itemp;

        nrcomp = ( *er - *sr + 1 ) / *icomp ;
        nccomp = ( *ec - *sc + 1 ) / *icomp ;

        aim     = a;    /* Initialize pointers                          */
        bim     = b;

/*      Do everything with integer arithmetic                   */

        izero = (int) ( 1024. * *zero) ;
        ispan = (int) ( 1024. * *span) ;

/*      Loop over TV rows                                       */
        for (row = 1; row <= nrcomp; row++) {
/*        Loop over TV columns                                  */
          for (col = 1; col <= nccomp; col++) {
                itemp = (int) (*aim * 1024.) - izero;
                itemp = ( itemp > 0 ? itemp : 0 );
                if ( *clip == 0 ) {
                  itemp = (itemp % ispan) * *i_f / ispan ;
                  itemp >> 10;
                }
                else {
                  if ( itemp > ispan )
                        itemp = *i_f;
                  else {
                        itemp = itemp * *i_f / ispan ;
                        itemp >> 10;
                  }
                }
                *bim++ = (char) itemp;
		aim += *icomp;
                }
            bim     += *ncolb - nccomp;
            aim     += *icomp * (*ncola - nccomp);
          }
}

ATUS(etvscale)(a,ncola,b,ncolb,sr,er,sc,ec,icomp,iexp,zero,span,i_f,clip)
float   *a;             /* Image data */
char    *b;             /* Output byte array */

int     *ncola,         /* No. of columns in a */
        *ncolb,         /* No. of columns in b */
        *sr,*er,        /* Starting, ending rows */
        *sc,*ec,
        *icomp, *iexp,
        *i_f,
        *clip;

float   *zero, *span;

{
        register float *aim;
        register char *bim;
        register int counter, i, row, col;

        int nr, nc, nrcomp, nccomp;
        int izero, ispan;
        int itemp;

        nrcomp = ( *er - *sr + 1 ) * *iexp;
        nccomp = ( *ec - *sc + 1 ) * *iexp;

        aim     = a;    /* Initialize pointers                          */
        bim     = b;
        counter = 0;

        izero = (int) ( 1024. * *zero ) ;
        ispan = (int) ( 1024. * *span ) ;

/*      Loop over TV rows                                       */
        for (row = 1; row <= nrcomp; row++) {

          if ( (row % *iexp) == 1 ) {

/*        Loop over TV columns                                  */
          for (col = 1; col <= nccomp; col++) {

              if ( (col % *iexp) == 1 ) {
                itemp = (int) (*aim * 1024.) - izero;
                itemp = ( itemp > 0 ? itemp : 0 );
                if ( *clip == 0 ) {
                  itemp = (itemp % ispan) * *i_f / ispan ;
                  itemp >> 10 ;
                }
                else {
                  if ( itemp > ispan )
                        itemp = *i_f;
                  else {
                        itemp = itemp * *i_f / ispan ;
                        itemp >> 10 ;
                  }
                }
                *bim = (char) itemp;
                counter++;
                aim++;
                bim++;
              }
              else {
                *bim = *(bim - 1);
                bim++;
              }
            }
            bim     += *ncolb - nccomp;
            aim     += *ncola - counter;
            counter = 0;
          }
          else {
            for (i=1; i<=nccomp; i++) {
              *bim = *(bim - *ncolb);
              bim++;
            }
            bim     += *ncolb - nccomp;
          }

        }

}

#ifdef __PER
ATUS(itvzoom)(new,old,nrold,ncnew,ifact,iexp,nc,nr,rastc0,rastr0,dispc0,dispr0,flip)
int     *nrold, *ncnew, *ifact, *iexp, *nc, *nr, *rastc0, *dispc0;
int     *rastr0, *dispr0, *flip;
unsigned char    *new, *old;

{
        register int i,ii,j, counter,iskip;
        register unsigned char *aim, *bim;
        unsigned char **pic;

        aim = old;
        bim = new;
        counter = 0;
        pic = malloc(512);

/*  We only need to look at the number of rows in the original image that  */
/*    apply. No need to repeat data when loading into output               */
        iskip = *ifact * *iexp;
        for (j=0; j<*nrold; j++) {
          for (ii=0; ii<iskip; ii++) 
                pic[j*iskip+ii] = bim;
          for (i=1; i<=*ncnew; i++) {
                *bim++ = *aim;
                if ( (i % *ifact) == 0 ) {
                  aim++;
                  counter++;
                }
          }
          aim += 512*(*iexp) - counter;
          counter = 0;
        }

/*  Now send this off to the TV                                           */
        pzdisp(pic,*nc,*nr,*rastc0,*rastr0,*dispc0,*dispr0,*flip);

        free(pic);
}

#endif  /* ifdef PER  */
