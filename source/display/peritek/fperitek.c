/*
Easy FORTRAN interface to PERITEK routines

JJH, CIT  3 June 1986
Modifications from J. Holtman  22 Jan 1986
*/

#include "fortrancompat"

fpzopen(result)
int *result;
{
*result = pzopen();
}

fgryvlt(black, white, blacktowhite, table)
int *black, *white, *blacktowhite, *table;
{
gryvlt(*black, *white, *blacktowhite, *table);
}

fgryvlto(black, white, over, table)
int *black, *white, *over, *table;
{
gryvlto(*black, *white, *over, *table);
}

fpzdisp(array,asizex,asizey,nc,nr,rastc0,rastr0,dispc0,dispr0,flip)
unsigned char *array;
int *asizex, *asizey;
int *nc, *nr, *rastc0,*rastr0,*dispc0,*dispr0,*flip;
{
int i, j, x, y;
char **pic, *malloc();

x= *asizex;
y= *asizey;

pic=malloc(4*y);

for(j=0; j<y; j++) pic[j] = array+j*x;

pzdisp(pic,*nc,*nr,*rastc0,*rastr0,*dispc0,*dispr0,*flip);
}


fpzclea(x0,y0,nx,ny)
int *x0, *y0, *nx, *ny;
{
pzclea(*x0,*y0,*nx,*ny);
}

fpzrvlt(vlt, table)
unsigned char *vlt;
int *table;
{
pzrvlt(vlt,*table);
}


fpzpoints(x, y, z, mask, n)
short int *x, *y, *z, *mask;
int *n;
{
int i, nn, ii;
/*  unsigned char *points;  */
short int *points;
char *malloc();

nn= *n;

points = malloc(8*nn);

for(i=0,ii=0; i<nn; i++, ii+=4) {
   points[ii]=x[i];
   points[ii+1]=y[i];
   points[ii+2]=z[i];
   points[ii+3]=mask[i];
   }

pzpoints(points,nn);

free(points);

return;
}


fclrovr(x0, y0, nx, ny)
int *x0, *y0, *nx, *ny;
{
clrovr(*x0, *y0, *nx, *ny);
}


fvltsel(mapnum)
int *mapnum;
{
vltsel(*mapnum);
}


fpztext(string, x0, y0, mode, fill, rot, scale)
struct dsc$descriptor_s *string, *mode;
int *x0, *y0, *fill, *rot, *scale;
{
int n;
char outstr[512];

n = string->dsc$w_length;
n = 511>n ? 511 : n;
strncpy(outstr,string->dsc$a_pointer,n);
outstr[n]=0;

pztext(outstr,*x0,*y0,strptr(mode),*fill,*rot,*scale);
}

