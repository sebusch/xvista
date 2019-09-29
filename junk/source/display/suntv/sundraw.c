/*	Author:	Tod R. Lauer	4/10/86
	Princeton University Observatory				*/
 
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include "tv.h"
#define  MAXVEC	30000
#define  MAXTEXT  200
#define  MAXCHAR 2000
 
struct tvvec {
	short xv;
	short yv;
	short end;
};
 
struct tvvec veclist[MAXVEC];
struct tvvec textlist[MAXTEXT];
char textbuf[MAXCHAR], *textp;
 
int	xv_set, yv_set, tv_cnt, tx_cnt;
 
sunmove_(ix,iy)
 
int	*ix, *iy;
 
{
	extern struct center last;
	extern struct box *raster;
	extern int xv_set, yv_set;
	extern struct tvvec veclist[MAXVEC];
	int	x, y, z, zl, comp, zad,
		xunz, yunz;
 
	zl	= last.zcn;
	if (zl > 0) {
		z	= zl;
		comp	= 1;
	} else {
		z	= -zl;
		comp	= 1 - zl;
	}

	zad	= (z - 1) / 2;
	xunz	= NCTV / 2 - raster->ncs / 2;
	yunz	= NRTV / 2 - raster->nrs / 2;
	x	= (z * (*ix - xunz + raster->scs - raster->sctv)
		+ raster->xlotv + zad) / comp;
	y	= (z * (*iy - yunz + raster->srs - raster->srtv)
		+ raster->ylotv + zad) / comp;
	xv_set	= x;
	yv_set	= y;
	if (tv_cnt > MAXVEC)
		return (-1);

	veclist[tv_cnt].xv	= *ix;
	veclist[tv_cnt].yv	= *iy;
	veclist[tv_cnt].end	= 0;
	tv_cnt++;
	return (0);	
}
 
todsundraw_(ix,iy)
 
int	*ix, *iy;
 
{
	extern struct center last;
	extern struct box *raster;
	extern struct pixwin *chan;
	extern int xv_set, yv_set;
	extern struct tvvec veclist[MAXVEC];
	int	x, y, z, zl, comp, zad,
		xunz, yunz;
 
	zl	= last.zcn;
	if (zl > 0) {
		z	= zl;
		comp	= 1;
	} else {
		z	= -zl;
		comp	= 1 - zl;
	}

	zad	= (z - 1) / 2;
	xunz	= NCTV / 2 - raster->ncs / 2;
	yunz	= NRTV / 2 - raster->nrs / 2;
	x	= (z * (*ix - xunz + raster->scs - raster->sctv)
		+ raster->xlotv + zad) / comp;
	y	= (z * (*iy - yunz + raster->srs - raster->srtv)
		+ raster->ylotv + zad) / comp;
	pw_vector(chan,xv_set,yv_set,x,y,PIX_SRC,127);
	xv_set	= x;
	yv_set	= y;
	if (tv_cnt > MAXVEC)
		return (-1);

	veclist[tv_cnt].xv	= *ix;
	veclist[tv_cnt].yv	= *iy;
	veclist[tv_cnt].end	= 1;
	tv_cnt++;
	return (0);	
}
 
suntext_(ix,iy,text)
 
int	*ix, *iy;
char	*text;
 
{
	extern struct center last;
	extern struct pixwin *chan;
	extern struct box *raster;
	extern struct tvvec textlist[MAXTEXT];
	extern char textbuf[MAXCHAR], *textp;
	extern int tx_cnt;
	int	x, y, z, zl, comp, zad,
		xunz, yunz, i;
	char	*s;
 
	if (tx_cnt == 0)
		textp	= textbuf;
 
	s	= text;
	for (i = 0; *s != NULL; i++)
		*textp++	= *s++;

	*textp++	= NULL;
 
	zl	= last.zcn;
	if (zl > 0) {
		z	= zl;
		comp	= 1;
	} else {
		z	= -zl;
		comp	= 1 - zl;
	}

	zad	= (z - 1) / 2;
	xunz	= NCTV / 2 - raster->ncs / 2;
	yunz	= NRTV / 2 - raster->nrs / 2;
	x	= (z * (*ix - xunz + raster->scs - raster->sctv)
		+ raster->xlotv + zad) / comp;
	y	= (z * (*iy - yunz + raster->srs - raster->srtv)
		+ raster->ylotv + zad) / comp;
	pw_ttext(chan, x - 4 * i, y + 5, PIX_COLOR(127)|PIX_SRC, NULL, text);
	if (tx_cnt > MAXTEXT)
		return (-1);

	textlist[tx_cnt].xv	= *ix;
	textlist[tx_cnt].yv	= *iy;
	textlist[tx_cnt].end	= i;
	tx_cnt++;
 
	return (0);	
}
 
sunvecplay()
 
{
	extern struct center last;
	extern struct box *raster;
	extern struct pixwin *chan;
	extern struct tvvec veclist[MAXVEC];
	extern int tv_cnt;
	extern char textbuf[MAXCHAR], *textp;
	extern struct tvvec textlist[MAXTEXT];
	extern int tx_cnt;
	int xv_set, yv_set;
	struct tvvec *vecp;
	int	x, y, z, zl, comp, zadx, zady,
		xunz, yunz, i, ix, iy;
 
	zl	= last.zcn;
	if (zl > 0) {
		z	= zl;
		comp	= 1;
	} else {
		z	= -zl;
		comp	= 1 - zl;
	}

	zadx	= (z - 1) / 2 + raster->xlotv;
	zady	= (z - 1) / 2 + raster->ylotv;
	xunz	=NCTV / 2 - raster->ncs / 2 - raster->scs + raster->sctv;
	yunz	=NRTV / 2 - raster->nrs / 2 - raster->srs + raster->srtv;
	vecp	= veclist;
	for (i = 0; i < tv_cnt; i++) {
		ix	= vecp->xv;
		iy	= vecp->yv;
		x	= (z * (ix - xunz) + zadx) / comp;
		y	= (z * (iy - yunz) + zady) / comp;
		if (vecp->end)
			pw_vector(chan,xv_set,yv_set,x,y,PIX_SRC,127);
		xv_set	= x;
		yv_set	= y;
		vecp++;
	}
 
	vecp	= textlist;
	textp	= textbuf;
	for (i = 0; i < tx_cnt; i++) {
		ix	= vecp->xv;
		iy	= vecp->yv;
		x	= (z * (ix - xunz) + zadx ) / comp;
		y	= (z * (iy - yunz) + zady ) / comp;
		if (vecp->end) {
			pw_ttext(chan, x - 4 * vecp->end, y + 5,
				 PIX_COLOR(127)|PIX_SRC, NULL, textp);
			textp	+= vecp->end + 1;
		}
		vecp++;
	}
	return (0);	
}
