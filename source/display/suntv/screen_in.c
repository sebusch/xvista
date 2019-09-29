/*	SUN image display window interaction				*/
 
/*	This routine controls interaction between the user and the
	image displayed in the image window.  The user can use the 
	mouse to zoom and pan, mark pixels from the keyboard and
	son on. This routine also keeps a running account of the
	pixel location of the mouse cursor, and the intensity value
	of the pixel marked by the cursor.  This routine is called by
	the window manager whenever it detects and "event" taking place
	in the displayed window.  That is, this is an asynchronus routine
	that can be called at anytime.					*/
 
/*	For some types of TV interaction, VISTA must wait for a
	character to be to enterred from the screen.  In this case a
	flag is set that lets is know that VISTA is waiting.		*/
 
/*	Author:	Tod R. Lauer	4/10/86
	Princeton University Observatory				*/
 
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include "tv.h"
#define BUFLEN 32
#define ABS(x) ((x) > 0 ? (x) : -(x))
 
struct markpix pixin;
 
screen_input(display, input, arg)
 
Window display;		/* Standard window event vector.		*/
Event *input;
caddr_t arg;
 
{
	extern struct center last, lastc, lastfrez;
	extern struct box *raster, rasters[MAX_IM_HOLD];
	extern struct markpix pixin;
	extern struct pixwin *chan, *chanc;
	extern Canvas screen;
	extern Panel_item valstring;
	extern int freze, frezetog, to_vista, waiting_for_c;
	float *locpix, fcoord, valm, valc;
	char c,	out[BUFLEN];
	static int escape = 0;
	static int dysub = 0, dxsub = 0, submove = 0;
	int	x, y, z, n,
		ix, iy, xunz, yunz, comp, zl,
		ixm, iym, ixc, iyc, rs, re, cs, ce,
		val, key, set, dx, dy;
 
/*	First find the pixel coordinates of the event			*/
 
	zl	= last.zcn;
	if (zl > 0)
		comp	= 1;
	else
		comp	= 1 - zl;
		
	x	= comp * event_x(input);	/* Cursor coordinates	*/
	y	= comp * event_y(input);
	dx	= 0;
	dy	= 0;
 
/*	Check to see if a key was enterred from the keyboard.  Keys
	are defined locally within VISTA with a few exceptions.  For
	most keys, just hold the key and do nothing.  Check for R
	for restore iamge or numerics, however, which mean save pixel
	location for VISTA.						*/
 
	if (event_is_ascii(input) && event_is_down(input)) {
		c	= event_id(input);
 
/*	Check first for an escape sequence definition like a cursor key.
	The characters that make up the sequence are sent as individual
	events, so we must set a flag when ESC is detected, and then
	watch for the next two characters returned.			*/
 
/*	The cursors always move just one pixel.  For images larger than
	the display, this means keeping track of subpixel steps as far
	as the display pixel size is concerned.				*/

		key	= c;
		if (key == 27) {
			escape	= 1;
		} else if (escape == 1) {
			if (c == '[')
				escape++;
			else
				escape	= 0;
		} else if (escape == 2) {
			if (zl > 0) {
				submove	= 0;
				if (c == 'A') {
					dx	= 0;
					dy	= -zl;
				} else if (c == 'B') {
					dx	= 0;
					dy	= zl;
				} else if (c == 'C') {
					dx	= zl;
					dy	= 0;
				} else if (c == 'D') {
					dx	= -zl;
					dy	= 0;
				}
			} else {
				submove	= 1;
				dx	= 0;
				dy	= 0;
				if (c == 'A') {
					dysub--;
				} else if (c == 'B') {
					dysub++;
				} else if (c == 'C') {
					dxsub++;
				} else if (c == 'D') {
					dxsub--;
				}

				if (dxsub <= -comp) {
					dxsub	= 0;
					dx	= -comp;
				}

				if (dxsub >= comp) {
					dxsub	= 0;
					dx	= comp;
				}

				if (dysub <= -comp) {
					dysub	= 0;
					dy	= -comp;
				}

				if (dysub >= comp) {
					dysub	= 0;
					dy	= comp;
				}

			}
 
			c	= '\0';	/* NULL to avoid other functions */
			escape	= 0;
		}
 
/*	Decode ASCII and check to see if event is in bounds.  If in bounds
	then get pixel memory location and the intensity value of the
	pixel.								*/
 
		key	= c - '0';
 
		if (x + dxsub >= raster->xlotv && x + dxsub <= raster->xhitv &&
		    y + dysub >= raster->ylotv && y + dysub <= raster->yhitv) {
			zl	= last.zcn;
			if (zl > 0)
				z	= zl;
			else
				z	= -zl;
		
			ix	=(x - raster->xlotv) / z + raster->sctv + dxsub;
			if (yup ==1) 
			  iy    =(raster->yhitv - y) / z + raster->srtv + dysub;
			else 
			  iy	=(y - raster->ylotv) / z + raster->srtv + dysub;
			locpix	= raster->loctv + raster->ncim * 
				(iy - raster->roworg) + ix - raster->colorg;
			if (raster->loctv == NULL)
				valm	= 0;
			else
				valm	= *locpix;
 
/*	If a VISTA routine is waiting for a character, send it over
	by an intraprocess pipe.					*/
 
			if (waiting_for_c && c != 'R' && c != 'r') {
				*out	= c;
				pixin.prow	= iy;
				pixin.pcol	= ix;
				if (raster->loctv != NULL)
					pixin.pval	= *locpix;
				pixin.pkey	= c;
				n	= write(to_vista,out,1);
 
			}
 
/*	If the key hit is 0 to 9, save the location in VISTA variables
	and print the key hit on the value panel display.		*/
 
		    	if (key >= 0 && key <= 9) {
				if (ABS(val) >= 1000.0) {
					val	= valm + 0.5;
					sprintf (out,"%4d  %4d  %6d  %1d",
					iy, ix, val, key);
				} else if (ABS(valm) >= 100.0) {
					sprintf (out,"%4d  %4d  %6.1f  %1d",
					iy, ix, valm, key);
				} else if (ABS(valm) >= 10.0) {
					sprintf (out,"%4d  %4d  %6.2f  %1d",
					iy, ix, valm, key);
				} else if (ABS(valm) >= 1.0) {
					sprintf (out,"%4d  %4d  %6.3f  %1d",
					iy, ix, valm, key);
				} else {
					sprintf (out,"%4d  %4d  %6.3f  %1d",
					iy, ix, valm, key);
				}
				panel_set (valstring,
					PANEL_LABEL_STRING,	out,
					0);
				set	= 1;
				fcoord	= iy;
				subvar_("R", &key, &fcoord, &set, 1);
				fcoord	= ix;
				subvar_("C", &key, &fcoord, &set, 1);
			}
		}
	}
 
/*	Now take care of mouse buttons.  Check display coordinates
	and current zoom factors.  Zoom in or out, pan, or restore
	centerring of display.						*/
 
	if (event_is_button(input) && event_is_down(input)) {
		zl	= last.zcn;
		if (zl > 0) {
			comp	= 1;
			z	= zl;
		} else {
			comp	= 1 - zl;
			z	= -zl;
		}
		
		xunz	= comp * NCTV / 2 - raster->ncs / 2;
		yunz	= comp * NRTV / 2 - raster->nrs / 2;
		ix	= (x - raster->xlotv) / z + (raster->sctv - raster->scs)
			+ xunz + dxsub;
		if (yup ==1) 
		  iy	= (raster->yhitv - y) / z + (raster->srtv - raster->srs)
			+ yunz + dysub;
		else
		  iy	= (y - raster->ylotv) / z + (raster->srtv - raster->srs)
			+ yunz + dysub;
 
/*	Left mouse button: zoom in.  Middle button: zoom out.
	Right button: don't change zoom factor, just pan.		*/
 
		if (event_id(input) == MS_LEFT) {
			if (zl > 0)
				zl	*= 2;
			else 
				zl	+= 1;

			if (zl == 0) {
				ix	-= NCTV / 2;
				iy	-= NRTV / 2;
				zl	= 1;
			}

			if (zl > 64)
				zl	= 64;
		}
 
		else if (event_id(input) == MS_MIDDLE) {
			if (zl >= 1) {
				zl	/= 2;
				if (zl < 1 ) {
					ix	+= NCTV / 2;
					iy	+= NRTV / 2;
				}
			}

			if (zl < 1) {
				zl	= -1;
			}
		}
 
		last.xcn	= ix;
		last.ycn	= iy;
		last.zcn	= zl;
		pw_damaged(chan);
		pw_exposed(chan);
		tvzoom_(&zl,&ix,&iy,1);	/* This routine does the display */
 
		if (zl > 0)
			z	= zl;
		else
			z	= -zl;

		dxsub	= 0;
		dysub	= 0;
		window_set (screen,
				WIN_MOUSE_XY,	NCTV/2 + z/2,	NRTV/2 + z/2,
				0);	/* Put cursor in middle of screen */
	}
 
/*	Restore the display to the original zoom factor and centerring
	if R or r keys were hit						*/
 
	else if (c == 'R' || c == 'r') {
		zl	= last.zi;
		if (zl > 0)
			comp	= 1;
		else
			comp	= 1 - zl;
		
		ix	= comp * NCTV/2;
		iy	= comp * NRTV/2;
		last.xcn	= ix;
		last.ycn	= iy;
		last.zcn	= zl;
		dxsub	= 0;
		dysub	= 0;
		pw_damaged(chan);
		pw_exposed(chan);
		tvzoom_(&zl,&ix,&iy,1);
 
		if (zl > 0)
			z	= zl;
		else
			z	= -zl;

		window_set (screen,
				WIN_MOUSE_XY,	NCTV/2 + z/2,	NRTV/2 + z/2,
				0);
	}
 
/*	Blink by putting up the old byte buffer without erasing.  Use the
	current zoom factor and cursor position.  */
 
	else if (c == '+' || c == '-') {
		if (c == '+') {
			raster++;
			if (raster >= rasters + MAX_IM_HOLD)
				raster	= rasters;
			else if (raster->byteloc == NULL)
				raster	= rasters;
		} else if (c == '-') {
			raster--;
			if (raster < rasters)
				raster	= rasters + MAX_IM_HOLD - 1;
			while (raster->byteloc == NULL && raster > rasters)
				raster--;
		}
 
		pw_damaged(chan);
		pw_exposed(chan);
		tvzoom_(&last.zcn,&last.xcn,&last.ycn,0);
		sunpanelb(raster);
		if (raster->loctv == NULL)
			lights(-2);
		else
			lights(2);
	}
 
/*	If dy or dx is nonzero, then the window cursor is to be moved
	with the cursor keys.  The motion is one pixel per hit.		*/
 
	if (dx || dy)
		window_set (screen,
				WIN_MOUSE_XY, (x + dx) / comp, (y  + dy) / comp,
				0);
 
/*	If the mouse has been moved, then just update the display of
	the pixel coordinates and intensity value of the pixel marked
	by the cursor.  Is the mouse is still, then update the cursor
	subimage.							*/
 
	else if (event_id(input) == LOC_MOVE || event_id(input) == LOC_STILL
		|| (c == '+' || c == '-') || submove == 1 ) {
		if (x + dxsub >= raster->xlotv && x + dxsub <= raster->xhitv &&
		    y + dysub >= raster->ylotv && y + dysub <= raster->yhitv) {
			zl	= last.zcn;
			if (zl > 0)
				z	= zl;
			else
				z	= -zl;

			ix	= (x - raster->xlotv) / z + raster->sctv + dxsub;
			if (yup == 1)
			  iy	=(raster->yhitv - y) / z + raster->srtv + dysub;
			else
			  iy	=(y - raster->ylotv) / z + raster->srtv + dysub;
			locpix	= raster->loctv + raster->ncim * 
				(iy - raster->roworg) + ix - raster->colorg;
			if (raster->loctv == NULL)
				valm	= 0;
			else
				valm	= *locpix;

			if (ABS(valm) >= 1000.0) {
				val	= valm + 0.5;
				sprintf (out,"%4d  %4d  %6d",
				iy, ix, val);
			} else if (ABS(valm) >= 100.0) {
				sprintf (out,"%4d  %4d  %6.1f",
				iy, ix, valm);
			} else if (ABS(valm) >= 10.0) {
				sprintf (out,"%4d  %4d  %6.2f",
				iy, ix, valm);
			} else if (ABS(valm) >= 1.0) {
				sprintf (out,"%4d  %4d  %6.3f",
				iy, ix, valm);
			} else {
				sprintf (out,"%4d  %4d  %6.3f",
				iy, ix, valm);
			}
 
			panel_set (valstring,
				PANEL_LABEL_STRING,	out,
				0);
			if ((event_id(input) == LOC_STILL && freze == 0)
			    || (c == '-' || c == '+') || submove == 1) {
				xunz	= NCTV / 2 - raster->ncs / 2;
				yunz	= NRTV / 2 - raster->nrs / 2;
				ix	= (x - raster->xlotv) / z + dxsub +
					 (raster->sctv - raster->scs) + xunz;
				if (yup == 1)
				  iy	= (raster->yhitv - y) / z + dysub +
					 (raster->srtv - raster->srs) + yunz;
				else
				  iy	= (y - raster->ylotv) / z + dysub +
					 (raster->srtv - raster->srs) + yunz;
				tvzoomc_(&z,&ix,&iy);
				pw_ttext(chanc,32,230,PIX_COLOR(127)|PIX_SRC,
					 0,out);
			}
		}
		submove	= 0;
	}
 
/*	For the P option, locate the peak valued pixel within the cursor
	subimage, and move the cursor to that location.			*/
 
	else if ((c == 'P' || c == 'p') && raster->loctv != NULL) {
		if (x >= raster->xlotv && x <= raster->xhitv &&
		    y >= raster->ylotv && y <= raster->yhitv) {
			zl	= last.zcn;
			if (zl > 0)
				z	= zl;
			else
				z	= -zl;

			ixm	= (x - raster->xlotv) / z + raster->sctv;
			if (yup == 1)
			  iym	= (raster->yhitv - y) / z + raster->srtv;
			else
			  iym	= (y - raster->ylotv) / z + raster->srtv;
			ixc	= ixm;
			iyc	= iym;
			locpix	= raster->loctv + raster->ncim * 
				(iym - raster->roworg) + ixm - raster->colorg;
				valm	= *locpix + 0.5;
			rs	= iym - 7;
			if (rs < raster->srtv)
				rs	= raster->srtv;
			cs	= ixm - 7;
			if (cs < raster->sctv)
				cs	= raster->sctv;
			re	= iym + 7;
			if (re > raster->srtv + raster->nrs - 1)
				re	= raster->srtv + raster->nrs - 1;
			ce	= ixm + 7;
			if (ce > raster->sctv + raster->ncs - 1)
				ce	= raster->sctv + raster->ncs - 1;
			for (iy = rs; iy <= re; iy++)
				for (ix = cs; ix <= ce; ix++) {
					locpix	= raster->loctv + raster->ncim
						 * (iy - raster->roworg) +
						 ix - raster->colorg;
					valc	= *locpix + 0.5;
					if (valc > valm) {
						valm	= valc;
						ixm	= ix;
						iym	= iy;
					}
				}
			if (yup == 1) {
			  dxsub	= x + z * (ixm - ixc) -
			 	 comp * (x + z * (ixm - ixc)) / comp;
			  dysub	= y - z * (iym - iyc) -
			 	 comp * (y - z * (iym - iyc)) / comp;
			  window_set (screen,
				WIN_MOUSE_XY, (x + z * (ixm - ixc)) / comp,
					      (y - z * (iym - iyc)) / comp,
				0);
			}  else {
			  dxsub	= x + z * (ixm - ixc) -
			 	 comp * (x + z * (ixm - ixc)) / comp;
			  dysub	= y + z * (iym - iyc) -
			 	 comp * (y + z * (iym - iyc)) / comp;
			  window_set (screen,
				WIN_MOUSE_XY, (x + z * (ixm - ixc)) / comp,
					      (y + z * (iym - iyc)) / comp,
				0);
			}
		}
	}
 
 
/*	Lockup the cursor subimage, or free it for updating.		*/
 
	else if (c == 'F' || c == 'f') {
/*		freze	= 1 - freze;
		lights(3 - 6 * freze); */
		xunz	= NCTV / 2 - raster->ncs / 2;
		yunz	= NRTV / 2 - raster->nrs / 2;
		ix	= (x - raster->xlotv) / z +
			 (raster->sctv - raster->scs) + xunz;
		if (yup == 1) 
		  iy	= (raster->yhitv - y) / z +
			 (raster->srtv - raster->srs) + yunz;
		else
		  iy	= (y - raster->ylotv) / z +
			 (raster->srtv - raster->srs) + yunz;
		if (x >= raster->xlotv && x <= raster->xhitv &&
		    y >= raster->ylotv && y <= raster->yhitv && freze == 0) {
			z	= last.zcn;
			tvzoomc_(&z,&ix,&iy);
		} else if (freze) {
			lastfrez.xcn	= ix;
			lastfrez.ycn	= iy;
			lastfrez.zcn	= z;
		}
	}
 
 
/*	Togle back to the last cursor subimage				*/
 
	else if (c == 'T' || c == 't') {
		frezetog	= 1 - frezetog;
		xunz	= NCTV / 2 - raster->ncs / 2;
		yunz	= NRTV / 2 - raster->nrs / 2;
		ix	= (x - raster->xlotv) / z +
			 (raster->sctv - raster->scs) + xunz;
		if (yup == 1)
		  iy	= (raster->yhitv - y) / z +
			 (raster->srtv - raster->srs) + yunz;
		else
		  iy	= (y - raster->ylotv) / z +
			 (raster->srtv - raster->srs) + yunz;
		if (x >= raster->xlotv && x <= raster->xhitv &&
		    y >= raster->ylotv && y <= raster->yhitv && frezetog == 0) {
			z	= last.zcn;
			tvzoomc_(&z,&ix,&iy);
		} else if (frezetog) {
			ix	= lastfrez.xcn;
			iy	= lastfrez.ycn;
			z	= lastfrez.zcn;
			tvzoomc_(&z,&ix,&iy);
		}
	}
 
	c	= '\0';	/* NULL out c to avoid repeating functions	*/
	return (0);	
}
