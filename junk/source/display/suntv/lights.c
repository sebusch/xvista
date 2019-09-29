#include	<suntool/sunview.h>
#include	<suntool/canvas.h>

lights(state)

int state;

{
	extern Pixwin *litch1,
		      *litch2,
		      *litch3,
		      *litch4;
	char r[4], g[4], b[4];
	int i;

	for (i=0; i < 4; i++) {
		r[i]	= 0;
		g[i]	= 0;
		b[i]	= 0;
	}

	r[1]	= 255;
	r[3]	= 255;
	g[2]	= 255;
	g[3]	= 255;
	b[3]	= 255;

	pw_putcolormap(litch1, 0, 4, r, g, b);
	pw_putcolormap(litch2, 0, 4, r, g, b);
	pw_putcolormap(litch3, 0, 4, r, g, b);
	pw_putcolormap(litch4, 0, 4, r, g, b);

	if (state == -1) {
		pw_writebackground(litch1, 0, 0, 65, 17, PIX_COLOR(1)|PIX_SRC);
		pw_ttext(litch1, 13, 14, PIX_COLOR(3)|PIX_SRC, NULL,
			"ASYNC");
	}
	else if (state == 1) {
		pw_writebackground(litch1, 0, 0, 65, 17, PIX_COLOR(2)|PIX_SRC);
		pw_ttext(litch1, 13, 14, PIX_COLOR(0)|PIX_SRC, NULL,
			"INPUT");
	}
	else if (state == -2) {
		pw_writebackground(litch2, 0, 0, 65, 18, PIX_COLOR(1)|PIX_SRC);
		pw_ttext(litch2, 5, 14, PIX_COLOR(3)|PIX_SRC, NULL,
			"NO DATA");
	}
	else if (state == 2) {
		pw_writebackground(litch2, 0, 0, 65, 18, PIX_COLOR(2)|PIX_SRC);
		pw_ttext(litch2, 13, 14, PIX_COLOR(0)|PIX_SRC, NULL,
			"READY");
	}
	else if (state == -3) {
		pw_writebackground(litch3, 0, 0, 65, 18, PIX_COLOR(1)|PIX_SRC);
		pw_ttext(litch3, 13, 14, PIX_COLOR(3)|PIX_SRC, NULL,
			"FREEZE");
	}
	else if (state == 3) {
		pw_writebackground(litch3, 0, 0, 65, 18, PIX_COLOR(2)|PIX_SRC);
		pw_ttext(litch3, 9, 14, PIX_COLOR(0)|PIX_SRC, NULL,
			"UPDATE");
	}
	else if (state == -4) {
		pw_writebackground(litch4, 0, 0, 65, 18, PIX_COLOR(1)|PIX_SRC);
		pw_ttext(litch4, 17, 14, PIX_COLOR(3)|PIX_SRC, NULL,
			"ZOOM");
	}
	else if (state == 4) {
		pw_writebackground(litch4, 0, 0, 65, 18, PIX_COLOR(2)|PIX_SRC);
		pw_ttext(litch4, 17, 14, PIX_COLOR(0)|PIX_SRC, NULL,
			"NORM");
	}
}
