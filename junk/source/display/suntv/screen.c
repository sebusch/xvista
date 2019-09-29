#include <curses.h>
#include <stdio.h>
screen_(flag)

int *flag;

{
	if (*flag >= 1) {
		if (initscr() != ERR) {
			savetty();
			raw();
			noecho();
			if (*flag >= 2) {
				mvcur(0,COLS-1,0,0);
				printf("     Row    Col    Value");
				mvcur(0,COLS-1,1,0);
			}
		}
	}
	else {
		mvcur(0,COLS-1,1,0);
		echo();
		noraw();
		resetty();
		endwin();
	}

	return (0);
}

prpix_(row,col,pix,mark)

int	*row,
	*col,
	*pix,
	*mark;

{
	if (*mark < 0) {
		printf ("    %4d   %4d   %6d",*row,*col,*pix);
		mvcur(0,COLS-1,0,0);
	}
	else {
		printf ("    %4d   %4d   %6d %2d",*row,*col,*pix,*mark);
		mvcur(0,COLS-1,1,0);
	}
	return (0);
}
