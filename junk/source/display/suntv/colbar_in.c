/*	Routine to handle SUN colorbar display input interrupts		*/

/*	This routine catches mouse motions, and mouse buttons
	enterred while the cursor is within the VISTA colorbar window on the
	SUN screen.  Inputs are handled as follows:

		Any mouse button	Initialize for rolling map
		X mouse motion with button down		Roll colormap according
							to motion in 'x'.  */

/*	Author:	Tod R. Lauer	4/10/86
	Princeton University Observatory				*/

#include <suntool/sunview.h>
#include <suntool/panel.h>

colorbar_input(display, input, arg)

Window display;
Event *input;
caddr_t arg;

{
	static int xl, mb;
	int  	x, jump, contrast;

	x	= event_x(input);	/* Cursor coordinates		*/

/*	If a mouse button has been pressed, just record its current 'x'
	position.
*/

	if ((event_is_button(input) && event_is_down(input)) ||
	     event_id(input) == LOC_WINENTER) {
		xl	= x;
		if (event_id(input) == MS_RIGHT)
			mb	= 3;
		else if (event_id(input) == MS_MIDDLE)
			mb	= 2;
		else if (event_id(input) == MS_LEFT)
			mb	= 1;
	}

/*	If the mouse moves with a button down, then the ammount to roll the
	color map is given by the distance in x it has moved since the last
	position recorded;							*/

	else if (event_id(input) == LOC_DRAG && mb != 3) {
		jump	= x - xl;
		contrast	= 0;
		xl	= x;
		if (jump > 255)
			jump	= 255;
		else if (jump < -255)
			jump	= -255;

		colorroll_(&jump,&contrast);	/* Roll the color map */
	}

/*	If the mouse moves with a button down, then the ammount to roll the
	color map is given by the distance in x it has moved since the last
	position recorded;							*/

	else if (event_id(input) == LOC_DRAG && mb == 3) {
		contrast	= x - xl;
		jump	= 0;
		xl	= x;
		if (contrast > 255)
			contrast	= 255;
		else if (contrast < -255)
			contrast	= -255;

		colorroll_(&jump,&contrast);	/* Roll the color map */
	}

	return (0);	
}
