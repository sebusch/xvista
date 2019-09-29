#include <suntool/sunview.h>
#include <suntool/tty.h>
#include <suntool/tool_struct.h>
#include <sunwindow/win_cursor.h>
#include <sgtty.h>
#include <stdio.h>
#include <ctype.h>
 
Window	gt_baseframe, gt_ttysw;

static short	iconimage[] = {
# include "Vista.icon"
};
DEFINE_ICON_FROM_IMAGE (icon, iconimage);

static	int main_argc, tty_argc;
static	char **main_argv, *tty_argv[50];
#include "vistadisk/source/display/suntv/tv.h"

/*	Tool to start up Vista in the SunView environment.		*/

main (argc, argv)
int	argc;
char	**argv;
{
	main_argc = argc;
	main_argv = argv;
	parse_args (argc, argv, &tty_argc, tty_argv);

	gt_baseframe = window_create (NULL, FRAME,
		FRAME_ICON,		&icon,
		FRAME_ARGC_PTR_ARGV,	&tty_argc, tty_argv,
		FRAME_CLOSED,		TRUE,
		FRAME_LABEL,		"VISTA",
		0);
	if (gt_baseframe == NULL)
	    _exit (1);

	create_tty_subwindow ();

	window_main_loop (gt_baseframe);
	exit (0);
}
static
parse_args (argc, argv, tty_argc, tty_argv)
int	argc;
char	*argv[];
int	*tty_argc;
char	*tty_argv[];
{
	register char	*argp;
	register int	arg = 1;

	/* Copy the tty arguments. */
	tty_argv[0] = argv[0];
	for (*tty_argc=1;  arg <= argc && (argp = argv[arg]) != NULL;  arg++)
	    if (strcmp (argv[arg], "-G") != 0) {
		tty_argv[(*tty_argc)++] = argp;
	    } else {
		arg++;
		break;
	    }


	tty_argv[(*tty_argc)] = NULL;
}

/* CREATE_TTY_SUBWINDOW -- Create a standard TTY subwindow of the base frame.
 * This code emulates the shelltool arguments.
 */
static
create_tty_subwindow ()
{
	char cmdstring[256];

	gt_ttysw = window_create (gt_baseframe, TTY, 0);

	window_set (gt_ttysw,
	    WIN_CONSUME_KBD_EVENTS, KEY_TOP(7), KEY_TOP(8), KEY_TOP(9), 0,
	    0);

	/* Pass supplied command, if given, to the shell. */
	    sprintf(cmdstring, "/vistadir/source/vista \n");
	    ttysw_input (gt_ttysw, cmdstring, strlen(cmdstring));


}
