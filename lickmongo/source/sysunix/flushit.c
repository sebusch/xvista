/*
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 *      This C file is Copyrighted software.
 *      The file COPYRIGHT must accompany this file.  See it for details.
 *      ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
 */
#include "Config.h"

#include "termio.h"

struct termios *arg;

UTUV(flushit)(fildes)
int     fildes;
{
	/*      flush all input (& output)      */
	tcflush(fildes,TCIOFLUSH);
}
