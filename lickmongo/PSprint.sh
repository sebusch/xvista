#! /bin/sh
#       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
#       This shell script is Copyrighted software.
#       The file COPYRIGHT must accompany this file.  See it for details.
#       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
#if test -f /bin/4d && 4d ; then
#   we presume to be on the Lick SGI 4d machine
#   we are not yet running IRIX 3.3, so we do not have a BSD lpr
#    sunps $1
#    rm $1
#else
#   print a PostScript file on a LaserWriter on a Lick machine with BSD lpr.
#   The environment variables PSPRINTER and PRINTER are checked to determine
#   where to send the plot, but if they are not set the plot goes to a
#   printer named "lw".
    if [ "$PSPRINTER" = "" ]; then
	if [ "$PRINTER" = "" ]; then
	    PSPRINTER=lw
	else
	    PSPRINTER=$PRINTER
	fi
    fi
    lpr -s -P$PSPRINTER -r $1
#    lpr -s -P$PSPRINTER $1
    prstat=$?
    if [ $prstat -eq 1 ] ; then
	echo "Could not print PostScript file $1" >&2
	echo "on the printer named $PSPRINTER" >&2
    fi
#fi
