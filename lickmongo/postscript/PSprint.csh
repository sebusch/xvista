#!/bin/csh
#       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
#       This shell script is Copyrighted software.
#       The file COPYRIGHT must accompany this file.  See it for details.
#       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
if ( -x /bin/machine ) then
    if ( `/bin/machine` == "alpha" ) then
	# I cannot believe it, I just cannot believe it.
	# It looks like DEC alphas running OSF have a race condition
	# where "lpr -r" may sometimes delete the file before insuring
	# successful queueing to the remote print host.
	lpr -s $1
	sleep 10; /bin/rm $1 &
    endif
else
	lpr -s -r $1
endif
set prstat=$status
if ( $prstat != 0 ) then
	echo "Could not print PostScript file $1"
	echo "on the printer"
endif
