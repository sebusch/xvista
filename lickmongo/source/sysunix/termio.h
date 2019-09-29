/*
 * this is a poor effort to try to consolidate all the madness of
 * different termial drivers which have been foisted on the world
 */

#ifdef _POSIX_SOURCE
     /* Most of the up-to-date Unixes are striving to be Posix-conformant */
#    include <termios.h>
#    define Modern
#else  /* _POSIX_SOURCE */
#   ifdef TERMIO
	/* Many who are not Posix-compliant are still SYSV */
#       define Modern
#       include <termio.h>
#       define termios termio
#               define TCSAFLUSH TCSETAF
#               define TCSADRAIN TCSETAW
#       define tcgetattr(f,a) ioctl(f,TCGETA,a)
#       define tcflush(f,q) ioctl(f,TCFLSH,q)
#       define tcsetattr(f,q,a) ioctl(f,q,a)
#   else  /* TERMIO */
	/* There do still exist some bsd-style systems */
#       include <sgtty.h>
#       define termios sgttyb
#               define TCSAFLUSH TIOCSETP
#               define TCSADRAIN TIOCSETN
#       define tcgetattr(f,a) ioctl(f,TIOCGETP,a)
#       define tcflush(f,q) ioctl(f,TCIOFLUSH,q)
#       define tcsetattr(f,q,a) ioctl(f,q,a)
#   endif  /* TERMIO */
#endif /* _POSIX_SOURCE */

/*
 * the very sorry state of this I/O is indicated by the fact that some
 * systems actually use these but their includes do not define them.
 */
#       define TCIFLUSH 0
#       define TCOFLUSH 1
#       define TCIOFLUSH 2
