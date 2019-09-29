C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
#include "Config.h"
C       This is the default device chosen by Lick Mongo when it is started.
c
	integer         DEF_DEV
	parameter       (DEF_DEV = _DR640)
c
C       Here we define parameters for the meaning of each kind of
C       plotting device that Lick Mongo knows about
c
#ifdef  _DR640
C               DEI Retrographics 640
	integer         DR640
	parameter       (DR640 = _DR640)
#endif  /* _DR640       */
#ifdef  _VT125
C               DEC VT125
	integer         VT125
	parameter       (VT125 = _VT125)
#endif  /* _VT125       */
#ifdef  _Tk4010
C               Tektronix 4010
	integer         Tk4010
	parameter       (Tk4010 = _Tk4010)
#endif  /* _Tk4010      */
C       Device 4 is for the local color image display device
C       (whatever that may be)
#ifdef  _Gr270
C               Grinnell 270
	integer         Gr270
	parameter       (Gr270 = _Gr270)
C       parameter       (AED512 = 0)
C       parameter       (GSI = 0)
#endif  /* _Gr270       */
#ifdef  _AED512
C               Advanced Electronic Devices AED 512
	integer         AED512
	parameter       (AED512 = _AED512)
C       parameter       (Gr270 = 0)
C       parameter       (GSI = 0)
#endif  /* _AED512      */
#ifdef  _GSI
C               Graphics Strategies Inc.
	integer         GSI
	parameter       (GSI = _GSI)
C       parameter       (AED512 = 0)
C       parameter       (Gr270 = 0)
#endif  /* _GSI         */
#ifdef  _HP2648A
C               Hewlet Packard Pen Plotter HP 2648A
	integer         HP2648A
	parameter       (HP2648A = _HP2648A)
#endif  /* _HP2648A     */
#ifdef  _ImTek
C               Imagen Laserprinter in Tek 4010 emulation mode
	integer         ImTek
	parameter       (ImTek = _ImTek)
#endif  /* _ImTek       */
#ifdef  _ISIOVW
C               ISI Optimum V Workstation
	integer         ISIOVW
	parameter       (ISIOVW = _ISIOVW)
C       parameter       (uVAXUIS = 0)
#ifdef  _ISIEpson
	integer         ISIEpson
	parameter       (ISIEpson = _ISIEpson)
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
C               MicroVAX/VMS Workstation using UIS software, VMS V4.5 to V4.7
	integer         uVAXUIS
	parameter       (uVAXUIS = _uVAXUIS)
C       parameter       (ISIOVW = 0)
#endif  /* _uVAXUIS     */
#ifdef  _Vis603
C               Visual 603 (don't buy one of these, you'll regret it)
	integer         Vis603
	parameter       (Vis603 = _Vis603)
#endif  /* _Vis603      */
#ifdef  _xterm
C               MIT X11 xterm Tektronix emulation
	integer         xterm
	parameter       (xterm = _xterm)
#endif  /* _xterm       */
#ifdef  _Selenar
C               Selenar (what is that?)  (something at Berkeley)
	integer         Selenar
	parameter       (Selenar = _Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
C               GraphOn 140
	integer         GO140
	parameter       (GO140 = _GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
C               MicroVAX/VMS Workstation in Tek emulation window
	integer         uVAXTek
	parameter       (uVAXTek = _uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
C               GraphOn 2xx series
	integer         GO2xx
	parameter       (GO2xx = _GO2xx)
#endif  /* _GO2xx       */
#ifdef  _SunView
C               SunView
	integer         SunView
	parameter       (SunView = _SunView)
#endif  /* _SunView     */
#ifdef  _X11
C               X11
	integer         X11
	parameter       (X11 = _X11)
#endif  /* _X11         */
#ifdef  _test
C               testing a new terminal
	integer         test
	parameter       (test  = 99)
#endif  /* _test        */
#ifdef  _PostScript
C               A PostScript device, output only
	integer         PostScript
	parameter       (PostScript = _PostScript)
#endif  /* _PostScript  */
#ifdef  _NDPGREX
C               An IBM PC with NDP Fortran and GREX Graphics library
	integer         NDPGREX
	parameter       (NDPGREX = _NDPGREX)
#endif  /* _NDPGREX     */

C       These are the three kinds of hardcopy devices Lick Mongo knows.
C       They are driven by writing a "vectorfile" which is then rasterized
C       for the Versatec and Printronix, or translated to Impress for Imagen.
C               Versatec V80
C               Printronix
C               Imagen  (in a really kludged fashion)

C       This is the maximum number of different plotting devices that
C       Lick Mongo can draw on at one time.
	integer         MAXSIMDEV
	parameter       (MAXSIMDEV = 1)
c                       there is only one output device at a time for now
	integer         ONLY1
	parameter       (ONLY1 = 1)

#       ifdef _X11
c                       X11 Mouse button codes returned by mongohairs
	integer         X11MB1
	parameter       (X11MB1 = -1)
	integer         X11MB2
	parameter       (X11MB2 = -2)
	integer         X11MB3
	parameter       (X11MB3 = -3)
	integer         X11MB4
	parameter       (X11MB4 = -4)
	integer         X11MB5
	parameter       (X11MB5 = -5)
#       endif /* _X11 */
#       ifdef _ISIOVW
c                       ISI Mouse button codes returned by mongohairs
	integer         ISIMB1
	parameter       (ISIMB1 = 40209)
	integer         ISIMB2
	parameter       (ISIMB2 = 40210)
	integer         ISIMB3
	parameter       (ISIMB3 = 40212)
#       endif /* _ISIOVW */
