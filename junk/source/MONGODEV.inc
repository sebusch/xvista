

C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **

C       This is the default device chosen by Lick Mongo when it is started.
c
	integer         DEF_DEV
	parameter       (DEF_DEV = 1)
c
C       Here we define parameters for the meaning of each kind of
C       plotting device that Lick Mongo knows about
c

C               DEI Retrographics 640
	integer         DR640
	parameter       (DR640 = 1)







C               Tektronix 4010
	integer         Tk4010
	parameter       (Tk4010 = 3)

C       Device 4 is for the local color image display device
C       (whatever that may be)
C               Visual 603 (don't buy one of these, you'll regret it)
	integer         Vis603
	parameter       (Vis603 = 8)
C               MIT X11 xterm Tektronix emulation
	integer         xterm
	parameter       (xterm = 10)
C               GraphOn 140
	integer         GO140
	parameter       (GO140 = 14)
C               GraphOn 2xx series
	integer         GO2xx
	parameter       (GO2xx = 13)
C               X11
	integer         X11
	parameter       (X11 = 11)
C               A PostScript device, output only
	integer         PostScript
	parameter       (PostScript = -17)

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
