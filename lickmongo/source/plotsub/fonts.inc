C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C------------------------------------------------------------------------------
c       NOTE:  all variables in the following section need to be SAVEd
c       Vector stroked fonts are stored in these variables.
c       The strokes for each character are stored in a coordinate
c       system where the Centerline and Halfline of the character
c       intersect at the origin.  This is consistent with the fact
c       that most of these characters are from the Hershey fonts.
c                       how many different fonts were in the font file?
	integer         numfont
c                       how many vertices overall for all these fonts?
	integer         nvertex
c               for each character in each font...
c                       ...how many strokes?
	character       nstroke(0:ASCIICHAR-1,0:MAXFONT-1)
c                       ...where is the left sidebearing?
	character       ladj(0:ASCIICHAR-1,0:MAXFONT-1)
c                       ...where is the right sidebearing?
	character       radj(0:ASCIICHAR-1,0:MAXFONT-1)
c                       ...where in the stroke array does this char start?
	integer         pointer(0:ASCIICHAR-1,0:MAXFONT-1)
c               The array font contains all the character stroke endpoints.
	character       font(MAXVRTX)
c               This is the vertical font metric information in GKS terms.
c               These are not all well-defined, and in fact can be nearly
c               meaningless for cartographic symbols.
c               In general, the following rules have been used to determine
c               the vertical metric information for Lick Mongo:
c                   Topline
c                       The Topline runs along the highest points of characters
c                       such as "(){}" etc.  In the Hershey fonts these
c                       characters extend above the Capline.
c                   Capline
c                       The Capline runs along the top of capital letters.
c                   Halfline
c                       The Halfline is defined by Dr. Hershey's data.
c                       The Halfline runs along the tops of lower case letters
c                       such as "acemnorsuvwxz".  In character sets without
c                       lower case letters, the Halfline is about halfway
c                       between the Baseline and the Capline.
c                   Baseline
c                       The Baseline runs along the bottom of letters which
c                       do not have descenders.
c                   Bottomline
c                       The Bottomline runs along the lowest points of the
c                       descenders of letters such as "gjpqy".
c               The result of these choices is that the Topline of of one line
c               of text may be coincident with the Bottomline of the line of
c               text immediately above with only rare collisions of the lines.
c               However, a true typesetter would probably advise using an
c               inter-Baseline spacing greater than (Topline-Bottomline).
	integer         vline(BOT:TOP,0:MAXFONT-1)
c       NOTE:  all variables in the preceding section need to be SAVEd
C------------------------------------------------------------------------------
