
   VISTA COOKBOOK MANUSCRIPT
   _________________________

   1988 September 2  [RWP]

  This directory contains the complete manuscript and structuring files for
the VISTA Cookbook. 

  The relevant files, and a brief description of their contents, follows:


  These files contain the cookbook text:

      COOK_INTRO.TEX  -  Chapter 1, Introduction
      COOK_TUTOR.TEX  -  Chapter 2, Brief Tutorial
      COOK_BASIC.TEX  -  Chapter 3, Basic Image Processing
      COOK_2D.TEX     -  Chapter 4, 2-D Image Reduction
      COOK_SPEC.TEX   -  Chapter 5, Spectral Reduction
      COOK_HAM.TEX    -  Chapter 6, Hamilton Echelle Reduction
      COOK_DIRT.TEX   -  Chapter 7, Dirty Tricks
      COOK_APPA.TEX   -  Appendix A, VISTA Command Summary
      COOK_APPB.TEX   -  Appendix B, Sample Command Procedures
      COOK_APPC.TEX   -  Appendix C, VISTA and FITS Format

      COOK_MASTER.TEX -  Master file for the cookbook.  Used to 
                         combine all of the above files into the whole

  These files are created by LaTeX during processing.  The first 2 (.TOC
  and .AUX) are stored as they contain important info for the final
  processing pass (see below).

      COOK_MASTER.TOC -  Table of Contents (TOC) file for the cookbook
                         created by LaTeX during text processing (see below)
                         Does not contain user-accessible text.

      COOK_MASTER.AUX -  Cross-reference table for the cookbook, created by
                         LaTeX during processing.  Does not contain user-
                         accessible text.

      COOK_MASTER.DVI -  Laser-Printer .DVI file, ready for printing.  Created
                         by LaTeX.


  These files are used by LaTeX to define the manual format

      MAN12.STY
      MANUAL.STY
      THESISMAC.TEX

 -----------------------------------------------------------------------------


 MAKING AN UPDATED VERSION OF THE COOKBOOK.
 _________________________________________


    First make the changes in the appropriate .TEX file.  Changes to the
    cover page or copyright page are made in COOK_MASTER.TEX.  Chapters
    may be added by inserting them into the COOK_MASTER.TEX file.

    Process the master file through LaTeX with the command:

      $ latex cook_master

    this is like TeX, but a lot fancier.  

    If there were no errors, the document will require a second pass to get 
    all of the table of contents entries and section numbers right.  Issue 
    the following commands to send LaTeX on a second pass:

      $ purge cook_master.*
      $ delete cook_master.lis
      $ delete cook_master.dvi
      $ latex cook_master

   If all of the cross-references are correct, you will have a new
   .DVI file in the account.  Purge the extra versions, and delete
   the COOK_MASTER.LIS file.  Imprint the cookbook using the command:

      $ imprint cook_master.dvi


For details on what is in the .TEX files, please consult the LaTeX Manual, a
book by Leslie Lamport.  Unlike its cousin, the ``TeXBook'', it is very well
written and very useful. 

There is also a set of examples of using LaTeX for typesetting a Ph.D. thesis
which might be useful.  These are (for the moment) in the directory
[pogge.latex].  These will be moved to a public place in the near future.  De
Clarke will know where they hide.  The file called "AAAREADME.TXT" in this
directory tells all (the Don Regan school of documentation).

  Good Luck.
      
 -----------------------------------------------------------------------------


 VISTA COMMAND SUMMARY
 _____________________


    Appendix A of the cookbook is a summary of all of the VISTA commands.
Since this is potentially useful to lots of people, there is a "stand-alone"
version of this in the file COMMSUMMARY.TEX.  Remember to update this
file as well when adding or deleting commands to Appendix A of the cookbook.

    To make a new version of this stand-alone command summary, issue
the following commands:

      $ latex commsummary
      $ purge commsummary.dvi
      $ delete commsummary.lis;*
      $ delete commsummary.aux;*
    
    Print the command summary by typing:

      $ imprint commsummary.dvi


