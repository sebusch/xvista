#include "Vista.h"
      SUBROUTINE DAOLOAD(LOCIM,ISR,IER,ISC,IEC,LOCB,ISRB,IERB,ISCB,IECB)

#ifdef __64BITADDRESS
      INTEGER*8 ILOCIM, LOCIM, ILOCB, LOCB
#else 
      INTEGER ILOCIM, LOCIM, ILOCB, LOCB
#endif

      INTEGER NCOLA, NROWA, NCOLB, NROWB
      COMMON /MEMLOC/ ILOCIM,IISR,IIER,IISC,IIEC,
     &                ILOCB,IISRB,IIERB,IISCB,IIECB,
     &                NCOLA, NROWA, NCOLB, NROWB

      COMMON /SIZE/ NCOL, NROW

      ilocim = locim
      iisr = isr
      iier = ier
      iisc = isc
      iiec = iec
      ilocb = locb
      iisrb = isrb
      iierb = ierb
      iiscb = iscb
      iiecb = iecb

      NCOLA = IEC-ISC+1
      NROWA = IER-ISR+1
      NCOLB = IECB-ISCB+1
      NROWB = IERB-ISRB+1

      NCOL = NCOLA
      NROW = NROWA

      RETURN
      END
