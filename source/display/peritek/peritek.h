/*
Definitions for Peritek code

Jeff Hester, CIT
*/

#include "qiodef"

extern struct shortint pza_chan;

struct pz_point {
   short int x;
   short int y;
   short int z;
   short int mask;
   };

/* Define lookup tables */

#define	PZ_RED		(-1)
#define PZ_GREEN	(-2)
#define PZ_BLUE		(-4)
#define PZ_RED_2	(-8)
#define PZ_GREEN_2	(-16)
#define PZ_BLUE_2	(-32)

