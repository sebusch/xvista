#include "Vista.h"
ATUS(markhelp)()
{
printf("General ITV commands: \n");
printf("'E' Exit\n");
printf("'D' Print position and value at cursor\n");
printf("'I' Zoom in\n");
printf("'O' Zoom out\n");
printf("'P' Pan\n");
printf("'R' Restore\n");
printf("'B' Blink (if BLINK display was used)\n");
printf("'X' X plot\n");
printf("'Y' Y plot\n");
printf("'U' Draw a box\n");
printf("'V' Draw a circle\n");
printf("'Z' Save to a file\n");
printf("MARKSTAR commands: \n");
printf("'C' Mark star with centroided position\n");
printf("'J' Mark star with position of cursor (integer)\n");
printf("'M' Remove last star marked\n");
printf("'S' Increment ID number without marking star\n");
printf("'#' Give star number of star nearest cursor\n");
printf("'!' Delete star (when save with SAVE COO=) nearest cursor\n");
return(0);
}

