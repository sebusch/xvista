#include "Vista.h"
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/***************************************************************************
 *
 * fhooks.c - replacements for Fortrans (like Linux f2c) that are lacking
 *            hooks into various Unix library utilities
 *
 * Includes:
 *
 *  Frand:  rand() random number generator function
 *  Ftime:  time() time since ... function
 *  Fdtime  dtime() elapsed time function
 *
 * We assume that Vista will always call LickMongo, which has to define
 * some other missing modules for itself.  See the file 
 *      mongosrc/sysunix/usleep.c
 * for replacements for the ctime(), perror(), and chdir() hooks
 *
 * To enable these routines, insert the appropriate #def statement into 
 * the relevant source/Vista.machine configuration file.
 *
 * R. Pogge
 * OSU Astronomy Dept.
 * 1996 November 30
 *
 **************************************************************************/

#ifdef Frand

/* replacement for Fortrans which have no hook into Unix rand() */

float ATUS(rand)(seed)
int seed;
{
  srand48(seed);
  return (float)(drand48());
}

#endif /* Frand */

/**************************************************************************/

#ifdef Ftime

/* replacement for Fortrans which have no hook into Unix time() */

int ATUS(time)()
{
  long now;

  return (int)(time(&now));
}

#endif /* Ftime */

/**************************************************************************/

#ifdef Fdtime

/* replacement for Fortrans which have no dtime() function */

time_t USave;
time_t SSave;

float ATUS(dtime)(tarray)
float tarray[2];
{
  struct tms datime;

  times(&datime);
  tarray[0] = (float)(datime.tms_utime - USave);
  tarray[1] = (float)(datime.tms_stime - SSave);
  USave = datime.tms_utime;
  SSave = datime.tms_stime;
  return (tarray[0]+tarray[1]);
}

#endif /* Fdtime */

int ATUS(fgetenv)(var,nv)
char *var;
int nv;
{
  char tmp[300];
  strncpy(tmp,var,nv);
  tmp[nv] = 0;
  if (getenv(tmp) != NULL)
    return(1);
  else
    return(0);
}
