/*
DOMAIN.H -- DEFINITIONS AND DECLARATIONS FOR WFPC DISK FILE FORMAT 
INTERFACE ROUTINES

NOTE:  This file automatically includes rms.h and stdio.h

Jeff Hester, CalTech
Version date = 30 April 1986 

1.0a  17 July 1986
	Replaced DOM$_MODCARD with DOM$_NOCARD
*/

/* Version number for include file */

#define	DOM$_HVERSION	"1.0a"

/* Values for data type */

#define DBYTE	(1)		/* 8 bit integers		*/
#define D16	(2)		/* 16 bit integers		*/
#define D32	(4)		/* 32 bit integers		*/
#define DREAL	(14)		/* 32 bit real			*/
#define DDOUBLE	(18)		/* 64 bit real			*/

/* FITS KEYWORD TYPES */

#define LOGICAL	(0)		/* Value in field is T or F	*/
#define CHAR	(1)		/* Value is 'char string'	*/
#define INT2	(2)		/* Value is int, read as I*2	*/
#define INT4	(4)		/* Value is int, read as I*4	*/
#define REAL4	(14)		/* Value is real, read as R*4	*/
#define REAL8	(18)		/* Value is real, read as R*8	*/

#define FITS_FIRST_OPT	(480)	/* Header offset to first optional keyword */

/*
STRUCTURES TO HOLD INFORMATION ABOUT OPEN IMAGES.  THE UNIT NUMBER
OF THE IMAGE IS AN INDEX INTO THIS ARRAY.
*/

#include rms

struct dom_info {

   /* Essential parameters for understanding file */

   int cols, rows;	/* Obvious				*/
   int elem_len;	/* Element length, in bytes		*/
   int type;		/* Data type.  See defines above	*/
   int npics;		/* Number of pictures in the file	*/
   int nrecs;		/* Number of records per picture	*/

   /* Book keeping parameters for routines */

   char imgname[256];	/* Name of the image file		*/
   char hdrname[256];	/* Name of the header file		*/
   int filenum;		/* "Number" of the image file		*/
   int picnum;		/* Which of npics images is referenced	*/
   int d_fd;		/* Data file descriptor, if open	*/
   int h_fd;		/* Header file descriptor		*/
   int h_end_pos;	/* Byte offset to the END card		*/
   struct FAB *fab;	/* VMS File access block, if mapped	*/
   int addr[2];		/* Addr, Low and high pages, if mapped	*/
   int mapped;		/* 1 if mapped, 0 otherwise		*/
   int locked;		/* 1 if locked into core, 0 otherwise	*/
   char *arr_ptr;	/* If mapped, the array of row pointers	*/

   /* 
   NONESSENTIAL PARAMETERS 

   These parameters are read from the file header if they are present.
   If a header parameter was not found, the parameter is set to 0.
   */

   char *object;	/* Object identification		*/

   /* 
   Space for more header parameters.
   Initially there are 50 entries in dummy.  Reduce this number
   for each new parameter added, so as to keep the size of the
   structure the same.  This will avoid problems when linking modules
   that may have been compiled with different revisions of this
   structure.  Also, always add new elements to the very bottom of
   the list, but above dummy.
   */

   int dummy[50];

   };

/* 
Allowed number of open units = DOM_UNITS-1.

F_units[i] is set to 1 if unit i is allocated.

F_info[i] holds the information about unit i.
*/

#define DOM_NUNITS	100

#ifdef DOMAINSUBS

struct dom_info *f_info[DOM_NUNITS];

int f_units[DOM_NUNITS] = 
   { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

char _d_dirname[256];		/* Directory to search for files */

#else 

extern struct dom_info *f_info[DOM_NUNITS];

extern int f_units[DOM_NUNITS];

extern char _d_dirname[256];

#endif


/* 
Error codes for DOMSUBS routines.

JJH, 4/86, CIT
*/

#ifdef DOMAINSUBS

int _dom_error = 0;		/* Value of last error condition set	*/
int _dom_err_f = -1;		/* File number at time of error		*/
int _dom_err_p = -1;		/* Picture number at time of error	*/

#else

extern int _dom_error;
extern int _dom_err_f;
extern int _dom_err_p;

#endif

#define DOM$_NCODES	35	/* Number of error codes			*/

#define DOM$_NORMAL	0  /* No error has occurred				*/
#define DOM$_GETUNIT	1  /* A unit number could not be allocated		*/
#define DOM$_NOTYPE	2  /* Bad data type specification			*/
#define DOM$_NOUNIT	3  /* Invalid picture unit number			*/
#define DOM$_NOCRHD	4  /* Header file create failed				*/
#define DOM$_NOCRIM	5  /* Image file create failed				*/
#define DOM$_IMEXISTS	6  /* Cannot create image file because it exists	*/
#define DOM$_HDEXISTS	7  /* Cannot create header file because it exists	*/
#define DOM$_NOOPHD	8  /* Cannot open header file				*/
#define DOM$_NOOPIM	9  /* Cannot open image file				*/
#define DOM$_NODUPHDR	10 /* Could not dup a header file descriptor		*/
#define DOM$_NODUPIM	11 /* Could not dup an image file descriptor		*/
#define DOM$_NODUPFAB	12 /* Could not dup a supposedly valid FAB		*/
#define DOM$_ISMAPPED	13 /* Mapped file, disallow open for record access	*/
#define DOM$_NOTMAPPED	14 /* Cannot map file currently open for rec access	*/
#define DOM$_PICOPEN	15 /* Specified file and picture is already open	*/
#define DOM$_FNOUT	16 /* File number is out of bounds			*/
#define DOM$_PGTNPICS	17 /* Less than requested number of picts in file	*/
#define DOM$_NONFILL	18 /* Error filling a newly created image file		*/
#define DOM$_NOEFILL	19 /* Error filling an extension to an image file	*/
#define DOM$_NORDHD	20 /* Error reading header file				*/
#define DOM$_NORDIM	21 /* Error reading image file				*/
#define DOM$_NOWRNHD	22 /* Error writing to new header file 			*/
#define DOM$_NOWRHD	23 /* Error writing to header file			*/
#define DOM$_NOWRIM	24 /* Error writing to image file			*/
#define DOM$_MAPFAIL	25 /* Failure mapping file				*/
#define DOM$_INTDECODE	26 /* Error decoding INT type header card		*/
#define DOM$_LOGDECODE	27 /* Error decoding LOGICAL type header card		*/
#define DOM$_REALDECODE	28 /* Error decoding REAL type header card		*/
#define DOM$_STRDECODE	29 /* Error decoding STRING type header card		*/
#define DOM$_NOCARD	30 /* Could not find card in header			*/
#define DOM$_IMSEEK	31 /* Error positioning I/O pointer in an image file	*/
#define	DOM$_HDSEEK	32 /* Error positioning I/O pointer in a header file	*/
#define	DOM$_VMALLOC	33 /* Error allocating virtual memory			*/
#define DOM$_VMFREE	34 /* Error freeing virtual memory			*/

/* Messages to go with error codes */


#ifdef DOMAINSUBS

char *_dom_err_msg[DOM$_NCODES+1] = {

"DOM$_NORMAL:	No error has occurred",
"DOM$_GETUNIT:	A unit number could not be allocated",
"DOM$_NOTYPE:	Bad data type specification",
"DOM$_NOUNIT:	Invalid picture unit number",
"DOM$_NOCRHD:	Header file create failed",
"DOM$_NOCRIM:	Image file create failed",
"DOM$_IMEXISTS:	Cannot create image file because it exists",
"DOM$_HDEXISTS:	Cannot create header file because it exists",
"DOM$_NOOPHD:	Cannot open header file",
"DOM$_NOOPIM:	Cannot open image file",
"DOM$_NODUPHDR:	Could not dup a header file descriptor",
"DOM$_NODUPIM:	Could not dup an image file descriptor",
"DOM$_NODUPFAB:	Could not dup a supposedly valid FAB",
"DOM$_ISMAPPED:	Mapped file, disallow open for record access",
"DOM$_NOTMAPPED: Cannot map file currently open for rec access",
"DOM$_PICOPEN:	Specified file and picture is already open",
"DOM$_FNOUT:	File number is out of bounds",
"DOM$_PGTNPICS:	Less than requested number of picts in file",
"DOM$_NONFILL:	Error filling a newly created image file",
"DOM$_NOEFILL:	Error filling an extension to an image file",
"DOM$_NORDHD:	Error reading header file",
"DOM$_NORDIM:	Error reading image file",
"DOM$_NOWRNHD:	Error writing to new header file",
"DOM$_NOWRHD:	Error writing to header file",
"DOM$_NOWRIM:	Error writing to image file",
"DOM$_MAPFAIL:	Failure mapping file",
"DOM$_INTDECODE: Error decoding INT type header card",
"DOM$_LOGDECODE: Error decoding LOGICAL type header card",
"DOM$_REALDECODE: Error decoding REAL type header card",
"DOM$_STRDECODE: Error decoding STRING type header card",
"DOM$_NOCARD:	Could not find card in header",
"DOM$_IMSEEK:	Error positioning I/O pointer in an image file",
"DOM$_HDSEEK:	Error positioning I/O pointer in a header file",
"DOM$_VMALLOC:	Error allocating virtual memory",
"DOM$_VMFREE:	Error freeing virtual memory",
"DOM$_BADERRN:	Invalid DOMSUBS error number"
};

#else

extern char *_dom_err_msg[];

#endif

/* Errors come in classes */

#define	DOM$_NCLASSES	6	/* Number of error classes	*/

#define	DOM$C_NONE	0	/* No error			*/
#define DOM$C_PARAM	1	/* Bad parameter specification	*/
#define DOM$C_ACCESS	2	/* Cannot access a file		*/
#define DOM$C_IO	3	/* Failure in file I/O		*/
#define DOM$C_FITS	4	/* Format error in FITS header	*/
#define DOM$C_VMS	5	/* VMS unable to perform service*/

#ifdef DOMAINSUBS

char *_dom_errc_msg[DOM$_NCLASSES] = {

"DOM$C_NONE: 	No error",
"DOM$C_PARAM:	Bad parameter specification",
"DOM$C_ACCESS:	Cannot access a file",
"DOM$C_IO:	Failure in file I/O",
"DOM$C_FITS:	Format error in FITS header",
"DOM$C_VMS:	VMS was unable to perform a requested service"
};

/* Classes of error codes */

int _dom_err_class[DOM$_NCODES+1] = {

/* DOM$_NORMAL	*/	DOM$C_NONE,
/* DOM$_GETUNIT	*/	DOM$C_PARAM,
/* DOM$_NOTYPE	*/	DOM$C_PARAM,
/* DOM$_NOUNIT	*/	DOM$C_PARAM,
/* DOM$_NOCRHD	*/	DOM$C_ACCESS,
/* DOM$_NOCRIM	*/	DOM$C_ACCESS,
/* DOM$_IMEXISTS*/	DOM$C_ACCESS,
/* DOM$_HDEXISTS*/	DOM$C_ACCESS,
/* DOM$_NOOPHD	*/	DOM$C_ACCESS,
/* DOM$_NOOPIM	*/	DOM$C_ACCESS,
/* DOM$_NODUPHDR*/	DOM$C_ACCESS,
/* DOM$_NODUPIM	*/	DOM$C_ACCESS,
/* DOM$_NODUPFAB*/	DOM$C_ACCESS,
/* DOM$_ISMAPPED*/	DOM$C_ACCESS,
/* DOM$_NOTMAPPED*/	DOM$C_ACCESS,
/* DOM$_PICOPEN	*/	DOM$C_ACCESS,
/* DOM$_FNOUT	*/	DOM$C_PARAM,
/* DOM$_PGTNPICS*/	DOM$C_PARAM,
/* DOM$_NONFILL	*/	DOM$C_IO,
/* DOM$_NOEFILL	*/	DOM$C_IO,
/* DOM$_NORDHD	*/	DOM$C_IO,
/* DOM$_NORDIM	*/	DOM$C_IO,
/* DOM$_NOWRNHD	*/	DOM$C_IO,
/* DOM$_NOWRHD	*/	DOM$C_IO,
/* DOM$_NOWRIM	*/	DOM$C_IO,
/* DOM$_MAPFAIL	*/	DOM$C_IO,
/* DOM$_INTDECODE*/	DOM$C_FITS,
/* DOM$_LOGDECODE*/	DOM$C_FITS,
/* DOM$_REALDECODE*/	DOM$C_FITS,
/* DOM$_STRDECODE*/	DOM$C_FITS,
/* DOM$_NOCARD	*/	DOM$C_FITS,
/* DOM$_IMSEEK	*/	DOM$C_IO,
/* DOM$_HDSEEK	*/	DOM$C_IO,
/* DOM$_VMALLOC	*/	DOM$C_VMS,
/* DOM$_VMFREE	*/	DOM$C_VMS
};

#else

extern int _dom_err_class[];

extern char *_dom_errc_msg[];

#endif

/* 
ERR() -- Set Domsubs error condition

Macro to set error code, free unit, and return -1.
Set u to 0 if unit should not be freed.  Set u to unit number if it should.
*/

#ifdef DOMAINSUBS

int _dom_verbose = 1;

#else

extern int _dom_verbose;

#endif

#include stdio

#define ERR(errcode,unit,wipe)			\
   {						\
   _dom_errset(errcode,unit,wipe);		\
   return(-1);					\
   }

