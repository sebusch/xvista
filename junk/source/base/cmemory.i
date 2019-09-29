 
 
 
 
 
 

 
 
 
 
 
 
 

 
 

















 



 


















 































































 


 


 

 


 

 


 




 


 















 

 






 


 







 


 

 
















 


 



 

 




 

 



 


 


 






 


 


 


 
 










 







 



 



 



 



 

 






 




 




 

 



 



 


 





 



 







 

 



 

		 

		 
typedef int ptrdiff_t;

typedef unsigned	size_t;

typedef long int	wchar_t;



   




 


















 



 


















 


 

 



 

		 

		 





   




 

 
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;

 
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
 typedef signed long long int __int64_t;
 typedef unsigned long long int __uint64_t;

 
 typedef long long int __quad_t;
 typedef unsigned long long int __u_quad_t;





























 


















 




 


 




 


 typedef unsigned long long int __dev_t;	 
 typedef unsigned int __uid_t;	 
 typedef unsigned int __gid_t;	 
 typedef unsigned long int __ino_t;	 
 typedef unsigned long long int __ino64_t;	 
 typedef unsigned int __mode_t;	 
 typedef unsigned int __nlink_t;	 
 typedef long int __off_t;	 
 typedef long long int __off64_t;	 
 typedef int __pid_t;	 
 typedef struct { int __val[2]; } __fsid_t;	 
 typedef long int __clock_t;	 
 typedef unsigned long int __rlim_t;	 
 typedef unsigned long long int __rlim64_t;	 
 typedef unsigned int __id_t;		 
 typedef long int __time_t;	 
 typedef unsigned int __useconds_t;  
 typedef long int __suseconds_t;  

 typedef int __daddr_t;	 
 typedef long int __swblk_t;	 
 typedef int __key_t;	 

 
 typedef int __clockid_t;

 
 typedef int __timer_t;

 
 typedef long int __blksize_t;

 

 
 typedef long int __blkcnt_t;
 typedef long long int __blkcnt64_t;

 
 typedef unsigned long int __fsblkcnt_t;
 typedef unsigned long long int __fsblkcnt64_t;

 
 typedef unsigned long int __fsfilcnt_t;
 typedef unsigned long long int __fsfilcnt64_t;

 typedef int __ssize_t;  


 
typedef __off64_t __loff_t;	 
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;

 
 typedef int __intptr_t;

 
 typedef unsigned int __socklen_t;







 
typedef struct _IO_FILE FILE;






 
typedef struct _IO_FILE __FILE;






























 



 


 

 

 



 

		 

		 





   




 



 
typedef unsigned int wint_t;
















 




 



 

 



 

		 

		 





   




 


















 





 

 


 
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    char __wchb[4];
  } __value;		 
} __mbstate_t;



 

typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
















 



 


















 




 



 

 



 

		 

		 





   




 



 

 





 

 

 



 

		 

		 





   




 

 

 
enum
{
  __GCONV_OK = 0,
  __GCONV_NOCONV,
  __GCONV_NODB,
  __GCONV_NOMEM,

  __GCONV_EMPTY_INPUT,
  __GCONV_FULL_OUTPUT,
  __GCONV_ILLEGAL_INPUT,
  __GCONV_INCOMPLETE_INPUT,

  __GCONV_ILLEGAL_DESCRIPTOR,
  __GCONV_INTERNAL_ERROR
};


 
enum
{
  __GCONV_IS_LAST = 0x0001,
  __GCONV_IGNORE_ERRORS = 0x0002
};


 
struct __gconv_step;
struct __gconv_step_data;
struct __gconv_loaded_object;
struct __gconv_trans_data;


 
typedef int (*__gconv_fct) (struct __gconv_step *, struct __gconv_step_data *,
			    __const unsigned char **, __const unsigned char *,
			    unsigned char **, size_t *, int, int);

 
typedef wint_t (*__gconv_btowc_fct) (struct __gconv_step *, unsigned char);

 
typedef int (*__gconv_init_fct) (struct __gconv_step *);
typedef void (*__gconv_end_fct) (struct __gconv_step *);


 
typedef int (*__gconv_trans_fct) (struct __gconv_step *,
				  struct __gconv_step_data *, void *,
				  __const unsigned char *,
				  __const unsigned char **,
				  __const unsigned char *, unsigned char **,
				  size_t *);

 
typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
					  __const unsigned char *,
					  unsigned char *, unsigned char *);

 
typedef int (*__gconv_trans_query_fct) (__const char *, __const char ***,
					size_t *);

 
typedef int (*__gconv_trans_init_fct) (void **, const char *);
typedef void (*__gconv_trans_end_fct) (void *);

struct __gconv_trans_data
{
   
  __gconv_trans_fct __trans_fct;
  __gconv_trans_context_fct __trans_context_fct;
  __gconv_trans_end_fct __trans_end_fct;
  void *__data;
  struct __gconv_trans_data *__next;
};


 
struct __gconv_step
{
  struct __gconv_loaded_object *__shlib_handle;
  __const char *__modname;

  int __counter;

  char *__from_name;
  char *__to_name;

  __gconv_fct __fct;
  __gconv_btowc_fct __btowc_fct;
  __gconv_init_fct __init_fct;
  __gconv_end_fct __end_fct;

  
 
  int __min_needed_from;
  int __max_needed_from;
  int __min_needed_to;
  int __max_needed_to;

   
  int __stateful;

  void *__data;		 
};


 
struct __gconv_step_data
{
  unsigned char *__outbuf;     
  unsigned char *__outbufend; 
 

   
  int __flags;

  
 
  int __invocation_counter;

  
 
  int __internal_use;

  __mbstate_t *__statep;
  __mbstate_t __state;	
 

   
  struct __gconv_trans_data *__trans;
};


 
typedef struct __gconv_info
{
  size_t __nsteps;
  struct __gconv_step *__steps;
   struct __gconv_step_data __data [];
} *__gconv_t;

typedef union
{
  struct __gconv_info __cd;
  struct
  {
    struct __gconv_info __cd;
    struct __gconv_step_data __data;
  } __combined;
} _G_iconv_t;

typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));



 




 


 



 

 
 










 










 












 








 

typedef char * va_list;

 
typedef	va_list	__gnuc_va_list;








 








 



 


struct _IO_jump_t;  struct _IO_FILE;

 
typedef void _IO_lock_t;


 

struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;
  
 
   
  int _pos;
};

 
enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};


struct _IO_FILE {
  int _flags;		 

   
   
  char* _IO_read_ptr;	 
  char* _IO_read_end;	 
  char* _IO_read_base;	 
  char* _IO_write_base;	 
  char* _IO_write_ptr;	 
  char* _IO_write_end;	 
  char* _IO_buf_base;	 
  char* _IO_buf_end;	 
   
  char *_IO_save_base;  
  char *_IO_backup_base;   
  char *_IO_save_end;  

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _flags2;
  __off_t _old_offset;  

   
  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];

   

  _IO_lock_t *_lock;
  __off64_t _offset;
  void *__pad1;
  void *__pad2;
  int _mode;
   
  char _unused2[15 * sizeof (int) - 2 * sizeof (void *)];
};

typedef struct _IO_FILE _IO_FILE;

struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;


 


 
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);






 
typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
				 size_t __n);






 
typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);

 
typedef int __io_close_fn (void *__cookie);





extern int __underflow (_IO_FILE *) ;
extern int __uflow (_IO_FILE *) ;
extern int __overflow (_IO_FILE *, int) ;
extern wint_t __wunderflow (_IO_FILE *) ;
extern wint_t __wuflow (_IO_FILE *) ;
extern wint_t __woverflow (_IO_FILE *, wint_t) ;




extern int _IO_getc (_IO_FILE *__fp) ;
extern int _IO_putc (int __c, _IO_FILE *__fp) ;
extern int _IO_feof (_IO_FILE *__fp) ;
extern int _IO_ferror (_IO_FILE *__fp) ;

extern int _IO_peekc_locked (_IO_FILE *__fp) ;

 

extern void _IO_flockfile (_IO_FILE *) ;
extern void _IO_funlockfile (_IO_FILE *) ;
extern int _IO_ftrylockfile (_IO_FILE *) ;


extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
			__gnuc_va_list, int *__restrict) ;
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
			 __gnuc_va_list) ;
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t) ;
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t) ;

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int) ;
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int) ;

extern void _IO_free_backup_area (_IO_FILE *) ;





 

typedef _G_fpos_t fpos_t;


 


 



 



 












 
















 







 
extern struct _IO_FILE *stdin;		 
extern struct _IO_FILE *stdout;		 
extern struct _IO_FILE *stderr;		 
 


 
extern int remove (__const char *__filename) ;
 
extern int rename (__const char *__old, __const char *__new) ;







 
extern FILE *tmpfile (void);


 
extern char *tmpnam (char *__s) ;










 
extern int fclose (FILE *__stream);



 
extern int fflush (FILE *__stream);









 
extern FILE *fopen (__const char *__restrict __filename,
		    __const char *__restrict __modes);



 
extern FILE *freopen (__const char *__restrict __filename,
		      __const char *__restrict __modes,
		      FILE *__restrict __stream);


 
extern FILE *fdopen (int __fd, __const char *__modes) ;





 
extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) ;


 
extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
		    int __modes, size_t __n) ;








 
extern int fprintf (FILE *__restrict __stream,
		    __const char *__restrict __format, ...);



 
extern int printf (__const char *__restrict __format, ...);
 
extern int sprintf (char *__restrict __s,
		    __const char *__restrict __format, ...) ;




 
extern int vfprintf (FILE *__restrict __s, __const char *__restrict __format,
		     __gnuc_va_list __arg);



 
extern int vprintf (__const char *__restrict __format, __gnuc_va_list __arg);
 
extern int vsprintf (char *__restrict __s, __const char *__restrict __format,
		     __gnuc_va_list __arg) ;









 
extern int fscanf (FILE *__restrict __stream,
		   __const char *__restrict __format, ...);



 
extern int scanf (__const char *__restrict __format, ...);
 
extern int sscanf (__const char *__restrict __s,
		   __const char *__restrict __format, ...) ;








 
extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);




 
extern int getchar (void);



 




 
extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);










 
extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);




 
extern int putchar (int __c);



 





 
extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);








 
extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream);





 
extern char *gets (char *__s);










 
extern int fputs (__const char *__restrict __s, FILE *__restrict __stream);




 
extern int puts (__const char *__s);





 
extern int ungetc (int __c, FILE *__stream);





 
extern size_t fread (void *__restrict __ptr, size_t __size,
		     size_t __n, FILE *__restrict __stream);



 
extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
		      size_t __n, FILE *__restrict __s);









 
extern int fseek (FILE *__stream, long int __off, int __whence);



 
extern long int ftell (FILE *__stream);



 
extern void rewind (FILE *__stream);





 






 
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);



 
extern int fsetpos (FILE *__stream, __const fpos_t *__pos);




 
extern void clearerr (FILE *__stream) ;
 
extern int feof (FILE *__stream) ;
 
extern int ferror (FILE *__stream) ;








 
extern void perror (__const char *__s);





 

















 


 



 
extern int fileno (FILE *__stream) ;





 
extern char *ctermid (char *__s) ;






 

 
extern void flockfile (FILE *__stream) ;


 
extern int ftrylockfile (FILE *__stream) ;

 
extern void funlockfile (FILE *__stream) ;



 




 

cctapewrite_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  tapewrite_ (*location,isr,ier,isc,iec);
}
 
ccaverage_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  average_ (*location,isr,ier,isc,iec);
}
 
ccoldsky_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  oldsky_ (*location,isr,ier,isc,iec);
}
 
ccsky_ (location,nr,nc)
int *location,*nr,*nc;
{
  sky_ (*location,nr,nc);
}
 
ccwsky_ (location,nr,nc)
int *location,*nr,*nc;
{
  wsky_ (*location,nr,nc);
}
 
ccanalbox_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  analbox_ (*location,isr,ier,isc,iec);
}
 
ccaxes_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  axes_ (*location,isr,ier,isc,iec);
}
 
cconeimagemath_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  oneimagemath_ (*location,isr,ier,isc,iec);
}
 
ccdoatan2_ (location,isr,ier,isc,iec,locb,jsr,jer,jsc,jec)
int *location,*isr,*ier,*isc,*iec;
int *locb,*jsr,*jer,*jsc,*jec;
{
  doatan2_ (*location,isr,ier,isc,iec,*locb,jsr,jer,jsc,jec);
}
 
ccbaseline_ (location,nr,nc)
int *location,*nr,*nc;
{
  baseline_ (*location,nr,nc);
}
 
ccclip_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  clip_ (*location,isr,ier,isc,iec);
}
 
ccrotate_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  rotate_ (*location,isr,ier,isc,iec);
}
 
ccflip_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  flip_ (*location,isr,ier,isc,iec);
}
 
ccfitspline_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  fitspline_ (*location,isr,ier,isc,iec);
}
 
ccispline_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  ispline_ (*location,isr,ier,isc,iec);
}

ccradplot_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  radplot_ (*location,isr,ier,isc,iec);
}
 
ccoverlay_(location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  overlay_(*location,isr,ier,isc,iec);
}
 
ccmakehistogram_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  makehistogram_ (*location,isr,ier,isc,iec);
}
 
ccthreed_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  threed_ (*location,isr,ier,isc,iec);
}
 
ccvideo_ (location,isr,ier,isc,iec,locb,isrb,ierb,iscb,iecb)
int *location,*isr,*ier,*isc,*iec,*locb,*isrb,*ierb,*iscb,*iecb;
{
  video_ (*location,isr,ier,isc,iec,*locb,isrb,ierb,iscb,iecb);
}
 
ccimpost_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  impost_ (*location,isr,ier,isc,iec);
}
 
ccbinanimage_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  binanimage_ (*location,isr,ier,isc,iec);
}
 
ccsurface_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  surface_ (*location,isr,ier,isc,iec);
}
 
ccshift_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  shift_ (*location,isr,ier,isc,iec);
}
 
ccpradial_ (location,nrow,ncol)
int *location,*nrow,*ncol;
{
  pradial_ (*location,nrow,ncol);
}

ccdopradial_(a,nrow,ncol,sb,nn,maxwid,maxlen)
int *a,*nrow,*ncol,*sb,*nn,*maxwid,*maxlen;
{
  dopradial_(a,nrow,ncol,*sb,*nn,maxwid,maxlen);
}
 
cctemplate_ (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
int *location,*isr,*ier,*isc,*iec;
int *loc2,*isr2,*ier2,*isc2,*iec2;
{
  template_ (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}
 
ccrecon_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  recon_ (*location,isr,ier,isc,iec);
}
 
ccsmooth_ (location,nr,nc)
int *location,*nr,*nc;
{
  smooth_ (*location,nr,nc);
}
 
cczap_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  zap_ (*location,isr,ier,isc,iec);
}
 
ccaperture_ (location,nr,nc)
int *location,*nr,*nc;
{
  aperture_ (*location,nr,nc);
}
 
ccprofile_ (location,nr,nc)
int *location,*nr,*nc;
{
  profile_ (*location,nr,nc);
}
 
ccannulus_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  annulus_ (*location,isr,ier,isc,iec);
}
 
cccrosscor_ (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
int *location,*isr,*ier,*isc,*iec;
int *loc2,*isr2,*ier2,*isc2,*iec2;
{
  crosscor_ (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}
 
cccolfix_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  colfix_ (*location,isr,ier,isc,iec);
}
 
ccinterp_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  interp_ (*location,isr,ier,isc,iec);
}
 
ccmash_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  mash_ (*location,isr,ier,isc,iec);
}
 
ccextract_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  extract_ (*location,isr,ier,isc,iec);
}
 
ccstretch_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  stretch_ (*location,isr,ier,isc,iec);
}
 
ccspectroid_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  spectroid_ (*location,isr,ier,isc,iec);
}

 
cclineid_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  lineid_ (*location,isr,ier,isc,iec);
}
 
ccwavescale_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  wavescale_ (*location,isr,ier,isc,iec);
}
 
ccewave_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  ewave_ (*location,isr,ier,isc,iec);
}
 
cccopyw_ (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
int *location,*isr,*ier,*isc,*iec;
int *loc2,*isr2,*ier2,*isc2,*iec2;
{
  copyw_ (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}
 
ccalign_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  align_ (*location,isr,ier,isc,iec);
}
 
ccextinct_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  extinct_ (*location,isr,ier,isc,iec);
}
ccrefract_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  refract_ (*location,isr,ier,isc,iec);
}
 
ccfluxcal_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  fluxcal_ (*location,isr,ier,isc,iec);
}
 
ccskyline_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  skyline_ (*location,isr,ier,isc,iec);
}
 
ccpolyfit_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  polyfit_ (*location,isr,ier,isc,iec);
}
 
ccextspec_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  extspec_ (*location,isr,ier,isc,iec);
}
 
ccfindpeak_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  findpeak_ (*location,isr,ier,isc,iec);
}
 
ccrowfit_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  rowfit_ (*location,isr,ier,isc,iec);
}
 
ccfindshift_ (location,isr,ier,isc,iec,loc2,isr2,ier2,isc2,iec2)
int *location,*isr,*ier,*isc,*iec;
int *loc2,*isr2,*ier2,*isc2,*iec2;
{
  findshift_ (*location,isr,ier,isc,iec,*loc2,isr2,ier2,isc2,iec2);
}

 

ccbbspec_(locim,npix,T,lam0,disp,ierr)
int *locim, *npix, *T, *lam0, *disp, *ierr;
{
  bbspec_(*locim,npix,T,lam0,disp,ierr);
}
 
ccmarkstar_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  markstar_ (*location,isr,ier,isc,iec);
}
 
ccarea_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  printf("calling area\n");
  areag_ (*location,isr,ier,isc,iec);
}
 
ccphotons_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  photons_ (*location,isr,ier,isc,iec);
}
 
ccautomark_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  automark_ (*location,isr,ier,isc,iec);
}
 
ccaperstar_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  aperstar_ (*location,isr,ier,isc,iec);
}
 
ccaedit_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  aedit_ (*location,isr,ier,isc,iec);
}
 
ccemag_ (location,nr,nc)
int *location,*nr,*nc;
{
  emag_ (*location,nr,nc);
}
 
ccgetwfpcad_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  getwfpcad_ (*location,isr,ier,isc,iec);
}
 
ccatodsim_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  atodsim_ (*location,isr,ier,isc,iec);
}
 
ccatodfix_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  atodfix_ (*location,isr,ier,isc,iec);
}
 
ccfilter_ (location,nr,nc)
int *location,*nr,*nc;
{
  filter_ (*location,nr,nc);
}

cchistnuc_ (a,nr,nc)
int *a, *nr, *nc;
{
  histnuc_ (*a,nr,nc);
}

ccsimulnuc_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  simulnuc_ (*location,isr,ier,isc,iec);
}

ccbdfit_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  bdfit_ (*location,isr,ier,isc,iec);
}
 
 
ccrenuc_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  renuc_ (*location,isr,ier,isc,iec);
}
 
ccpolargrid_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  polargrid_ (*location,isr,ier,isc,iec);
}
 
ccrancr_ (location,nr,nc,locb,nrowb,ncolb)
int *location,*nr,*nc,*locb,*nrowb,*ncolb;
{
  rancr_ (*location,nr,nc,*locb,nrowb,ncolb);
}
 
ccstreak_ (location,nr,nc,locb,nrowb,ncolb)
int *location,*nr,*nc,*locb,*nrowb,*ncolb;
{
  streak_ (*location,nr,nc,*locb,nrowb,ncolb);
}
 
cctabtran_ (location,nr,nc)
int *location,*nr,*nc;
{
  tabtran_ (*location,nr,nc);
}
 
ccdevauc_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  devauc_ (*location,isr,ier,isc,iec);
}
 
cctext_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  text_ (*location,isr,ier,isc,iec);
}

ccregistar_ (location,isr,ier,isc,iec)
int *location,*isr,*ier,*isc,*iec;
{
  registar_ (*location,isr,ier,isc,iec);
}

ccspindex_ (a,isra,iera,isca,ieca)
int *a,*isra,*iera,*isca,*ieca;
{
  spindex_ (*a,isra,iera,isca,ieca);
}

cclincomb_ (a,isra,iera,isca,ieca)
int *a,*isra,*iera,*isca,*ieca;
{
  lincomb_ (*a,isra,iera,isca,ieca);
}

ccfquo_ (a,isra,iera,isca,ieca,b,isrb,ierb,iscb,iecb)
int *a,*isra,*iera,*isca,*ieca,*b,*isrb,*ierb,*iscb,*iecb;
{
  fquo_ (*a,isra,iera,isca,ieca,*b,isrb,ierb,iscb,iecb);
}

ccrebin_ (b,a,isr,ier,isc,iec,nbins,bin)
int *b,*a,*isr,*ier,*isc,*iec,*nbins,*bin;
{
  rebin_ (*b,a,isr,ier,isc,iec,nbins,bin);
}

ccrebinlc_ (b,a,isr,ier,isc,iec,nbins,bin,mod,isrm,ierm,iscm,iecm)
int *b,*a,*isr,*ier,*isc,*iec,*nbins,*bin,*mod,*isrm,*ierm,*iscm,*iecm;
{
  rebinlc_ (*b,a,isr,ier,isc,iec,nbins,bin,*mod,isrm,ierm,iscm,iecm);
}

ccreal2dfft_ (a,nra,nca,b,nrb,ncb,isign,work)
int *a,*nra,*nca,*b,*nrb,*ncb,*isign,*work;
{
  real2dfft_ (*a,nra,nca,*b,nrb,ncb,isign,work);
}

ccpows_ (a,nra,nca,b,nrb,ncb)
int *a,*nra,*nca,*b,*nrb,*ncb;
{
  pows_ (*a,nra,nca,*b,nrb,ncb);
}

cccabs_ (a,b,nr,nc)
int *a,*b,*nr,*nc;
{
  cabs_ (*a,*b,nr,nc);
}

ccconj_ (a,b,nr,nc)
int *a,*b,*nr,*nc;
{
  conj_ (*a,*b,nr,nc);
}

ccreal_ (a,b,nr,nc)
int *a,*b,*nr,*nc;
{
  real_ (*a,*b,nr,nc);
}

ccimag_ (a,b,nr,nc)
int *a,*b,*nr,*nc;
{
  imag_ (*a,*b,nr,nc);
}

cccmplx_ (a,b,c,nr,nc)
int *a,*b,*c,*nr,*nc;
{
  cmplx_ (*a,*b,*c,nr,nc);
}

cccmul_ (a,isra,iera,isca,ieca,b,isrb,ierb,iscb,iecb)
int *a,*isra,*iera,*isca,*ieca,*b,*isrb,*ierb,*iscb,*iecb;
{
  cmul_ (*a,isra,iera,isca,ieca,*b,isrb,ierb,iscb,iecb);
}

cccdiv_ (a,isra,iera,isca,ieca,b,isrb,ierb,iscb,iecb)
int *a,*isra,*iera,*isca,*ieca,*b,*isrb,*ierb,*iscb,*iecb;
{
  cdiv_ (*a,isra,iera,isca,ieca,*b,isrb,ierb,iscb,iecb);
}

ccautocen_ (a,nr,nc)
int *a, *nr, *nc;
{
  autocen_ (*a,nr,nc);
}

ccsector_ (a,nr,nc)
int *a, *nr, *nc;
{
  sector_ (*a,nr,nc);
}

ccegal_ (a,nr,nc)
int *a, *nr, *nc;
{
  egal_ (*a,nr,nc);
}

ccpsfmodel_ (a,nr,nc)
int *a, *nr, *nc;
{
  psfmodel_ (*a,nr,nc);
}

ccpsftrans_ (a,nr,nc)
int *a, *nr, *nc;
{
  psftrans_ (*a,nr,nc);
}

ccellmag_ (a,nr,nc)
int *a, *nr, *nc;
{
  ellmag_ (*a,nr,nc);
}

ccsurfbright_ (a,nr,nc)
int *a, *nr, *nc;
{
  surfbright_ (*a,nr,nc);
}


 
 
 
 
 
 
cclibfill_ (location,nbox,nlib,gauss,psf,npsf,maxpsf,amin,amax,app,xpsf,ypsf)
int *location,*nbox,*nlib,*gauss,*psf,*npsf,*maxpsf,*amin,*amax,*app,*xpsf,*ypsf;
{
  libfill_(*location,nbox,nlib,gauss,psf,npsf,maxpsf,amin,amax,app,xpsf,ypsf);
}
 
cclibget_(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy)
int *location,*nbox,*nlib,*ix,*iy,*x,*y,*value,*dvdx,*dvdy;
{
  libget_(*location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy);
}
 
cclibmul_(location,nbox,fact)
int *location,*nbox,*fact;
{
  libmul_(*location,nbox,fact);
}
 
ccliball_(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy,nall,nbyte)
int *location;
int *nbox,*nlib,*ix,*iy,*x,*y,*nall,*nbyte;
float *value,*dvdx,*dvdy;
{
  int i;




 
  for (i=0;i<*nall;i++) {
   libget_(*location+i*(*nbyte),nbox,nlib,ix,iy,x,y,value+i,dvdx+i,dvdy+i);
  }
}

cclibone_(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy,nall,nbyte,nsamp,offset,nskip)
int **location,*nbox,*nlib,*ix,*iy,*nall,*nbyte,*offset,*nsamp,*nskip;
float *value,*x,*y,*dvdx,*dvdy;
{
  int nside, ixfrac, iyfrac, ixpix, iypix;
  float spacing;

  *nskip = 0;
  if (*nsamp>1) {
    nside = *nbox / *nlib ;
    spacing = 1. / *nlib ;
    ixfrac = (*x - (int)*x) / spacing ;
    ixpix = (*ix - (int)*x + nside/2) * (*nlib) + (*nlib - ixfrac);
    if ( ixpix<2 || ixpix>(*nbox) ) {
      *nskip = 1;
    } else {
      iyfrac = (*y - (int)*y) / spacing ;
      iypix = (*iy - (int)*y + nside/2) * (*nlib) + (*nlib - iyfrac);
      if ( iypix<2 || iypix>(*nbox) ) 
        *nskip = 1;
    }
  } 
  libget_(*location+*offset*(*nbyte/4)+(*nskip)*(*nbyte/4)*(*nall),nbox,nlib+(*nskip),ix,iy,x,y,value,dvdx,dvdy); 
}

cclibnew_(location,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy,nall,nbyte,nsamp,ixpsf,iypsf,nskip,deriv,error)
int *nbox,*nlib,*ix,*iy,*nall,*nbyte,*nsamp,*nskip,*deriv,*error;
short *ixpsf, *iypsf;
float **location,*value,*x,*y,*dvdx,*dvdy;
{


 

  int nside, ixfrac, iyfrac, ixpix, iypix, iypix2;
  int iix, iiy, iii, offset, o1, o2, o3, o4, o5, o6, nb4, tmp;
  float spacing,xfrac,yfrac,val1,val2,*ltmp,*ltmp2,*ltmp4;
  double sqrt();

  *nskip = 0;
  nb4 = *nbyte / 4;
  *error = 0;

 
  spacing = 1. / *nlib ;
  nside = *nbox * spacing ;
  nside = nside / 2 ;
  xfrac = (*x - (int)*x) * (*nlib) ;
  ixfrac = (int) xfrac ;
  ixpix = (*ix - (int)*x + nside) * (*nlib) + (*nlib - ixfrac);

   
  if ( ixpix<2 || ixpix>(*nbox) ) {
    *nskip = 1;
  } else {
     
    yfrac = (*y - (int)*y) * (*nlib) ;
    iyfrac = (int) yfrac;
    iypix = (*iy - (int)*y + nside) * (*nlib) + (*nlib - iyfrac);
    if ( iypix<2 || iypix>(*nbox) ) 
      *nskip = 1;
  }

 
  if (*nskip == 1) {
    spacing = 1. / *(nlib+1) ;
    nside = *nbox * spacing ;
    nside = nside / 2 ;
    xfrac = (*x - (int)*x) * *(nlib+1) ;
    ixfrac = (int) xfrac ;
    ixpix = (*ix - (int)*x + nside) * (*(nlib+1)) + (*(nlib+1) - ixfrac);
    if ( (ixpix<2) || (ixpix>(*nbox)) ) {
        printf("%f %d %d %d %d %d %d\n",
               *x,*(nlib+1),*ix,nside,ixfrac,ixpix,*nbox);
        printf("x Pixel out of library range\n");
        *error = 1;
        for (iii=0;iii<4;iii++) {
          *(value+iii) = 0.;
          *(dvdx+iii) = 0.;
          *(dvdy+iii) = 0.;
        }
        return;
    } else {
       
      yfrac = (*y - (int)*y) * *(nlib+1) ;
      iyfrac = (int) yfrac;
      iypix = (*iy - (int)*y + nside) * (*(nlib+1)) + (*(nlib+1) - iyfrac);
      if ( (iypix<2) || (iypix>(*nbox)) ) { 
        printf("y Pixel out of library range\n");
	printf("%f %d %d %d %d %d %d\n",
               *y,*(nlib+1),*iy,nside,iyfrac,iypix,*nbox);
        *error = 1;
        for (iii=0;iii<4;iii++) {
          *(value+iii) = 0.;
          *(dvdx+iii) = 0.;
          *(dvdy+iii) = 0.;
        }
        return;
      }
    }

 
  }


 
    o1 = (iypix-1)*(*nbox)+(ixpix-2) ;
    o2 = o1 + 1 ;
    o3 = o1 - *nbox;
    o4 = o3 + 1 ;

 
    iii = 0;
    tmp = (int)sqrt((float)*nall);
    offset = (*nskip)*nb4*(*nall);
    offset += (*ixpsf + *iypsf*tmp ) * nb4;
    iypix2 = iypix ;
    if ((yfrac - iyfrac) >= 0.5) iypix2--;
    if (*deriv == 1) {
      o5 = (iypix2-1)*(*nbox)+(ixpix-2) ;
      o6 = o5 + 1;
    }

 
    xfrac = xfrac - ixfrac ;
    yfrac = yfrac - iyfrac ;




 
    for (iiy=0;iiy<2;iiy++) {
       for (iix=0;iix<2;iix++) {
 
 
          ltmp = *location + offset;
          ltmp2 = ltmp + o2;
          ltmp4 = ltmp + o4;
          val1 = *(ltmp+o1) - *ltmp2;
          val1 = val1*xfrac + *ltmp2;
          val2 = *(ltmp+o3) - *ltmp4; 
          val2 = val2*xfrac + *ltmp4;
          *(value+iii) = (val2 - val1)*yfrac + val1;
          if (*deriv == 1) {
            *(dvdy+iii) = val2 - val1 ;
            *(dvdx+iii) = *(ltmp+o5) - *(ltmp+o6);
          }
          iii++;
          offset += nb4;
       }
 
       offset += (tmp-2)*nb4 ;
     }
}


ccgetdata_(location,ix,iy,nx,ny,data)
int *ix,*iy,*nx,*ny;
float **location;
float *data;
{
   *data = *(*location+(*iy-1)*(*nx)+(*ix-1));
}

ccputdata_(location,ix,iy,nx,ny,data)
int *ix,*iy,*nx,*ny;
float **location;
float *data;
{
   *(*location+(*iy-1)*(*nx)+(*ix-1)) = *data;
}

cclibint_(location,location2,npsflib,nlib,psfrad,amin,amax)
int *location,*location2,*npsflib,*nlib,*psfrad,*amin,*amax;
{
  libint_(*location,*location2,npsflib,nlib,psfrad,amin,amax);
}
 
cclibput_(location,nbox,nlib,ix,iy,x,y,value,amin,amax)
int *location,*nbox,*nlib,*ix,*iy,*x,*y,*value,*amin,*amax;
{
  libput_(*location,nbox,nlib,ix,iy,x,y,value,amin,amax);
}

 
cceqwindex_ (b,a,nra,nca,bands,nb,zp1,dp)
int *b,*a,*nra,*nca,*bands,*nb,*zp1,*dp;
{
  eqwindex_ (*b,a,nra,nca,bands,nb,zp1,dp);
}

cceqwindexv_ (b,a,nra,nca,bands,nb,zp1,dp,v)
int *b,*a,*nra,*nca,*bands,*nb,*zp1,*dp,*v;
{
  eqwindexv_ (*b,a,nra,nca,bands,nb,zp1,dp,*v);
}

 
 
 
cccopio_ (location,nr2,nc2,spa,nr,nc,ii,jj)
int *location,*nr2,*nc2,*nc,*nr,*ii,*jj,*spa;
{
  copio_ (*location,nr2,nc2,spa,nr,nc,ii,jj);
}
 
cccopio2_ (location,nr2,nc2,spa,nr,nc,ii,jj)
int *location,*nr2,*nc2,*nc,*nr,*ii,*jj,*spa;
{
  copio2_ (*location,nr2,nc2,spa,nr,nc,ii,jj);
}
 
cc1copio_ (location,nr2,nc2,loc2,nr,nc,ii,jj)
int *location,*nr2,*nc2,*nc,*nr,*loc2,*ii,*jj;
{
  copio_ (*location,nr2,nc2,*loc2,nr,nc,ii,jj);
}
 
cc2copio_ (location,nr2,nc2,loc2,nr,nc,ii,jj)
int *location,*nr2,*nc2,*nc,*nr,*loc2,*ii,*jj;
{
  copio_ (location,nr2,nc2,*loc2,nr,nc,ii,jj);
}
 
ccbinit_ (a,isr,ier,isc,iec,bsr,bxc,loc,nr,nc,dr,dc,normal,sample)
int *a,*isr,*ier,*isc,*iec,*bsr,*bxc,*loc,*nr,*nc,*dr,*dc,*normal,*sample;
{
  binit_ (a,isr,ier,isc,iec,bsr,bxc,*loc,nr,nc,dr,dc,normal,sample);
}
 
ccrepit_ (a,isr,ier,isc,iec,bsr,bxc,loc,nr,nc,dr,dc,normal)
int *a,*isr,*ier,*isc,*iec,*bsr,*bxc,*loc,*nr,*nc,*dr,*dc,*normal;
{
  repit_ (a,isr,ier,isc,iec,bsr,bxc,*loc,nr,nc,dr,dc,normal);
}

ccdocross_ (a,asr,aer,asc,aec,b,bsr,ber,bsc,bec,loc,isr,ier,isc,iec,isrow,ierow,
iscol,iecol)
int *a,*asr,*aer,*asc,*aec,*b,*bsr,*ber,*bsc,*bec,*loc,*isr,*ier,*isc,*iec;
int *isrow,*ierow,*iscol,*iecol;
{
  docross_ (a,asr,aer,asc,aec,b,bsr,ber,bsc,bec,*loc,isr,ier,isc,iec,isrow,ierow,
	       iscol,iecol);
}
 
ccfindlimits_ (loc,isr,ier,isc,iec,pmin,pmax,haveblank,fblank)
int *loc,*isr,*ier,*isc,*iec,*pmin,*pmax,*haveblank,*fblank;
{
  findlimits_ (*loc,isr,ier,isc,iec,pmin,pmax,haveblank,fblank);
}
 
ccextractsky_ (a,isr,ier,isc,iec,sr,er,ns,skyord,loc)
int *a,*isr,*ier,*isc,*iec,*sr,*er,*ns,*skyord,*loc;
{
  extractsky_ (a,isr,ier,isc,iec,sr,er,ns,skyord,*loc);
}
 
ccoptimal_ (a,isr,ier,isc,iec,locsky,locpro,locv,locspec,locsvar,locmask,
   sr,er,pro)
int *a,*isr,*ier,*isc,*iec,*locsky,*locpro,*locv,*locspec,*locsvar,*locmask;
int *sr, *er, *pro;
{
  optimal_ (a,isr,ier,isc,iec,*locsky,*locpro,*locv,*locspec,*locsvar,*locmask,
   sr,er,pro);
}
 
ccsubsky_ (a,isr,ier,isc,iec,loc)
int *a, *isr,*ier,*isc,*iec,*loc;
{
  subsky_ (a,isr,ier,isc,iec,*loc);
}
 
ccflipinnrows_ (a,loc,isr,ier,isc,iec)
int *a,*loc,*isr,*ier,*isc,*iec;
{
  flipinnrows_ (a,*loc,isr,ier,isc,iec);
}
 
ccflipinncols_ (a,loc,isr,ier,isc,iec)
int *a,*loc,*isr,*ier,*isc,*iec;
{
  flipinncols_ (a,*loc,isr,ier,isc,iec);
}
 
ccfillfrommask_ (loc,sr,er,sc,ec,zero,fill)
int *loc,*sr,*er,*sc,*ec,*zero,*fill;
{
  fillfrommask_ (*loc,sr,er,sc,ec,zero,fill);
}

ccloadmask_(iunit,loc,nwords,fortran)
int *loc, *nwords, *iunit, *fortran;
{
  int nbytes;
  if (*fortran ==1)
    loadmask_(iunit,*loc,nwords);
  else {
    nbytes = 4 * *nwords;
    readint_(iunit,*loc,&nbytes);
  }
}
 
ccwritemask_(ifile,loc,nbytes)
int *loc, *nbytes, *ifile;
{
   writeint_(ifile,*loc,nbytes);
}
 
ccmeanofimage_ (loc,nr,nc)
int *loc, *nr, *nc;
{
  meanofimage_ (*loc,nr,nc);
}
 
ccdomerge_ (locm,locw,nr,nc,next,sr,er,sc,ec,nozero,i)
int *locm, *locw, *nr, *nc, *next, *sr, *er, *sc, *ec, *nozero, *i;
{
  domerge_ (*locm,*locw,nr,nc,*next,sr,er,sc,ec,nozero,i);
}

ccmedian_ (loc,npts,fmedia,perc)
int *loc, *npts, *fmedia, *perc;
{
  median_(*loc,npts,fmedia,perc);
}
 
cclibread_ (loc,nside,maxlib,npsf,ipsf)
int *loc, *nside,*maxlib,*npsf,*ipsf;
{
  libread_ (*loc,nside,maxlib,npsf,ipsf);
}
 
ccdpsfall_ (loc,nside,maxlib,npsf,col,row,ixpix,iypix,val,dvdx,dvdy)
int *loc,*nside,*maxlib,*npsf,*col,*row,*ixpix,*iypix,*val,*dvdx,*dvdy;
{
  dpsfall_ (*loc,nside,maxlib,npsf,col,row,ixpix,iypix,val,dvdx,dvdy);
}
 
ccloadvector_ (loc,isr,ier,isc,iec,yplot,nplot,ir1,ir2,ic1,ic2,rowmash,norm)
int *loc,*isr,*ier,*isc,*iec,*yplot,*nplot,*ir1,*ir2,*ic1,*ic2,*rowmash,*norm;
{
  loadvector_ (*loc,isr,ier,isc,iec,yplot,nplot,ir1,ir2,ic1,ic2,rowmash,norm);
}
 
ccprintimage_ (loc,isr,ier,isc,iec,ncmx,nsig,user,haveform,formstr,nc)
int *loc,*isr,*ier,*isc,*iec,*ncmx,*nsig,*user,*haveform,*formstr;
int nc;
{
  printimage_ (*loc,isr,ier,isc,iec,ncmx,nsig,user,haveform,formstr,nc);
}
 
ccrot90_ (a,loc,nr,nc)
int *a, *loc, *nr, *nc;
{
  rot90_ (a,*loc,nr,nc);
}
 
ccrot180_ (a,loc,nr,nc)
int *a, *loc, *nr, *nc;
{
  rot180_ (a,*loc,nr,nc);
}
 
ccrot270_ (a,loc,nr,nc)
int *a, *loc, *nr, *nc;
{
  rot270_ (a,*loc,nr,nc);
}
 
cctranspose_ (a,loc,nr,nc)
int *a, *loc, *nr, *nc;
{
  transpose_ (a,*loc,nr,nc);
}
 
ccrotateit_ (a,isr,ier,isc,iec,loc,jsr,jer,jsc,jec,pa,interp,row0,col0)
int *a,*isr,*ier,*isc,*iec,*loc,*jsr,*jer,*jsc,*jec,*pa,*interp,*row0,*col0;
{
  rotateit_ (a,isr,ier,isc,iec,*loc,jsr,jer,jsc,jec,pa,interp,row0,col0);
}
 
ccunloadfit1_ (a,isr,ier,isc,iec,lpf,isx,iex,isy,iey,ncoef,oper,loc,jsy,jey)
int *a,*isr,*ier,*isc,*iec,*lpf,*isx,*iex,*isy,*iey,*ncoef,*oper,*loc;
int *jsy,*jey;
{
  unloadfit1_ (a,isr,ier,isc,iec,*lpf,isx,iex,isy,iey,ncoef,oper,*loc,jsy,jey);
}
 
ccloadfit1_ (a,isr,ier,isc,iec,lpf,isx,iex,isy,iey,lim,nlim,ncoef,oper,load,
  y,loc,clip,fclip)
int *a,*isr,*ier,*isc,*iec,*lpf,*isx,*iex,*isy,*iey,*ncoef,*oper,*loc;
int *lim,*nlim,*load,*y,*clip,*fclip;
{
  loadfit1_ (a,isr,ier,isc,iec,*lpf,isx,iex,isy,iey,lim,nlim,ncoef,oper,load,
    y,*loc,clip,fclip);
}
 
ccloadfit2_(a,isr,ier,isc,iec,lpf,isx,iex,isy,iey,lim,nlim,ncoef,oper,
  load,y,loc,clip,fclip,w,isw,iew)
int *a,*isr,*ier,*isc,*iec,*lpf,*isx,*iex,*isy,*iey,*ncoef,*oper,*loc;
int *lim,*nlim,*load,*y,*clip,*fclip,*w,*isw,*iew;
{
  loadfit2_ (a,isr,ier,isc,iec,*lpf,isx,iex,isy,iey,lim,nlim,ncoef,oper,
      load,y,*loc,clip,fclip,*w,isw,iew);
}

ccpfunc_(lpf,x,ncoef,nx,i,j,ifunc)
int *lpf,*x,*ncoef,*nx,*i,*j,*ifunc;
{
  pfunc_ (*lpf,x,ncoef,nx,i,j,ifunc);
}
 
ccshiftitbymod_ (a,loc,nr,nc,rshift,cshift,rmodel,cmodel,mode)
int *a,*loc,*nr,*nc,*rshift,*cshift,*rmodel,*cmodel,*mode;
{
  shiftitbymod_ (a,*loc,nr,nc,rshift,cshift,rmodel,cmodel,mode);
}
 
ccshiftit_ (a,loc,nr,nc,dr,dc,mode)
int *a,*loc,*nr,*nc,*dr,*dc,*mode;
{
  shiftit_ (a,*loc,nr,nc,dr,dc,mode);
}
 
ccbshiftit_(a,loc,nr,nc,dr,dc,blanking,blank)
int *a,*loc,*nr,*nc,*dr,*dc,*blanking,*blank;
{
  bshiftit_(a,*loc,nr,nc,dr,dc,blanking,blank);
}
 
ccstretchit_ (sp,nspec,loc,nr,nc,vert)
int *sp,*nspec,*loc,*nr,*nc,*vert;
{
  stretchit_ (sp,nspec,*loc,nr,nc,vert);
}
 
ccfill_ (loc,nc,nr,a,isr,ier,isc,iec,sr,er,sc,ec)
int *loc,*nc,*nr,*a,*isr,*ier,*isc,*iec,*sr,*er,*sc,*ec;
{
  fill_ (*loc,nc,nr,a,isr,ier,isc,iec,sr,er,sc,ec);
}
 






 
 
ccgetthepixel_ (loc,nc,nr,col,row,pix)
int *loc,*nc,*nr,*col,*row,*pix;
{
  getthepixel_ (*loc,nc,nr,col,row,pix);
}
 
ccgetcol_ (loc,isr,ier,isc,iec,func,nx,lx,mx,irow,one)
int *loc,*isr,*ier,*isc,*iec,*func,*nx,*lx,*mx,*irow,*one;
{
  getcol_ (*loc,isr,ier,isc,iec,func,nx,lx,mx,irow,one);
}
 
cczapthatsucker_ (a,isr,ier,isc,iec,loc,sr,er,sc,ec,rsize,csize,sig,perc,tty,noisemod,gain,rn)
int *a,*isr,*ier,*isc,*iec,*loc,*sr,*er,*sc,*ec,*rsize,*csize,*sig,*perc,*tty,*noisemod,*gain,*rn;
{
  zapthatsucker_ (a,isr,ier,isc,iec,*loc,sr,er,sc,ec,rsize,csize,sig,perc,tty,noisemod,gain,rn);
}
 
ccsaturate_ (loc,isc,iec,isr,ier,col,row,satur,nbox,sat,maskrad)
int *loc,*isc,*iec,*isr,*ier,*col,*row,*satur,*nbox,*sat,*maskrad;
{
  saturate_ (*loc,isc,iec,isr,ier,col,row,satur,nbox,sat,maskrad);
}
 
ccround_ (a,loc,nsr,ner,nsc,nec,ellip,pa,xc,yc,invert)
int *a,*loc,*nsr,*ner,*nsc,*nec,*ellip,*pa,*xc,*yc,*invert;
{
  round_ (a,*loc,nsr,ner,nsc,nec,ellip,pa,xc,yc,invert);
}
 
ccresam_ (b,a,isr,ier,isc,iec,nbins,bin)
int *b,*a,*isr,*ier,*isc,*iec,*nbins,*bin;
{
  resam_ (*b,a,isr,ier,isc,iec,nbins,bin);
}
 
ccresamlc_ (b,a,isr,ier,isc,iec,nbins,bin,mod,isrm,ierm,iscm,iecm)
int *b,*a,*isr,*ier,*isc,*iec,*nbins,*bin,*mod,*isrm,*ierm,*iscm,*iecm;
{
  resamlc_ (*b,a,isr,ier,isc,iec,nbins,bin,*mod,isrm,ierm,iscm,iecm);
}

ccloadhistbuf_ (loc,x,y,n)
int *loc, *x, *y, *n;
{
  loadhistbuf_ (*loc,x,y,n);
}

ccdobigmed_(a,isr,ier,isc,iec,ifile,havebias,mean,bitpix,bzero,bscale,nfiles,sig,wfpc,havelowbad,lowbad,havehighbad,highbad)
int *a,*isr,*ier,*isc,*iec,*ifile,*havebias,*mean,*bitpix,*bzero,*bscale;
int *nfiles,*sig,*wfpc;
int *havelowbad,*lowbad,*havehighbad,*highbad;
{
  dobigmed_ (*a,isr,ier,isc,iec,ifile,havebias,mean,bitpix,bzero,bscale,nfiles,sig,wfpc,havelowbad,lowbad,havehighbad,highbad);
}

ccdopiccrs_(locim,loc1,loc2,imsr,imer,imsc,imec,ifile,ofile,havebias,mean,
bitpix,bzero,bscale,nfiles,fits,loca,locb,locc,locd,flag,neigh,bad,pflag,nts,
lines,n1,n2,n3)
int *locim,*loc1,*loc2,*imsr,*imer,*imsc,*imec,*ifile,*ofile,*havebias,*mean;
int *bitpix, *bzero, *bscale, *nfiles, *fits;
int *loca,*locb,*locc,*locd,*n1,*n2,*n3;
int *flag,*neigh,*bad,*pflag,*nts,*lines;
{
  dopiccrs_(*locim,*loc1,*loc2,imsr,imer,imsc,imec,ifile,ofile,
havebias,mean, bitpix,bzero,bscale,nfiles,fits,
*loca,*locb,*locc,*locd,*flag,*neigh,*bad,*pflag,*nts,*lines,n1,n2,n3);
}

ccwfpc2un_(locim,isr,ier,isc,iec,c,d,loc1,loc2,loc3,loc4)
int *locim, *isr, *ier, *isc, *iec, *c, *d, *loc1, *loc2, *loc3, *loc4;
{
  wfpc2un_(*locim,isr,ier,isc,iec,c,d,*loc1,*loc2,*loc3,*loc4);
}

ccconvolveit_(locnew,jsr,jer,jsc,jec,locim,isr,ier,isc,iec,a,nrow,ncol)
int *locnew,*jsr,*jer,*jsc,*jec,*locim,*isr,*ier,*isc,*iec,*a,*nrow,*ncol;
{
convolveit_(*locnew,jsr,jer,jsc,jec,*locim,isr,ier,isc,iec,a,nrow,ncol);
}

ccsumimage_ (location,isr,ier,isc,iec,sr,er,sc,ec,sum,maxpix)
int *location,*isr,*ier,*isc,*iec,*sr,*er,*sc,*ec,*sum,*maxpix;
{
  sumimage_(*location,isr,ier,isc,iec,sr,er,sc,ec,sum,maxpix);
}

ccmpgcont_ (location,nc,nr,psc,pec,psr,pper,alev,is,tr)
int *location,*nc,*nr,*psc,*pec,*psr,*pper,*alev,*is,*tr;
{
  mpgcont_ (*location,nc,nr,psc,pec,psr,pper,alev,is,tr);
}

ccmpgcons_ (location,nc,nr,psc,pec,psr,pper,alev,is,tr)
int *location,*nc,*nr,*psc,*pec,*psr,*pper,*alev,*is,*tr;
{
  mpgcons_ (*location,nc,nr,psc,pec,psr,pper,alev,is,tr);
}

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
ccmathimagecon_ (location,isr,ier,isc,iec,loc2,jsr,jer,jsc,jec,com,constant,nc)
int *location,*isr,*ier,*isc,*iec,*loc2,*jsr,*jer,*jsc,*jec,*com;
int nc;
float *constant;
{
  mathimagecon_ (*location,isr,ier,isc,iec,*loc2,jsr,jer,jsc,jec,
		  com,constant,nc);
}
 
ccmathwithimag_ (location,isr,ier,isc,iec,loc2,jsr,jer,jsc,jec,com,nc)
int *location,*isr,*ier,*isc,*iec,*loc2,*jsr,*jer,*jsc,*jec,*com;
int nc;
{
  mathwithimag_ (*location,isr,ier,isc,iec,*loc2,jsr,jer,jsc,jec,
		  com,nc);
}
 
ccmathwithcon_ (location,isr,ier,isc,iec,com,constant,nc)
int *location,*isr,*ier,*isc,*iec,*com;
float *constant;
int nc;
{
  mathwithcon_ (*location,isr,ier,isc,iec,com,constant,nc);
}
 
 
ccrdtape_ (nunit,imtape,zero,loc,nr,nc,headnew,xerr,nchar)
int *nunit,*imtape,*zero,*loc,*nr,*nc,*headnew,*xerr;
int nchar;
{
  rdtape_ (nunit,imtape,zero,*loc,nr,nc,headnew,xerr,nchar);
}








 

ccsubstarv_ (locim,isr,ier,isc,iec)
int *locim,*isr,*ier,*isc,*iec;
{
  substarv_ (*locim,isr,ier,isc,iec);
}

ccsubphot_ (locb,jsr,jer,jsc,jec,locim,isr,ier,isc,iec)
int *locim,*isr,*ier,*isc,*iec;
int *locb,*jsr,*jer,*jsc,*jec;
{
  subphot_ (*locb,jsr,jer,jsc,jec,*locim,isr,ier,isc,iec);
}

ccpkfit_(f,nbox,nbox2,maxbox,scale,x,y,sky,radius,lowbad,highbad,gauss,psf,npsf,errmag,chi,sharp,niter,location,npsflib,nlib)
int *f,*nbox,*nbox2,*maxbox,*scale,*x,*y,*sky,*radius,*lowbad,*highbad;
int *gauss,*psf,*npsf,*errmag,*chi,*sharp,*niter,*location;
int *npsflib,*nlib;
{
  pkfit_(f,nbox,nbox2,maxbox,scale,x,y,sky,radius,lowbad,highbad,gauss,psf,npsf,errmag,chi,sharp,niter,*location,npsflib,nlib);
 }


ccunfit_ (inchar,loc,nchar)
int *inchar, *loc;
int nchar;
{
  unfit_ (inchar,*loc,nchar,*(loc+1));
}
 
ccheader_ (inchar,loc,index,nchar)
int *inchar,*loc,*index;
int nchar;
{
  header_ (inchar,*loc,index,nchar,*(loc+1));
}
 
ccprfits_ (loc,inchar,full,nchar)
int *loc,*inchar,*full;
int nchar;
{
  prfits_ (*loc,inchar,full,*(loc+1),nchar);
}
 
ccsetpoly_ (loc,a,b,c,d)
int *loc,*a,*b,*c,*d;
{
  setpoly_ (*loc,a,b,c,d,*(loc+1));
}
 
ccesetpoly_ (loc,a,b,c,d,e)
int *loc,*a,*b,*c,*d,*e;
{
  esetpoly_ (*loc,a,b,c,d,e,*(loc+1));
}
 
ccegetpoly_ (loc,a,b,c,d)
int *loc,*a,*b,*c,*d;
{
  egetpoly_ (*loc,a,b,c,d,*(loc+1));
}

ccaddhistory_ (string,l,ls)
char *string;
int *l;
int ls;
{
  *(string+*l) = 0;
  add_history(string);
  *(string+*l) = ' ';
}
 
 
 
 
 
 
 
 



