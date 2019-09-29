/*
 *                                      compression_generic.c
 *                                      September 9, 1993
 */

#define 	HITEST		0
#define 	HISAMPLE	0
#define 	HICT		0

#include <stdio.h>

#ifndef BASE_TYPE
#define BASE_TYPE DATA_TYPE
#endif
#if BASE_TYPE <= 2
#define MAX_SIZE_OUT 256
#else
#if BASE_TYPE == 4
#define MAX_SIZE_OUT 257
#endif
#endif
#if HITEST
static FILE *fp_test = NULL;
static int ct_test = 0;;
#define SAMPLECT 10000
#endif
#if HICT
int ct_0=0, ct_1=0, ct_2=0, ct_4=0, ct_8=0, ct_16=0, ct_32=0;
#endif
#include "im_compress.h"

extern int   compression_0bit_ok;
extern int   compression_to_end;

#if DATA_TYPE <= BASE_TYPE
#define CKMM
#define CKMMT
#else
#if ((BASE_TYPE == 2) && (DATA_TYPE == 4))
#define CKMM if( min < 0 || max > 32767 ) d = 900000;
#define CKMMT if( t_min < 0 || t_max > 32767 ) goto ENDXBIT;
#else
#endif
#endif
#define MM x = *p++; if (x<min) min = x; if(x>max) max=x;
#define ST min = max = *p++;
#define ENDD d = max - min;
#define MM4 ST MM MM MM ENDD
#define MM8 ST MM MM MM MM MM MM MM ENDD
#define MM16 ST MM MM MM MM MM MM MM MM MM MM MM MM MM MM MM ENDD
#define MM4M MM MM MM MM ENDD
#define MM8M MM MM MM MM MM MM MM MM ENDD
#define MM16M MM MM MM MM MM MM MM MM MM MM MM MM MM MM MM MM ENDD
#define MMST_T t_min = t_max = *p++;
#define MMT x = *p++; if(x<t_min) t_min = x; if(x>t_max) t_max=x;
#define MM4MT MMST_T MMT MMT MMT
#define MM4MTP MMT MMT MMT MMT
#define MM8MT MMST_T MMT MMT MMT MMT MMT MMT MMT
#define MM8MTP MMT MMT MMT MMT MMT MMT MMT MMT
#define MM16MT MM8MT MM8MTP 
#define MM16MTP MM8MTP MM8MTP 
#define MM32MT MM16MT MM16MTP
#define MM32MTP MM16MTP MM16MTP
#define MM64MT MM32MT MM32MTP
#define MM64MTP MM32MTP MM32MTP
#define MM128MT MM64MT MM64MTP
#define CK_COMB q_min=(min<t_min)?min:t_min; q_max=(max>t_max)?max:t_max; \
	d = q_max - q_min;
#define END *((base_data_t *)p_o_st) = min; *(p_o_st+sizeof(base_data_t)) = len; 
#if BASE_TYPE <= 2
#define INIT( X ) p_o_st = (unsigned char *)p_o; \
	*(p_o_st+sizeof(base_data_t)+1)= X; p_o++;
#else
#if BASE_TYPE == 4
#define INIT( X ) p_o_st = (unsigned char *)p_o; \
	*(p_o_st+sizeof(base_data_t)+1)= X; p_o++; p_o++;
#endif
#endif
#define SMM s_min = min; s_max = max; s_p = p;
#define RMM min = s_min; max = s_max; p = s_p;

/*  returns:
 *     0  = Completely compressed to the end and no error occurred
 *     -2 = "compress_to_end" is set to off and not enough input data (32*256)
 *           to complete a segment if 1 bit per number is to be used
 *     -3 = not enough space to create an other complete output segment 
 *                      and compress_to_end turned off
 *     -4 = not enough space to create an other complete output segment
 *                      and compress_to_end turned on
 *     other value = error occurred
 */
typedef unsigned int data_t;
#if DATA_TYPE == 1
 typedef char in_data_t;
#if BASE_TYPE == 1
  typedef char base_data_t;
  int
  compression_char( len_in, p_in, p_len_in_used, len_out_in, p_out, p_len_out_used)
#else
#if BASE_TYPE == 2
   typedef short base_data_t;
   int
   compression_char_short_base( len_in, p_in, p_len_in_used, len_out_in, p_out, p_len_out_used)
#endif
#endif
#endif

#if (DATA_TYPE == 2 && BASE_TYPE == 2 )
 typedef short in_data_t;
 typedef short base_data_t;
 int
 compression_short( len_in, p_in, p_len_in_used, len_out_in, p_out, p_len_out_used)
#endif

#if DATA_TYPE == 4
 typedef int in_data_t;
#if BASE_TYPE == 2
  typedef unsigned short base_data_t;
  int
  compression_int_unshort_base( len_in, p_in, p_len_in_used, len_out_in, p_out, p_len_out_used)
#else
#if BASE_TYPE == 4
  typedef int base_data_t;
  int
  compression_int( len_in, p_in, p_len_in_used, len_out_in, p_out, p_len_out_used)
#endif
#endif
#endif
int len_in; /* length in in_data_t's in p_in */
in_data_t *p_in;
int *p_len_in_used; /* number of in_data_t's used */
int len_out_in; /* length in chars in p_out */
data_t *p_out;
int *p_len_out_used; /* number of chars used */ 
{
   int ret = 0, ct, ck, left_in, left_out, len_out;
   int ct2, len, low_sw, return_sw;
   register long t_min, t_max, x, min, max;
   long q_min, q_max, s_min, s_max, d, d2;
   register in_data_t *p;
   register unsigned long v;
   data_t *p_o;
   unsigned char *p_o_st;
   in_data_t *p_st, *s_p;

#if HITEST
   if( fp_test == NULL ) {
      fp_test = fopen( "comp.out", "w" );
      if( fp_test == NULL ) {
         ret = -67345675;
         fprintf( stderr, "Could not open comp.out - %d\n", ret );
         goto retu;
      }
   }
#endif
   p = p_in; p_o = p_out; len_out = len_out_in / 4;
start:
   ck=1; left_in=len_in-(p-p_in); left_out=len_out-(p_o-p_out);
#if HISAMPLE
   if( ct_test++ < SAMPLECT ) {
      fprintf( fp_test, "           start left_in=%d\n", left_in );
      fflush( fp_test );
   }
#endif
   if( compression_to_end ) {
      if( left_in <= 0 ) { goto retu; }
   } else {
      if( left_in < 32 * 256 ) { ret = -2; goto retu; }
   }
   if( left_out < MAX_SIZE_OUT ) {
      if( compression_to_end ) {
         ret = -4; goto retu;
      } else {
         ret = -3; goto retu;
      }
   }
   p_st = p;
   if( left_in < 4) { 
#if DATA_TYPE == 1
      if( compression_0bit_ok) { ct = 0; min = *p; goto do0bit; }
      ct = left_in; goto do8bit; 
#endif
#if DATA_TYPE == 2
      if( left_in == 1 && compression_0bit_ok) { ct = 0; min = *p; goto do0bit; }
      ct = left_in; goto do16bit; 
#endif
#if DATA_TYPE == 4
      if( left_in == 1 && compression_0bit_ok) { ct = 0; min = *p; goto do0bit; }
      ct = left_in; goto do32bit; 
#endif
   }
   MM4 CKMM
#if HISAMPLE
   if( ct_test < SAMPLECT ) {
      fprintf( fp_test, " 1 d=%d\n", d );
      fflush( fp_test );
   }
#endif
#if DATA_TYPE == 4
   if( d > 256 * 256 - 1 ) {
      	 ct = 4; goto do32bit;
   }
#endif
   if( d > 255 ) {
#if DATA_TYPE == 1
      ret = -68764; goto retu;
#endif
#if DATA_TYPE == 2
      ct = 2; p = p_st + ct; goto do16bit;
#endif
#if DATA_TYPE == 4
      ct = 4; goto do16bit;
#endif
   } else {
      if( left_in < 8) { ct = 4; goto do8bit; }
      SMM MM4M CKMM
#if HISAMPLE
      if( ct_test < SAMPLECT ) {
         fprintf( fp_test, " 2 d=%d\n", d );
         fflush( fp_test );
      }
#endif
      if( d > 255 ) {
#if DATA_TYPE == 1
         ret = -58764; goto retu;
#else
         RMM ct = 4; goto do8bit;
#endif
      } else {
         if( d < 16 ) {
            if( left_in < 16) { ct = 8; goto do4bit; }
            SMM MM8M CKMM
#if HISAMPLE
            if( ct_test < SAMPLECT ) {
               fprintf( fp_test, " 3 d=%d\n", d );
               fflush( fp_test );
            }
#endif
            if( d < 16 ) {
               if( d < 4) {
                  if( left_in < 32) { ct = 16; goto do2bit; }
#if HISAMPLE
                  if( ct_test < SAMPLECT ) {
                     fprintf( fp_test, " 4 d=%d\n", d );
                     fflush( fp_test );
                  }
#endif
                  SMM MM16M CKMM
                  if( d < 2 ) {
                     ct = 32;
                     if( d == 0 ) {
                       if(compression_0bit_ok) {
                          goto do0bit;
                       } else {
                          goto do1bit;
                       }
                     } else {
                       goto do1bit;
                     }
                  } else {
                     if( d < 4 ) {
                        ct = 32; goto do2bit;
                     } else {
                        RMM ct = 16; goto do2bit;
                     }
                  }
               } else {
                  ct = 16; goto do4bit;
               }
            } else {
               if( d < 256 ) {
                  ct = 16; goto do8bit;
               } else {
                  RMM ct = 8; goto do8bit;
               }
            }
         } else {
            ct = 8; goto do8bit;
         }
      }
   }

#if DATA_TYPE > 2
#define OUT16BITST v = *(++p) - min;
#define OUT16BIT v <<= 16; v |= *(--p) - min; 
#define OUT16BIT2 OUT16BITST OUT16BIT *p_o++ = v; p += 2;
#define DOXBIT do16bit
#define CTX ct_16++;
#define SX S16 
#define SHORT_PER_INT 2
#define OUTX OUT16BIT2
#define CONTXBIT cont16bit
#if BASE_TYPE <= 2
#define MMST_X MMST_T MMT
#define MMREST_X MMT MMT MMT MMT MMT MMT
#define LOOK_FACTOR 4
#else
#if BASE_TYPE == 4
#define MMST_X MMST_T MMT 
#define MMREST_X MM8TP MM4TP MMT MMT
#define LOOK_FACTOR 8
#endif
#endif
#define LOWX_D 256
#define LOWLOWX_D 16
#define DOLOWERBITS do8bit
#define HIGHX_D 256 * 256
#define ENDXBIT end16bit
#include "compression_sh.h"
#endif

#define OUTCST p += 3; v = 0xff & (*p - min);
#define OUTC v <<= 8; v |= (*--p - min);
#define OUTC4 OUTCST OUTC OUTC OUTC *p_o++ = v; p += 4;
#if DATA_TYPE > 1
#define DOXBIT do8bit
#define CTX ct_8++;
#define SX S8 
#define SHORT_PER_INT 4
#define OUTX OUTC4
#define CONTXBIT cont8bit
#if BASE_TYPE <= 2
#define MMST_X MM4MT
#define MMREST_X MM4MTP MM4MTP MM4MTP
#define LOOK_FACTOR 4
#else
#if BASE_TYPE == 4
#define MMST_X MM4MT
#define MMREST_X MM4MTP MM4MTP MM4MTP MM4MTP MM4MTP MM4MTP MM4MTP
#define LOOK_FACTOR 8
#endif
#endif
#define LOWX_D 16
#define LOWLOWX_D 4
#define DOLOWERBITS do4bit
#define HIGHX_D 256
#define ENDXBIT end8bit
#include "compression_sh.h"
#endif

#define OUT4BITST p += 7; v = (*p - min);
#define OUT4BIT v <<= 4; v |= (*--p - min);
#define OUT4BIT8 OUT4BITST OUT4BIT OUT4BIT OUT4BIT \
		OUT4BIT OUT4BIT OUT4BIT OUT4BIT *p_o++ = v; p += 8;
#define DOXBIT do4bit
#define CTX ct_4++;
#define SX S4 
#define SHORT_PER_INT 8
#define OUTX OUT4BIT8
#define CONTXBIT cont4bit
#if BASE_TYPE <= 2
#define MMST_X MM8MT
#define MMREST_X MM8MTP MM8MTP MM8MTP
#define LOOK_FACTOR 4
#else
#if BASE_TYPE == 4
#define MMST_X MM8MT
#define MMREST_X MM8MTP MM8MTP MM8MTP MM8MTP MM8MTP MM8MTP MM8MTP
#define LOOK_FACTOR 8
#endif
#endif
#define LOWX_D 4
#define LOWLOWX_D 2
#define DOLOWERBITS do2bit
#define HIGHX_D 16
#define ENDXBIT end4bit
#include "compression_sh.h"

#define OUT2BITST p += 15; v = (*p - min);
#define OUT2BIT v <<= 2; v |= (*--p - min);
#define OUT2BIT16 OUT2BITST OUT2BIT OUT2BIT OUT2BIT \
		OUT2BIT OUT2BIT OUT2BIT OUT2BIT \
		OUT2BIT OUT2BIT OUT2BIT OUT2BIT \
		OUT2BIT OUT2BIT OUT2BIT OUT2BIT *p_o++ = v; p += 16;
#define DOXBIT do2bit
#define CTX ct_2++;
#define SX S2 
#define SHORT_PER_INT 16
#define OUTX OUT2BIT16
#define CONTXBIT cont2bit
#if BASE_TYPE <= 2
#define MMST_X MM16MT
#define MMREST_X MM16MTP MM16MTP MM16MTP
#define LOOK_FACTOR 4
#else
#if BASE_TYPE == 4
#define MMST_X MM16MT
#define MMREST_X MM16MTP MM16MTP MM16MTP MM16MTP MM16MTP MM16MTP MM16MTP
#define LOOK_FACTOR 8
#endif
#endif
#define LOWX_D 2
#define LOWLOWX_D 1
#define DOLOWERBITS do1bit
#define HIGHX_D 4
#define ENDXBIT end2bit
#include "compression_sh.h"

#define OUT1BITST p += 31; v = (*p - min);
#define OUT1BIT v <<= 1; v |= (*--p - min);
#define OUT1BIT32 OUT1BITST OUT1BIT OUT1BIT OUT1BIT \
		OUT1BIT OUT1BIT OUT1BIT OUT1BIT \
		OUT1BIT OUT1BIT OUT1BIT OUT1BIT \
		OUT1BIT OUT1BIT OUT1BIT OUT1BIT \
		OUT1BIT OUT1BIT OUT1BIT OUT1BIT \
		OUT1BIT OUT1BIT OUT1BIT OUT1BIT \
		OUT1BIT OUT1BIT OUT1BIT OUT1BIT \
		OUT1BIT OUT1BIT OUT1BIT OUT1BIT *p_o++ = v; p += 32;
#define DOXBIT do1bit
#define CTX ct_1++;
#define SX S1 
#define SHORT_PER_INT 32
#define OUTX OUT1BIT32
#define CONTXBIT cont1bit
#if BASE_TYPE <= 2
#define MMST_X MM32MT
#define MMREST_X MM32MTP MM32MTP MM32MTP
#define LOOK_FACTOR 4
#else
#if BASE_TYPE == 4
#define MMST_X MM32MT
#define MMREST_X MM32MTP MM32MTP MM32MTP MM32MTP MM32MTP MM32MTP MM32MTP
#define LOOK_FACTOR 8
#endif
#endif
#define LOWX_D 1
#define LOWLOWX_D 0
#define DOLOWERBITS do0bit
#define HIGHX_D 2
#define ENDXBIT end1bit
#include "compression_sh.h"

do0bit:
#if HICT
   ct_0++;
#endif
   if( compression_0bit_ok == 0) {
      ck = 0; goto cont1bit;
   } else {
      INIT(S0) len = ct; left_in = len_in - ( p - p_in);
#if HISAMPLE
      if( ct_test < SAMPLECT ) {
         fprintf( fp_test, " 0 len=%d, left_in=%d\n", len, left_in );
         fflush( fp_test );
      }
#endif
      while( *p == min ) { 
         if( len > 254 ) break;
         len++; left_in--; p++;
         if(left_in < 1) break;
      }
#if HISAMPLE
      if( ct_test++ < SAMPLECT ) {
         fprintf( fp_test, "   0 len=%d, left_in=%d\n", len, left_in );
         fflush( fp_test );
      }
#endif
      *((in_data_t *)p_o_st) = min; *(p_o_st+2) = len; goto start;
   }

#if DATA_TYPE == 1
do8bit:
#if HICT
   ct_8++;
#endif
   INIT(S8) low_sw=0; return_sw=0; len=ct/4; left_in=len_in-(p-p_in);
cont8bit:
#if HISAMPLE
   if( ct_test < SAMPLECT ) {
       fprintf( fp_test, " 8 len=%d, left_in=%d\n", len, left_in );
       fflush( fp_test );
   }
#endif
   if( len > 249 ) { goto end8bit; }
   if( left_in < 8 ) {
      ct = left_in / 4; len += ct; left_in -= 4 * ct; goto end8bit;
   }
   s_p = p;
   MM4MT
   d = t_max - t_min;
   if( d < 16 ) { MM4MTP MM4MTP MM4MTP d = t_max - t_min; }
   if( d < 16 ) {
      ct2 = 16; low_sw = 1; goto end8bit;
   } else {
      p = s_p + 4; len++; left_in -= 4;
      if( left_in <= 0 ) {
         return_sw = 1; goto end8bit;
      } else {
         goto cont8bit;
      }
   }
end8bit:
#if HISAMPLE
   if( ct_test < SAMPLECT ) {
       fprintf( fp_test, 
          "   8 end len=%d, left_in=%d, low_sw=%d\n", 
		len, left_in, low_sw );
       fflush( fp_test );
   }
#endif
   min = 0; ct = len; p = p_st; while( ct-- ) { OUTC4 }
   if( left_in < 4 && compression_0bit_ok == 0 && compression_to_end ) {
      return_sw = 1;
      p += left_in - 1; v = 0xff & *p;
      left_in--;
      while( left_in-- > 0 ) {
         v <<= 8; v |= *--p;
      }
      len++; *p_o++ = v;
   }
   END
   if( low_sw ) {
      if( d < 4 ) {
         goto start;
      } else {
         left_in=len_in-(p-p_in); left_out=len_out-(p_o-p_out);
         if( left_out < 256 ) { goto start; };
         if(compression_to_end == 0 && left_in < 32 * 256) {
            ret = -2; goto retu;
         }
         ck=1; ct=ct2; p_st=p; p += ct2; min=t_min; max=t_max; goto do4bit;
      }
   } else {
      if( return_sw ) {
         goto retu;
      } else {
         goto start;
      }
   }
retu:
   *p_len_in_used = p - p_in;
   *p_len_out_used = 4 *( p_o - p_out);
   return( ret );
}
#endif

#if DATA_TYPE == 2
#define OUT16BITST v = *(unsigned short *)(++p);
#define OUT16BIT v <<= 16; v |= *(unsigned short *)(--p); 
#define OUT16BIT2 OUT16BITST OUT16BIT *p_o++ = v; p += 2;
do16bit:
#if HICT
   ct_16++;
#endif
   INIT(S16) low_sw=0; return_sw=0; len=ct/2; left_in=len_in-(p-p_in);
cont16bit:
#if HISAMPLE
   if( ct_test < SAMPLECT ) {
       fprintf( fp_test, " 16 len=%d, left_in=%d\n", len, left_in );
       fflush( fp_test );
   }
#endif
   if( len > 249 ) { goto end16bit; }
   if( left_in < 8 ) {
      ct = left_in / 2; len += ct; left_in -= 2 * ct; goto end16bit;
   }
   s_p = p;
   MMST_T MMT
   d = t_max - t_min;
   if( d < 256 ) { MMT MMT MMT MMT MMT MMT d = t_max - t_min; }
   if( d < 256 ) {
      ct2 = 8; low_sw = 1; goto end16bit;
   } else {
      p = s_p + 2; len++; left_in -= 2;
      if( left_in <= 0 ) {
         return_sw = 1; goto end16bit;
      } else {
         goto cont16bit;
      }
   }
end16bit:
#if HISAMPLE
   if( ct_test < SAMPLECT ) {
       fprintf( fp_test, 
          "   16 end len=%d, left_in=%d, low_sw=%d\n", 
		len, left_in, low_sw );
       fflush( fp_test );
   }
#endif
   min = 0; ct = len; p = p_st; while( ct-- ) { OUT16BIT2 }
   if( left_in == 1 && compression_0bit_ok == 0 ) {
      len++; v = *p++; *p_o++ = v; return_sw = 1;
   }
   END
   if( low_sw ) {
      if( d < 16 ) {
         goto start;
      } else {
         left_in=len_in-(p-p_in); left_out=len_out-(p_o-p_out);
         if( left_out < 256 ) { goto start; };
         if(compression_to_end == 0 && left_in < 32 * 256) {
            ret = -2; goto retu;
         }
         ck=1; ct=ct2; p_st=p; p += ct2; min=t_min; max=t_max; goto do8bit;
      }
   } else {
      if( return_sw ) {
         goto retu;
      } else {
         goto start;
      }
   }
retu:
   *p_len_in_used = p - p_in;
   *p_len_out_used = 4 *( p_o - p_out);
   return( ret );
}
#endif


#if DATA_TYPE == 4
#define OUT32 *p_o++ = *p++;

do32bit:
#if HICT
   ct_32++;
#endif
   INIT(S32) low_sw=0; return_sw=0; len=ct; left_in=len_in-(p-p_in);
cont32bit:
#if HISAMPLE
   if( ct_test < SAMPLECT ) {
       fprintf( fp_test, " 32 len=%d, left_in=%d\n", len, left_in );
       fflush( fp_test );
   }
#endif
   if( len > 249 ) { goto end32bit; }
   if( left_in < 4 ) {
      ct = left_in; len += ct; left_in -= ct; goto end32bit;
   }
   s_p = p;
   MMST_T MMT d = t_max - t_min;
#if BASE_TYPE == 4
   if( d < 256*256 ) { MMT MMT d = t_max - t_min; }
   if( d < 256*256 ) {
#else
#if BASE_TYPE == 2
   if( d < 256*256 && t_min >= 0 && t_max < 32678 ) { MMT MMT d = t_max - t_min; }
   if( d < 256*256 && t_min >= 0 && t_max < 32678 ) { 
#endif
#endif
      ct2 = 4; low_sw = 1; goto end32bit;
   } else {
      p = s_p + 2; len += 2; left_in -= 2;
      if( left_in <= 0 ) {
         return_sw = 1; goto end32bit;
      } else {
         goto cont32bit;
      }
   }
end32bit:
#if HISAMPLE
   if( ct_test < SAMPLECT ) {
       fprintf( fp_test, 
          "   32 end len=%d, left_in=%d, low_sw=%d\n", 
		len, left_in, low_sw );
       fflush( fp_test );
   }
#endif
   min = 0; ct = len; p = p_st; while( ct-- ) { OUT32 }
   END
   if( low_sw ) {
      if( d < 256 ) {
         goto start;
      } else {
         left_in=len_in-(p-p_in); left_out=len_out-(p_o-p_out);
         if( left_out < 256 ) { goto start; };
         if(compression_to_end == 0 && left_in < 32 * 256) {
            ret = -2; goto retu;
         }
         ck=1; ct=ct2; p_st=p; p += ct2; min=t_min; max=t_max; goto do16bit;
      }
   } else {
      if( return_sw ) {
         goto retu;
      } else {
         goto start;
      }
   }
retu:
   *p_len_in_used = p - p_in;
   *p_len_out_used = 4 *( p_o - p_out);
   return( ret );
}
#endif

