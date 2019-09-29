/*
 *						expansion_generic.c
 *						September 8, 1993
 */

#define		HISIZE		0
#define		SIZE_PRINT_MAX	50
#define		DISP_SIZE	32000
#define		HIRETURN	0
#define		HILOOPCT	0
#define		HILIMIT		20000	

#include "im_compress.h"

extern int exp_hibytehi_machine;
extern int exp_hibytehi_in;
extern int exp_expand_to_end;       /* expand output to the end of the output buffer
				     * even though only part of an input segment
				     * will be processed.
				     * You would want to set this to a value of 0
				     * if you planned to fill the output by
				     * whole segments.  This allows the program to be
				     * used in a sequence of calls where each call could
				     * have access to only part of the input and/or have 
				     * room for only part of the output.
				     */


/*
 *    returns 0, 1, 2, 3, 4 normally, 0 hit the end of input or output at end
 *                                          of a segment exactly
 *				      1 indicates output was filled completely 
 *                                       when expand_to_end flag was set.
 *                                    2 indicates last input segment not complete.
 *					 ( leaves stops at beginning of segment )
 *				      3 indicates end marker hit.
 *				      4 only room for part of segment 
 *					  but expand_to_end flag not set.
 *    Any other return codes are not normal.  An error must have occurred.
 */
#ifndef BASE_TYPE
#define BASE_TYPE DATA_TYPE
#endif

#if BASE_TYPE == 1
typedef char base_data_t;
#define HEADER_SIZE 4
#else
#if BASE_TYPE == 2
#if DATA_TYPE == 4
typedef unsigned short base_data_t;
#else
typedef short base_data_t;
#endif
#define HEADER_SIZE 4
#else
#if BASE_TYPE == 4
typedef int base_data_t;
#define HEADER_SIZE 8
#endif
#endif
#endif

#if DATA_TYPE == 1
 typedef char data_t;
 typedef unsigned char unsigned_data_t;
#if BASE_TYPE == 1
  int	
  expansion_char(len_in,image_in,len_used_in,max_len_out,image_out,len_used_out)
#else
#if BASE_TYPE == 2
   int	
   expansion_char_short_base(len_in,image_in,len_used_in,max_len_out,image_out,len_used_out)
#endif
#endif
#endif

#if (DATA_TYPE == 2 && BASE_TYPE == 2)
 typedef short data_t;
 typedef unsigned short unsigned_data_t;
 int	
 expansion_short(len_in,image_in,len_used_in,max_len_out,image_out,len_used_out)
#endif

#if DATA_TYPE == 4
 typedef int data_t;
 typedef unsigned int unsigned_data_t;
#if BASE_TYPE == 4
  int	
  expansion_int(len_in,image_in,len_used_in,max_len_out,image_out,len_used_out)
#else
#if BASE_TYPE == 2
   int	
   expansion_int_unshort_base(len_in,image_in,len_used_in,max_len_out,image_out,len_used_out)
#endif
#endif
#endif
int len_in;	/* length of image_in in bytes */
unsigned char *image_in;
int *len_used_in;  /* length of image_in used and converted to image_out */
int max_len_out;   /* maximum length of image_out in data_t */
data_t *image_out;
int *len_used_out; /* length of image_out used */
{
	int ret = 0;
	register long base;
	register unsigned char *p;
	register data_t *q;
	register unsigned long mask;
#ifdef LONG_EQ_64_BITS
	register unsigned long data;
	register unsigned long data_long;
	register int	even;
#else
	register unsigned long data;
#endif
	register int ct;
	unsigned char *p_end, *p_st;
	data_t *q_end, *q_st;
	int count, size;
	base_data_t base_data;
	int swap_work;
#if HILOOPCT
	int loop_ct = 0;
#endif

	base_data = 0;
	p = image_in;
	q = image_out;
#if BASE_TYPE == 4
	if( len_in < 8 ) {
#else
	if( len_in < 4 ) {
#endif
		ret = -5;
		goto retu;
	}
	p_end = p + len_in;
	if( max_len_out < 0 ) {
		ret = -6;
		goto retu;
	}
	q_end = q + max_len_out;
	if( exp_hibytehi_machine == exp_hibytehi_in ) {
		swap_work = 0;
	} else {
		swap_work = 1;
	}
	p_st = p;
	q_st = q;
	
start:

#if HILOOPCT
	loop_ct++;
	if( loop_ct > HILIMIT ) {
		ret = -7893490;
		goto retu;
	}
#endif
#if BASE_TYPE == 1
	*((unsigned char *)(&base_data)) = *p++;
#endif
#if BASE_TYPE == 2
	if( swap_work == 0 ) {
		*((unsigned char *)(&base_data)) = *p++;
		*(((unsigned char *)(&base_data))+1) = *p++;
	} else {
		*(((unsigned char *)(&base_data))+1) = *p++;
		*((unsigned char *)(&base_data)) = *p++;
	}
#endif
#if BASE_TYPE == 4
	if( swap_work == 0 ) {
		*((unsigned char *)(&base_data)) = *p++;
		*(((unsigned char *)(&base_data))+1) = *p++;
		*(((unsigned char *)(&base_data))+2) = *p++;
		*(((unsigned char *)(&base_data))+3) = *p++;
	} else {
		*(((unsigned char *)(&base_data))+3) = *p++;
		*(((unsigned char *)(&base_data))+2) = *p++;
		*(((unsigned char *)(&base_data))+1) = *p++;
		*((unsigned char *)(&base_data)) = *p++;
	}
#endif
	base = base_data;
	count = *p++;
	size = *p++;
#if HISIZE
	{
	static int disp_size = 0, size_print_ct = 0;
	int pix_per_ct;


	if( size != 0 ) {
		pix_per_ct = 32 / size;
	} else {
		pix_per_ct = 1;
	}
	if( disp_size + count * pix_per_ct > DISP_SIZE ) {
		if( size_print_ct++ < SIZE_PRINT_MAX ) {
			fprintf( stderr, "base=%d, count=%d, size=%d, disp=%d\n",
				base, count, size, disp_size);
		}
	}
	disp_size += count * pix_per_ct;
	}
#endif
#if BASE_TYPE == 1
	p++;
#endif
#if BASE_TYPE == 4
	p++; p++;
#endif
	if( count <= 0 ) {
		if( count == 0 ) {
			ret = 3;
#if BASE_TYPE == 4
			p_st += 2 * sizeof(int);
#else
			p_st += sizeof(int);
#endif
			goto retu;
		} else {
			ret = -3;
			goto retu;
		}
	}

#ifdef LONG_EQ_64_BITS
#define GETDATA if(even) \
	{ data = data_long&0x0ffffffff; even = 0; } else \
	{ data = data_long >> 32; even = 1; p += 2 * sizeof(int); \
		data_long = *(unsigned long *)p;}
/*********
#define GETDATA r=(unsigned char *)(&data_long); \
	*r++=*p++;*r++=*p++;*r++=*p++;*r=*p++;data=data_long;
#define GETDATA memcpy(&data_long, p, sizeof(int)); data = data_long; \
	p += sizeof(int);
**********/
#else
#define GETDATA data = *((unsigned long *)p); p += sizeof(int);
#endif

#define CF *q++ = (mask & data) + base;

#define C(SHIFT) *q++ = (mask & (data >> SHIFT )) + base;
#define CL(SHIFT) *q++ = (data >> SHIFT ) + base;

#define CD(SHIFT) data >>= SHIFT; *q++ = (mask & data) + base;
#define CDL(SHIFT) data >>= SHIFT; *q++ = data + base;

#define CFM { *((unsigned_data_t *)q) = (mask & data); q++; }
#define CLS(SHIFT) { *((unsigned_data_t *)q) = (data >> SHIFT ); q++; }

#define CFSWAP16 { *((unsigned_data_t *)q) = ((data>>24)&0x00FF)|((data>>8)&0xFF00); q++; }
#define CLSWAP16 { *((unsigned_data_t *)q) = ((data>>8)&0x00FF)|((data<<8)&0xFF00); q++; }
#define CFSWAP16B { *q = (((data>>24)&0x00FF)|((data>>8)&0xFF00))+base; q++; }
#define CLSWAP16B { *q = (((data>>8)&0x00FF)|((data<<8)&0xFF00))+base; q++; }

#define CSWAP32 { *((unsigned_data_t *)q) = ((data>>24)&0x00FF)|((data>>8)&0xFF00) \
			|((data<<24)&0xFF000000)|((data<<8)&0xFF0000) ; q++; }

#define C1_1	CF C(1) C(2) C(3) C(4) C(5) C(6) C(7)
#define C1_2	C(8) C(9) C(10) C(11) C(12) C(13) C(14) C(15)
#define C1_3	C(16) C(17) C(18) C(19) C(20) C(21) C(22) C(23)
#define C1_4	C(24) C(25) C(26) C(27) C(28) C(29) C(30) CL(31)

#define C1_F	CF CD(1) CD(1) CD(1) CD(1) CD(1) CD(1) CD(1)
#define C1_M	CD(1) CD(1) CD(1) CD(1) CD(1) CD(1) CD(1) CD(1)
#define C1_L	CD(1) CD(1) CD(1) CD(1) CD(1) CD(1) CD(1) CDL(1)

#define C2_1	CF C(2) C(4) C(6)
#define C2_2	C(8) C(10) C(12) C(14)
#define C2_3	C(16) C(18) C(20) C(22)
#define C2_4	C(24) C(26) C(28) CL(30)

#define C2_F	CF CD(2) CD(2) CD(2)
#define C2_M	CD(2) CD(2) CD(2) CD(2)
#define C2_L	CD(2) CD(2) CD(2) CDL(2)

#define C4_1	CF C(4)
#define C4_2	C(8) C(12)
#define C4_3	C(16) C(20)
#define C4_4	C(24) CL(28)

#define C4_F	CF CD(4)
#define C4_M	CD(4) CD(4)
#define C4_L	CD(4) CDL(4)

	ct = count;
	if( size != S0 ) {
		if( p + sizeof(int) * ct > p_end ) {
			ret = 2;
			goto retu;
		}
	}
#ifdef LONG_EQ_64_BITS
	if( ((int)p) & 0x07 ) {
		even = 0;
		p -= sizeof(int);
	} else {
		even = 1;
	}
	data_long = *(unsigned long *)p;
#endif
	switch( size ) {
	case S8:
		mask = M8;
		if( q + CT8*ct > q_end ) {
			goto some_out;
		}
		if( swap_work == 0 ) {
		    while( ct-- > 0 ) {
			GETDATA
			CF CD(8) CD(8) CDL(8)
		    }
		} else {
		    while( ct-- > 0 ) {
			GETDATA
			CL(24) C(16) C(8) CF
		    }
		}
		break;
	case S4:
		mask = M4;
		if( q + CT4*ct > q_end ) {
			goto some_out;
		}
		if( swap_work == 0 ) {
		    while( ct-- > 0 ) {
			GETDATA
			C4_F C4_M C4_M C4_L
		    }
		} else {
		    while( ct-- > 0 ) {
			GETDATA
			C4_4 C4_3 C4_2 C4_1
		    }
		}
		break;
#if ( DATA_TYPE == 4 || DATA_TYPE == 2 )
	case S16:
		mask = M16;
		if( q + CT16*ct > q_end ) {
			goto some_out;
		}
		if( base == 0 ) {
			if( swap_work == 0 ) {
				while( ct-- > 0 ) {
					GETDATA
					CFM CLS(16)
				}
			} else {
				while( ct-- > 0 ) {
					GETDATA
					CFSWAP16 CLSWAP16
				}
			}
		} else {
			if( swap_work == 0 ) {
				while( ct-- > 0 ) {
					GETDATA
					CF CL(16)
				}
			} else {
				while( ct-- > 0 ) {
					GETDATA
					CFSWAP16B CLSWAP16B
				}
			}
		}
		break;
#endif
	case S2:
		mask = M2;
		if( q + CT2*ct > q_end ) {
			goto some_out;
		}
		if( swap_work == 0 ) {
		    while( ct-- > 0 ) {
			GETDATA
			C2_F C2_M C2_M C2_L
		    }
		} else {
		    while( ct-- > 0 ) {
			GETDATA
			C2_4 C2_3 C2_2 C2_1
		    }
		}
		break;
	case S1:
		mask = M1;
		if( q + CT1*ct > q_end ) {
			goto some_out;
		}
		if( swap_work == 0 ) {
		    while( ct-- > 0 ) {
			GETDATA
			C1_F C1_M C1_M C1_L
		    }
		} else {
		    while( ct-- > 0 ) {
			GETDATA
			C1_4 C1_3 C1_2 C1_1
		    }
		}
		break;
	case S0:
		if( q + ct >= q_end ) {
			goto some_out;
		}
		ct >>= 2;
		while( --ct >= 0 ) {
			*q = base;
			*++q = base;
			*++q = base;
			*++q = base;
			q++;
		}
		ct = 3 & count;
		while( --ct >= 0 ) {
			*q = base;
			q++;
		}
		break;
#if DATA_TYPE == 4
	case S32:
		if( q + CT32*ct > q_end ) {
			goto some_out;
		}
		if( swap_work == 0 ) {
			while( ct-- > 0 ) {
				GETDATA
				*q++ = data;
			}
		} else {
			while( ct-- > 0 ) {
				GETDATA
				CSWAP32
			}
		}
		break;
#endif
	default:
		ret = -56547;
		goto retu;
	}

	goto end;

some_out:

	if( ! exp_expand_to_end ) {
		ret = 4;
		goto retu;
	}
#undef CF
#undef C
#undef CL
#undef CFM
#undef CLS

#define CF *q++ = (mask & data) + base; if( q >= q_end ) break;
#define C(SHIFT) *q++ = (mask & (data >> SHIFT )) + base; if( q >= q_end ) break;
#define CL(SHIFT) *q++ = (data >> SHIFT ) + base; if( q >= q_end ) break;

#define CFM { *((unsigned_data_t *)q) = (mask & data); q++; } if( q >= q_end ) break;
#define CLS(SHIFT) { *((unsigned_data_t *)q) = (data >> SHIFT ); q++; } if( q >= q_end ) break;

#undef CFSWAP16
#undef CLSWAP16
#undef CFSWAP16B
#undef CLSWAP16B
#undef CSWAP32
#define CFSWAP16 {*((unsigned_data_t *)q)=((data>>24)&0x00FF)|((data>>8)&0xFF00); q++; } \
	if( q >= q_end ) break;
#define CLSWAP16 {*((unsigned_data_t *)q)=((data>>8)&0x00FF)|((data<<8)&0xFF00); q++; } \
	if( q >= q_end ) break;
#define CFSWAP16B { *q = (((data>>24)&0x00FF)|((data>>8)&0xFF00))+base; q++; } \
	if( q >= q_end ) break;
#define CLSWAP16B { *q = (((data>>8)&0x00FF)|((data<<8)&0xFF00))+base; q++; } \
	if( q >= q_end ) break;

#define CSWAP32 { *((unsigned_data_t *)q) = ((data>>24)&0x00FF)|((data>>8)&0xFF00) \
			|((data<<24)&0xFF000000)|((data<<8)&0xFF0000) ; q++; } \
				if( q >= q_end ) break;


	ct = count;
	switch( size ) {
	case S8:
		mask = M8;
		if( swap_work == 0 ) {
		    while( ct-- > 0 ) {
			GETDATA
			CF C(8) C(16) CL(24)
		    }
		} else {
		    while( ct-- > 0 ) {
			GETDATA
			CL(24) C(16) C(8) CF
		    }
		}
		break;
	case S4:
		mask = M4;
		if( swap_work == 0 ) {
		    while( ct-- > 0 ) {
			GETDATA
			C4_1 C4_2 C4_3 C4_4
		    }
		} else {
		    while( ct-- > 0 ) {
			GETDATA
			C4_4 C4_3 C4_2 C4_1
		    }
		}
		break;
#if ( DATA_TYPE == 4 || DATA_TYPE == 2 )
	case S16:
		mask = M16;
		if( base == 0 ) {
			if( swap_work == 0 ) {
				while( ct-- > 0 ) {
					GETDATA
					CFM CLS(16)
				}
			} else {
				while( ct-- > 0 ) {
					GETDATA
					CFSWAP16 CLSWAP16
				}
			}
		} else {
			if( swap_work == 0 ) {
				while( ct-- > 0 ) {
					GETDATA
					CF CL(16)
				}
			} else {
				while( ct-- > 0 ) {
					GETDATA
					CFSWAP16B CLSWAP16B
				}
			}
		}
		break;
#endif
	case S2:
		mask = M2;
		if( swap_work == 0 ) {
		    while( ct-- > 0 ) {
			GETDATA
			C2_1 C2_2 C2_3 C2_4
		    }
		} else {
		    while( ct-- > 0 ) {
			GETDATA
			C2_4 C2_3 C2_2 C2_1
		    }
		}
		break;
	case S1:
		mask = M1;
		if( swap_work == 0 ) {
		    while( ct-- > 0 ) {
			GETDATA
			C1_1 C1_2 C1_3 C1_4
		    }
		} else {
		    while( ct-- > 0 ) {
			GETDATA
			C1_4 C1_3 C1_2 C1_1
		    }
		}
		break;
	case S0:
		while( ct-- > 0 ) {
			*q = base;
			q++;
			if( q >= q_end )
				break;
		}
		break;
#if DATA_TYPE == 4
	case S32:
		if( swap_work == 0 ) {
			while( ct-- > 0 ) {
				GETDATA
				*q++ = data;
			}
		} else {
			while( ct-- > 0 ) {
				GETDATA
				CSWAP32
			}
		}
		break;
#endif
	default:
		ret = -56548;
		goto retu;
	}

	ret = 1;
	if( size != S0 ) {
		p_st = p_st + count*sizeof(int) + HEADER_SIZE;
	} else {
		p_st = p_st + HEADER_SIZE;
	}
	q_st = q;
	goto retu;


end:
#ifdef LONG_EQ_64_BITS
	if( ! even ) {
		p += sizeof(int);
	}
#endif
	p_st = p;
	q_st = q;
	if( p >= p_end ) {
		goto retu;
	}
	if( q >= q_end ) {
		goto retu;
	}
	if( p_end - p < HEADER_SIZE ) {
		ret = 2;
		goto retu;
	}
	goto start;

retu:
	*len_used_in = p_st - image_in;
	*len_used_out = q_st - image_out;

#if HIRETURN
#if HILOOPCT
	fprintf( stderr,
"%d=expansion_data_t( len=%d,, used_in=%d, , , used_out=%d) loop_ct=%d\n",
		ret, len_in, *len_used_in, *len_used_out, loop_ct );
#else
	fprintf( stderr,
"%d=expansion_data_t( len=%d,, used_in=%d, , , used_out=%d)\n",
		ret, len_in, *len_used_in, *len_used_out );
#endif
#endif

	return( ret );
}

