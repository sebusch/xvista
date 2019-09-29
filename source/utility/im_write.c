/*
 *						im_write.c
 *						September 30, 1993
 */

#define		HI	0
#define		HIWRITE	0

#include <stdio.h>

int im_write_init = 0;
int im_write_block_size = 512;
int im_write_only_single_blocks = 0;
int im_write_end = 0;
int im_write_whole_end_block = 0;

static char *temp = NULL;
static int temp_size = 0;
static int temp_remaining = 0;
static int verbose = 1;

int
im_write_verbose( verbos )
int verbos;
{
	int ret = 0;

	verbose = verbos;

retu:
	return( ret );
}


int
im_write( fd, p, len )
int fd, len;
char *p;
{
	int ret = 0, ret2, ct, rem, block_size, ext;
	char *q;

#if HI
	static int sum = 0;

	sum += len;
	fprintf( stderr, "im_write(,,%d) sum=%d\n", len, sum);
#endif
	if( im_write_init ) {
		temp_remaining = 0;
		im_write_init = 0;
	}
	block_size = im_write_block_size;
	if( temp_size < block_size ) {
		if( temp_remaining > 0 ) {
			ret = -383477;
			goto retu;
		}
		if( temp ) {
			free( temp );
			temp = NULL;
			temp_size = 0;
		}
		if( NULL == ( temp = (char *)malloc( block_size ) ) ) {
			ret = -687649;
			goto retu;
		}
		temp_size = block_size;
	}
	if( temp_remaining > 0 && len > 0 ) {
		if( temp_remaining > block_size ) {
			ret = -383476;
			goto retu;
		}
		ext = block_size - temp_remaining;
		if( ext > len ) {
			ext = len;
		}
		q = temp + temp_remaining;
		memcpy( q, p, ext);
		temp_remaining += ext;
		p += ext;
		len -= ext;
		ret += ext;
		if( temp_remaining == block_size ) {
			ret2 = write( fd, temp, block_size );
			if( ret2 < 0 ) {
				if( verbose ) {
					fprintf( stderr, 
					"im_write %d=write(,,%d)\n", 
					ret2, block_size );
				}
				ret = -132123;
				goto retu;
			}
#if HIWRITE
			fprintf( stderr, "im_write %d=write(%d,,%d)\n", 
				ret2, fd, block_size );
#endif
			temp_remaining = 0;
		}
	}
#ifdef VAX
	im_write_only_single_blocks = 1;
#endif
	if( im_write_only_single_blocks ) {
	    while( len > block_size ) {
		ret2 = write( fd, p, block_size );
		if( ret2 < block_size ) {
			if( verbose ) {
				fprintf( stderr, 
				"im_write %d=write(%d,,%d)\n", 
				ret2, fd, block_size );
			}
			ret = -32123;
			goto retu;
		}
#if HIWRITE
		fprintf( stderr, "im_write %d=write(%d,,%d)\n", 
			ret2, fd, block_size );
#endif
		ret += ret2;
		len -= ret2;
		p += ret2;
	    }
	} else {
	    ct = block_size * ( len / block_size );
	    if( ct > 0 ) {
		ret2 = write( fd, p, ct );
		if( ret2 < ct ) {
			if( verbose ) {
				fprintf( stderr, 
				"im_write %d=write(%d,,%d)\n", 
				ret2, fd, ct );
			}
			ret = -232123;
			goto retu;
		}
#if HIWRITE
		fprintf( stderr, "im_write %d=write(%d,,%d)\n", 
			ret2, fd, ct );
#endif
		ret += ret2;
		len -= ret2;
		p += ret2;
	    }
	}
	if( len > 0 ) {
		if( temp_remaining > 0 ) {
			ret = -786754;
			goto retu;
		}
		memcpy( temp, p, len );
		temp_remaining = len;
		ret += len;
	}
	if( temp_remaining ) {
		if( im_write_end ) {
			int len_write;

			if( im_write_whole_end_block ) {
				p = temp + temp_remaining;
				ext = block_size - temp_remaining;
				for( ct = 0; ct < ext; ct++, p++ ) { *p = '\0'; }
				len_write = block_size;
			} else {
				ext = 0;	
				len_write = temp_remaining;
			}
			ret2 = write( fd, temp, len_write );
			if( ret2 < len_write ) {
				if( verbose ) {
					fprintf( stderr, 
					"im_write %d=write(,,%d)\n", 
					ret2, len_write );
				}
				ret = -332123;
				goto retu;
			}
#if HIWRITE
			fprintf( stderr, "im_write %d=write(%d,,%d)\n", 
				ret2, fd, len_write );
#endif
			ret += ext;
			temp_remaining = 0;
		} 
	}
retu:
#if HI
	fprintf( stderr, "ret = %d temp_remaining=%d\n", ret, temp_remaining);
#endif
	return( ret );
}

