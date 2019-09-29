/*
 *						expansion_util.c
 *						07/30/93
 */

#define		HISIZE		0
#define		HIRETURN	0
#define		HILOOPCT	0
#define		HILIMIT		20000	

#include <stdio.h>
#include "im_compress.h"


struct expand_s exp_data= { 1, 1, 0, 0, 1, 1, 
		EXPAND_BLOCK_SIZE, EXPAND_BUFFER_SIZE,
                COMP_VERSION_NUM, COMP_BEGIN_MAGIC_NUM, COMP_MAGIC_NUM,
                0, 0, 0, 0, 0, 0, NULL };

int exp_hibytehi_machine = HIBYTEHI;
int exp_hibytehi_in = HIBYTEHI;
int exp_expand_to_end = 1; /* expand output to the end of the output buffer
				     * even though only part of an input segment
				     * will be processed.
				     * You would want to set this to a value of 0
				     * if you planned to fill the output by
				     * whole segments.  This allows the program to be
				     * used in a sequence of calls where each call could
				     * have access to only part of the input and/or have 
				     * room for only part of the output.
				     */

int
expand_machine_order(highbytehigh) 
int highbytehigh;
{
	int ret = 0;

	if( highbytehigh != 0 ) highbytehigh = 1;
	exp_hibytehi_machine = highbytehigh;

	return( ret );
};

int
expand_image_in_order(highbytehigh)
int highbytehigh;
{
	int ret = 0;

	if( highbytehigh != 0 ) highbytehigh = 1;
	exp_hibytehi_in = highbytehigh;

	return( ret );
};

int
expand_to_end( sw )
int sw;
{
	exp_expand_to_end = sw;

	return( 0 );
}


int
expand_get_int( p_in, p_out)
char *p_in, *p_out;
{
        int ret = 0, swap_work;
        if( exp_hibytehi_machine == exp_hibytehi_in ) {
                swap_work = 0;
        } else {
                swap_work = 1;
        }
        if( swap_work ) {
                p_out += 3;
                *p_out-- = *p_in++; *p_out-- = *p_in++; *p_out-- = *p_in++; *p_out = *p_in;
        } else {
                *p_out++ = *p_in++; *p_out++ = *p_in++; *p_out++ = *p_in++; *p_out = *p_in;
        }

        return( ret );
}

