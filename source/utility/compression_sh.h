/*
 *                                      compression_sh.h
 *                                      August 3, 1993
 */

DOXBIT:
#if HICT
   CTX
#endif
   INIT(SX)
   low_sw = 0;
   len = ct / SHORT_PER_INT;
   if( LOWX_D == 1 ) {
      if( compression_0bit_ok == 0 ) {
          ck = 0;
      }
   }
   left_in = len_in - ( p - p_in);
#if HISAMPLE
   if( ct_test < SAMPLECT ) {
      fprintf( fp_test, "%d ct=%d min=%d, max=%d, left_in=%d, ck=%d\n", 
	SX, ct, min, max, left_in, ck);
      fflush( fp_test );
   }
#endif
CONTXBIT:
   if( len > 254 ) { goto ENDXBIT; }
   if( left_in < LOOK_FACTOR * SHORT_PER_INT ) {
      ck = 0;
      if( left_in < SHORT_PER_INT ) { goto ENDXBIT; }
   }
   MMST_X CKMMT
   CK_COMB
   d2 = t_max - t_min;
   if( d2 >= LOWX_D || ck == 0) {
      if( d < HIGHX_D ) {
         len++;
         left_in -= SHORT_PER_INT;
         min = q_min;
         max = q_max;
         goto CONTXBIT;
      } else {
         goto ENDXBIT;
      }
   } else {
      s_p = p;
      MMREST_X CKMMT
      d2 = t_max - t_min;
      if( d2 < LOWX_D ) {
         low_sw = 1;
         goto ENDXBIT;
      } else {
         if( d < HIGHX_D ) {
            p = s_p;
            len++;
            left_in -= SHORT_PER_INT;
            min = q_min;
            max = q_max;
            goto CONTXBIT;
         } else {
            goto ENDXBIT;
         }
      }
   }
ENDXBIT:
#if HISAMPLE
   if( ct_test++ < SAMPLECT ) {
      int ct2 = 0, v, last_v;
      int first_sw = 1;

      ct = len * SHORT_PER_INT;
      p = p_st;
      fprintf( fp_test, "%d ct=%d min=%d, max=%d ", SX, ct, min, max);
      if( low_sw ) {
         fprintf( fp_test, "t_min=%d t_max=%d LOWLOWX_D=%d", 
            t_min, t_max, LOWLOWX_D);
      }
      fprintf( fp_test, "\n");
      fflush( fp_test );
      while( ct-- > 0 ) { 
         v = *p;
         if( first_sw ) {
            first_sw = 0;
            last_v = v + 1;
         }
         if( v != last_v ) {
            ct2 += 5;
            fprintf( fp_test, " %d", v); 
         } else {
            ct2 += 1;
            fprintf( fp_test, "."); 
         }
         last_v = v;
         p++;
         if( ct2 > 90 ) {
            fprintf( fp_test, "\n  " );
            fflush( fp_test );
            ct2 = 3;
         }
      }
      fprintf( fp_test, "\n" );
      fflush( fp_test );
   }
#endif
   ct = len; 
   p = p_st;
   while( ct-- ) { OUTX }
   END
   if( low_sw ) {
      if( d < LOWLOWX_D ) {
         goto start;
      } else {
	 left_out = len_out - ( p_o - p_out );
	 if( left_out < MAX_SIZE_OUT ) {
	    goto start;
         }
	 if( compression_to_end == 0 && left_in < 32 * 256 ) {
	    ret = -2; goto retu;
         }
         ck = 1;
         min = t_min;
         max = t_max;
         ct = LOOK_FACTOR * SHORT_PER_INT;
         p_st = p;
	 p += ct;
         goto DOLOWERBITS;
      }
   } else {
      goto start;
   }

#undef DOXBIT
#undef CTX
#undef SX
#undef SHORT_PER_INT
#undef OUTX
#undef CONTXBIT
#undef MMST_X
#undef MMREST_X
#undef LOWX_D
#undef LOWLOWX_D
#undef DOLOWERBITS
#undef HIGHX_D
#undef ENDXBIT

