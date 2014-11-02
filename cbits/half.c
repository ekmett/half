unsigned short hs_floatToHalf (float f) {
  union { float d; unsigned int i; } u = { f };
  int s =  (u.i >> 16) & 0x00008000;
  int e = ((u.i >> 23) & 0x000000ff) - 112;
  int m =          u.i & 0x007fffff;
  if (e <= 0) {
    if (e < -10) return s; /* underflowed */
    /* force leading 1 and round */
    m |= 0x00800000;
    int t = 14 - e;
    int a = (1 << (t - 1)) - 1;
    int b = (m >> t) & 1;
    return s | ((m + a + b) >> t);
  } 
  if (e == 143) {
    if (m == 0) return s | 0x7c00; /* +/- infinity */

    /* NaN, m == 0 forces us to set at least one bit and not become an infinity */
    m >>= 13;
    return s | 0x7c00 | m | (m == 0); 
  }
  
  /* round the normalized float */
  m += 0x00000fff + ((m >> 13) & 1);

  /* significand overflow */
  if (m & 0x00800000) {
     m =  0;
     e += 1;
  }

  /* exponent overflow */
  if (e > 30) return s | 0x7c00;

  return s | (e << 10) | (m >> 13);
}


float hs_halfToFloat (unsigned short c) {
  int s = (c >> 15) & 0x0001;
  int e = (c >> 10) & 0x001f;
  int m =         c & 0x03ff;
  if (e == 0) {
    if (m == 0) /* +/- 0 */ return s << 31;
    else {
      /* denormalized */
      while (!(m & 0x00000400)) {
        m <<= 1;
        e -=  1;
      }
      e += 1;
      m &= ~0x00000400;
    }
  } else if (e == 31) {
    if (m == 0)  /* +/- infinity */ return (s << 31) | 0x7f800000;
    else return (s << 31) | 0x7f800000 | (m << 13);
  }  
  e += 112;
  m <<= 13;
  union { float d; unsigned int i; } u;
  u.i = (s << 31) | (e << 23) | m;
  return u.d;
}
