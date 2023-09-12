#ifndef MATH_H
#define MATH_H


/*@
  // Gets replaced by internal SMT function in VerCors
  decreases;
pure bool is_int(float x);
@*/

/*@
  // Gets replaced by internal SMT function in VerCors
  decreases;
pure float pow_math_h(float x, float y);
@*/

/*@
  ensures \result == (is_int(x) ? x : (float)((int)x + 1));
  decreases;
@*/
float /*@ pure @*/ ceilf(float x);

/*@
  ensures \result == (float)((int)x);
  decreases;
@*/
float /*@ pure @*/ floor(float x);

/*@
  decreases;
@*/
float /*@ pure @*/ fabs(float x) = x >= 0 ? x : -x;

/*@
  // Gets replaced by internal SMT function in VerCors
  ensures \result == pow_math_h(x, y);
  decreases;
@*/
float /*@ pure @*/ pow(float x, float y);


#endif

