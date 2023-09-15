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
pure double pow_math_h(double x, double y);
@*/

/*@
  ensures \result == (is_int(x) ? x : (double)((int)x + 1));
  decreases;
@*/
double /*@ pure @*/ ceil(double x);
/*@
  decreases;
@*/
double /*@ pure @*/ fabs(double x) = x >= 0 ? x : -x;

/*@
  ensures \result == (double)((int)x);
  decreases;
@*/
double /*@ pure @*/ floor(double x);

/*@
  // Gets replaced by internal SMT function in VerCors
  ensures \result == pow_math_h(x, y);
  decreases;
@*/
double /*@ pure @*/ pow(double x, double y);

/*@
  // Given precision, since we cannot express irrational numbers
  given double eps;
  requires eps > 0.0;
  requires x >= 0.0;
  ensures x - eps <= \result*\result && x + eps < \result*\result;
  decreases;
@*/
double /*@ pure @*/ sqrt(double x);

/*@
  ensures \result == (double)(int)(x + 0.5);
  decreases;
@*/
double /*@ pure @*/ round(double x);

#endif

