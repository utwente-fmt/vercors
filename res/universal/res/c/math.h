#ifndef MATH_H
#define MATH_H

/*@
  ensures \result == (\is_int(x) ? x : (double)((int)x + 1));
  decreases;
@*/
double /*@ pure @*/ ceil(double x);

/*@
  ensures \result == (x >= 0 ? x : -x);
  decreases;
@*/
double /*@ pure @*/ fabs(double x);

/*@
  ensures \result == (double)((int)x);
  decreases;
@*/
double /*@ pure @*/ floor(double x);

/*@
  // Gets replaced by internal SMT function in VerCors
  ensures \result == \pow(x, y);
  decreases;
@*/
double /*@ pure @*/ pow(double x, double y);

/*@
  ensures \result == \pow(x, 0.5);
  decreases;
@*/
double /*@ pure @*/ sqrt(double x);

/*@
  ensures !(x < 0 && \is_int(x-0.5)) ==> \result == (double)(int)(x + 0.5);
  ensures (x < 0 && \is_int(x-0.5)) ==> \result == x-0.5;
  decreases;
@*/
double /*@ pure @*/ round(double x);

#endif

