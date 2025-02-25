#ifndef MATH_H
#define MATH_H

# define M_PI           3.14159265358979323846  /* pi */

/*@
  decreases;
pure double M_PI() = 3.14159265358979323846;
@*/

/*@
  ensures \result == (x >= 0 ? x : -x);
  decreases;
@*/
float /*@ pure @*/ fabsf(float x);

/*@
  requires x>= 0;
  ensures \result == \pow(x, 0.5);
  decreases;
@*/
float /*@ pure @*/ sqrtf(float x);

/*@ ensures \result >= -1 && \result <= 1;
    decreases;
@*/
float /*@ pure @*/ sinf(float x);

/*@
  requires x >= -1 && x <= 1;
  ensures \result >= -M_PI()/2 && \result <= M_PI()/2;
  decreases;
@*/
float /*@ pure @*/ asinf(float x);
/*@
  ensures \result >= -1 && \result <= 1;
  decreases;
@*/
float /*@ pure @*/ cosf(float x);

/*@
  requires x >= -1 && x <= 1;
  ensures \result >= 0 && \result <= M_PI();
  decreases;
@*/
float /*@ pure @*/ acosf(float x);

/*@
  ensures x == 0 ==> \result == 0;
  decreases;
@*/
float /*@ pure @*/ tanf(float x);

/*@
  ensures \result >= -M_PI()/2 && \result <= M_PI()/2;
  decreases;
@*/
float /*@ pure @*/ atanf(float x);

/*@
  ensures -M_PI() <= \result && \result <= M_PI();
  decreases;
@*/
float /*@ pure @*/ atan2f(float x, float y);

/*@
  ensures \result == (expf(x) - expf(-x)) \ 2;
  decreases;
@*/
float /*@ pure @*/ sinhf(float x);

/*@
  ensures \result == (expf(x) + expf(-x)) \ 2;
  decreases;
@*/
float /*@ pure @*/ coshf(float x);

/*@
  ensures \result == (expf(x) - expf(-x)) \ (expf(x) + expf(-x));
  decreases;
@*/
float /*@ pure @*/ tanhf(float x);

/*@
  ensures \result == powf(x*x + y*y, 0.5);
  decreases;
@*/
float /*@ pure @*/ hypotf(float x, float y);

/*@
  ensures \result == powf(2.7182818284, x);
  decreases;
@*/
float /*@ pure @*/ expf(float x);

/*@
  decreases;
@*/
float /*@ pure @*/ logf(float x);

/*@
  ensures \result == \pow(x, y);
  ensures x > 0 ==> \result > 0;
  decreases;
@*/
float /*@ pure @*/ powf(float x, float y);

/*@
  ensures \result == (float)((int)x);
  decreases;
@*/
float /*@ pure @*/ floorf(float x);

/*@
  ensures \result == (\is_int(x) ? x : (float)((int)x + 1));
  decreases;
@*/
float /*@ pure @*/ ceilf(float x);

/*@
  ensures !(x < 0 && \is_int(x-0.5)) ==> \result == (float)(int)(x + 0.5);
  ensures (x < 0 && \is_int(x-0.5)) ==> \result == x-0.5;
  decreases;
@*/
float /*@ pure @*/ roundf(float x);

/*@
  ensures \result == (x >= 0 ? x : -x);
  decreases;
@*/
double /*@ pure @*/ fabs(double x);

/*@
  requires x>= 0;
  ensures \result == pow(x, 0.5);
  decreases;
@*/
double /*@ pure @*/ sqrt(double x);

/*@ ensures \result >= -1 && \result <= 1;
    decreases;
@*/
double /*@ pure @*/ sin(double x);

/*@
  requires x >= -1 && x <= 1;
  ensures \result >= -M_PI()/2 && \result <= M_PI()/2;
  decreases;
@*/
double /*@ pure @*/ asin(double x);
/*@
  ensures \result >= -1 && \result <= 1;
  decreases;
@*/
double /*@ pure @*/ cos(double x);

/*@
  requires x >= -1 && x <= 1;
  ensures \result >= 0 && \result <= M_PI();
  decreases;
@*/
double /*@ pure @*/ acos(double x);

/*@
  ensures x == 0 ==> \result == 0;
  decreases;
@*/
double /*@ pure @*/ tan(double x);

/*@
  ensures \result >= -M_PI()/2 && \result <= M_PI()/2;
  decreases;
@*/
double /*@ pure @*/ atan(double x);

/*@
  ensures -M_PI() <= \result && \result <= M_PI();
  decreases;
@*/
double /*@ pure @*/ atan2(double x, double y);

/*@
  //ensures \result == (exp(x) - exp(-x)) \ 2;
  decreases;
@*/
double /*@ pure @*/ sinh(double x);

/*@
  ensures \result == (exp(x) + exp(-x)) \ 2;
  decreases;
@*/
double /*@ pure @*/ cosh(double x);

/*@
  ensures \result == (exp(x) - exp(-x)) \ (exp(x) + exp(-x));
  decreases;
@*/
double /*@ pure @*/ tanh(double x);

/*@
  ensures \result == pow(x*x + y*y, 0.5);
  decreases;
@*/
double /*@ pure @*/ hypot(double x, double y);

/*@
  ensures \result == pow(2.7182818284, x);
  decreases;
@*/
double /*@ pure @*/ exp(double x);

/*@
  decreases;
@*/
double /*@ pure @*/ log(double x);

/*@
  ensures \result == \pow(x, y);
  ensures x > 0 ==> \result > 0;
  decreases;
@*/
double /*@ pure @*/ pow(double x, double y);

/*@
  ensures \result == (double)((int)x);
  decreases;
@*/
double /*@ pure @*/ floor(double x);

/*@
  ensures \result == (\is_int(x) ? x : (double)((int)x + 1));
  decreases;
@*/
double /*@ pure @*/ ceil(double x);

/*@
  ensures !(x < 0 && \is_int(x-0.5)) ==> \result == (double)(int)(x + 0.5);
  ensures (x < 0 && \is_int(x-0.5)) ==> \result == x-0.5;
  decreases;
@*/
double /*@ pure @*/ round(double x);

#endif

