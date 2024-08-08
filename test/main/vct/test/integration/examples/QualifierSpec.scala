package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class QualifierSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/unique/arrays.c"

  vercors should verify using silicon in "same uniques pointer parameter" c """void f(/*@ unique<1> @*/ int* x0, /*@ unique<1> @*/ int* x1){x1 = x0;}"""
  vercors should verify using silicon in "same uniques array parameter" c """void f(/*@ unique<1> @*/ int x0[], /*@ unique<1> @*/ int x1[]){x1 = x0;}"""
  vercors should verify using silicon in "same uniques local" c """void f(){/*@ unique<1> @*/ int x0[2]; /*@ unique<1> @*/ int* x1; x1 = x0;}"""
  vercors should verify using silicon in "same uniques local with inits" c """void f(){/*@ unique<1> @*/ int x0[2] = {1,2}; /*@ unique<1> @*/ int* x1; x1 = x0;}"""
  vercors should verify using silicon in "malloc uniques" c
    """#include <stdlib.h>
  void f(){/*@ unique<1> @*/ int* x0 = (/*@ unique<1> @*/ int*) malloc(sizeof(int)*2); /*@ unique<1> @*/ int* x1; x1 = x0; free(x0);}"""
  vercors should verify using silicon in "uniques pointer of unique pointer" c """void f(){/*@ unique<1> @*/ int * /*@ unique<2> @*/ * x0;}"""

  vercors should error withCode "disallowedQualifiedCoercion" in "malloc different uniques" c
    """#include <stdlib.h>
  void f(){/*@ unique<1> @*/ int* x0; x0 = (/*@ unique<2> @*/ int*) malloc(sizeof(int)*2); /*@ unique<1> @*/ int* x1; x1 = x0;}"""

  vercors should error withCode "resolutionError:type" in "diff uniques pointer of unique pointer - 1" c """void f(){/*@ unique<1> @*/ int * /*@ unique<2> @*/ * x0; /*@ unique<3> @*/ int * /*@ unique<4> @*/ * x1; x0 = x1;}"""
  vercors should error withCode "disallowedQualifiedCoercion" in "diff uniques pointer of unique pointer - 2" c """void f(){/*@ unique<1> @*/ int * /*@ unique<2> @*/ * x0; /*@ unique<1> @*/ int * /*@ unique<4> @*/ * x1; x0 = x1;}"""
  vercors should error withCode "resolutionError:type" in "diff uniques pointer of unique pointer - 3" c """void f(){/*@ unique<1> @*/ int * /*@ unique<2> @*/ * x0; /*@ unique<3> @*/ int * /*@ unique<2> @*/ * x1; x0 = x1;}"""

  vercors should error withCode "cTypeNotSupported" in "multiple uniques" c """void f(/*@ unique<1> @*/ /*@ unique<2> @*/ int* x0){}"""
  vercors should error withCode "disallowedQualifiedCoercion" in "different uniques param - 1" c """void f(/*@ unique<1> @*/ int* x0){ int* x1 = x0;}"""
  vercors should error withCode "disallowedQualifiedCoercion" in "different uniques param - 2" c """void f(/*@ unique<1> @*/ int* x0){ /*@ unique<2> @*/ int* x1 = x0;}"""
  vercors should error withCode "disallowedQualifiedCoercion" in "different uniques local" c """void f(){/*@ unique<1> @*/ int x0[2] = {1,2}; /*@ unique<2> @*/ int* x1; x1 = x0;}"""
  vercors should error withCode "disallowedQualifiedCoercion" in "multiple uniques parameter" c """void f(/*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){x1 = x0;}"""

  vercors should verify using silicon in "Assign to init const" c """void f(){const int x = 2; /*@ assert x == 2; @*/}"""
  vercors should error withCode "disallowedConstAssignment" in "Assign to local const" c """void f(){const int x; x = 0;}"""
  vercors should error withCode "disallowedConstAssignment" in "Assign to param const" c """void f(const int x){x = 0;}"""

  vercors should verify using silicon in "Assign to init const array" c """void f(){const int x[2] = {0, 2}; /*@ assert x[1] == 2; @*/}"""
  vercors should error withCode "disallowedConstAssignment" in "Assign to local array of const" c """void f(){const int x[2] = {0, 2}; x[0] = 1;}"""
  vercors should error withCode "disallowedConstAssignment" in "Assign to local pointer of const" c """void f(){const int *x; x[0] = 1;}"""
  vercors should error withCode "disallowedConstAssignment" in "Assign to param pointer of const" c """void f(const int *x){x[0] = 1;}"""

  vercors should verify using silicon in "Assign const array to const pointer" c """void f(const int* y){const int x[2] = {0, 2}; y = x;}"""
  vercors should error withCode "resolutionError:type" in "Assign const array to non-const pointer" c """void f(int* y){const int x[2] = {0, 2}; x = y;}"""

  vercors should error withCode "disallowedConstAssignment" in "Assign const pointer" c """void f(int* const y){int* const x; y = x;}"""
  vercors should verify using silicon in "Assign element of const pointer" c
    """/*@ context x!=NULL ** \pointer_length(x) == 1 ** Perm(&x[0], write); ensures x[0] == 1;@*/
  void f(int * const x){x[0] = 1;}"""

  vercors should verify using silicon in "Call non-unique procedure" c """/*@
  context n > 0;
  context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
  context x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
  ensures \result == x0[0] + x1[0];
  @*/
  int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
    return h(x0) + h(x1);
  }

  /*@
    context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\2);
    ensures \result == x[0];
  @*/
  int h(int* x){
    return x[0];
  }"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Recursive procedure call wrong uniques" c """/*@
  context n > 0;
  context x0 != NULL ** \pointer_length(x0) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
  context x1 != NULL ** \pointer_length(x1) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
@*/
int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
  if(n == 1){
    return x0[0] + x1[0];
  }
  else {
    return f(n-1, x1, x0);
  }
}"""

  vercors should verify using silicon in "Recursive procedure call with uniques" c """/*@
  context n > 0;
  context x0 != NULL ** \pointer_length(x0) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
  context x1 != NULL ** \pointer_length(x1) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
@*/
int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
  if(n == 1){
    return x0[0] + x1[0];
  }
  else {
    return f(n-1, x0, x1);
  }
}"""

  vercors should verify using silicon in "Recursive procedure call with uniques and coercion" c """/*@
  context n > 0;
  context x0 != NULL ** \pointer_length(x0) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
  context x1 != NULL ** \pointer_length(x1) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
  ensures \result == x0[0] + x1[0];
@*/
int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
  if(n == 1){
    return h(x0) + h(x1);
  }
  else {
    return f(n-1, x0, x1);
  }
}

/*@
  context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\2);
  ensures \result == x[0];
@*/
int h(int* x){
  return x[0];
}
"""

  vercors should verify using silicon in "Call procedure with multiple consistent coercions" c """/*@
context n > 0;
context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
context x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
ensures \result == 2*x0[0] + 2*x1[0];
@*/
int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
  return h(x0, x0) + h(x1, x1);
}

/*@
  context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\4);
  context y != NULL ** \pointer_length(y) > 0 ** Perm(&y[0], 1\4);
  ensures \result == x[0] + y[0];
@*/
int h(int* x, int* y){
  return x[0] + y[0];
}"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Call procedure with multiple inconsistent coercions" c """/*@
context n > 0;
context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
context x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
ensures \result == x0[0] + x1[0];
@*/
int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
  return h(x0, x1);
}

/*@
  context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\4);
  context y != NULL ** \pointer_length(y) > 0 ** Perm(&y[0], 1\4);
  ensures \result == x[0] + y[0];
@*/
int h(int* x, int* y){
  return x[0] + y[0];
}"""

  vercors should error withCode "resolutionError:type" in "Cannot coerce pointers of pointers" c """/*@
context n > 0;
context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
@*/
int f(int n, /*@ unique<1> @*/ int* x0){
  /*@ unique<1> @*/ int y[1] = {1};
  /*@ unique<1> @*/ int* yy[1] = {y};
  return h(x0, yy);
}

/*@
  context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\4);
  context yy != NULL ** \pointer_length(yy) > 0 ** Perm(&yy[0], 1\4);
  context yy[0] != NULL ** \pointer_length(yy[0]) > 0 ** Perm(&yy[0][0], 1\4);
  ensures \result == x[0] + yy[0][0];
@*/
int h(int* x, int** yy){
  return x[0] + yy[0][0];
}"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Disallow coercion of types which are subtypes of other types" c """/*@
context n > 0;
context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
@*/
int f(int n, /*@ unique<1> @*/ int* x0){
  int y[1] = {1};
  int* yy[1] = {y};
  return h(x0, yy);
}

/*@
  context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\4);
  context yy != NULL ** \pointer_length(yy) > 0 ** Perm(&yy[0], 1\4);
  context yy[0] != NULL ** \pointer_length(yy[0]) > 0 ** Perm(&yy[0][0], 1\4);
  ensures \result == x[0] + yy[0][0];
@*/
int h(int* x, int** yy){
  return x[0] + yy[0][0];
}"""

  vercors should verify using silicon in "Indirect recursive procedure call with uniques and coercion" c """/*@
context n > 0;
context x0 != NULL ** \pointer_length(x0) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
context x1 != NULL ** \pointer_length(x1) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
ensures \result == x0[0] + x1[0];
@*/
int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
  if(n == 1){
    return h(x0) + h(x1);
  }
  else {
    return g(n-1, x0, x1);
  }
}

/*@
  context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\2);
  ensures \result == x[0];
@*/
int h(int* x){
  return x[0];
}

/*@
  context n > 0;
  context x != NULL ** \pointer_length(x) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x[i], 1\2));
  context y != NULL ** \pointer_length(y) >= n ** (\forall* int i; 0<=i && i<n; Perm(&y[i], 1\2));
  ensures \result == x[0] + y[0];
@*/
int g(int n, /*@ unique<1> @*/ int* x, /*@ unique<2> @*/ int* y){
  return f(n, x, y);
}"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Indirect recursive procedure call with uniques and coercion" c """/*@
context n > 0;
context x0 != NULL ** \pointer_length(x0) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
context x1 != NULL ** \pointer_length(x1) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
ensures \result == x0[0] + x1[0];
@*/
int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
  if(n == 1){
    return h(x0) + h(x1);
  }
  else {
    return g(n-1, x1, x0);
  }
}

/*@
  context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\2);
  ensures \result == x[0];
@*/
int h(int* x){
  return x[0];
}

/*@
  context n > 0;
  context x != NULL ** \pointer_length(x) >= n ** (\forall* int i; 0<=i && i<n; Perm(&x[i], 1\2));
  context y != NULL ** \pointer_length(y) >= n ** (\forall* int i; 0<=i && i<n; Perm(&y[i], 1\2));
  ensures \result == x[0] + y[0];
@*/
int g(int n, /*@ unique<1> @*/ int* x, /*@ unique<2> @*/ int* y){
  return f(n, x, y);
}"""

  vercors should verify using silicon in "Call procedure which already has unique type" c """/*@
context n > 0;
context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
context x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
ensures \result == x0[0] + x1[0];
@*/
int f(int n, /*@ unique<1> @*/ int* x0, int* x1){
  return h(x0) + h(x1);
}

/*@
  context x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\4);
  ensures \result == x[0];
@*/
int h(/*@ unique<2> @*/ int* x){
  return x[0];
}"""

  vercors should verify using silicon in "Call procedure which returns pointer" c """/*@
context n > 0;
context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
context x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
ensures \result == x0[0] + x1[0];
@*/
int f(int n, /*@ unique<1> @*/ int* x0, int* x1){
  int y = h(x0)[0];
  /*@ unique<1> @*/ int* yy = h(x0);
  return h(x0)[0] + h(x1)[0];
}

/*@
  ensures \result == \old(x);
@*/
int* h(int* x){
  return x;
}"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Call procedure with unsupported return type" c """/*@
context n > 1;
context x0 != NULL ** \pointer_length(x0) == n;
@*/
int f(int n, /*@ unique<1> @*/ int* x0){
  /*@ unique<2> @*/ int* yy = h(x0);
}

/*@
  ensures \result == \old(x);
@*/
int* h(int* x){
  return x;
}"""

  vercors should error withCode "disallowedQualifiedCoercion" in "Returns non-unique when should" c """
int* h(int /*@ unique<1> @*/ * x, int /*@ unique<2> @*/ * y){
  return x;
}
"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Arguments are same but should be unique" c """/*@
context n > 1;
context x0 != NULL ** \pointer_length(x0) == n;
@*/
int f(int n, /*@ unique<1> @*/ int* x0, int* x1){
  h(x0, x0);
}

/*@ unique<1> @*/ int* h(int /*@ unique<1> @*/ * x, int /*@ unique<2> @*/ * y){
  return x;
}"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Return type cannot be same as coerced argument type" c """/*@
context n > 1;
context x0 != NULL ** \pointer_length(x0) == n;
@*/
int f(int n, /*@ unique<1> @*/ int* x0, int* x1){
  h(x0);
}

/*@ unique<1> @*/ int* h(int /*@ unique<2> @*/ * y){
  return NULL;
}"""
}