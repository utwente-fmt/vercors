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
  void f(){/*@ unique<1> @*/ int* x0 = (/*@ unique<1> @*/ int*) malloc(sizeof(int)*2); /*@ unique<1> @*/ int* x1; x1 = x0;}"""
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
  vercors should error withCode "disallowedConstAssignment" in "Assign to local array of consts" c """void f(){const int x[2] = {0, 2}; x[0] = 1;}"""
  vercors should error withCode "disallowedConstAssignment" in "Assign to local pointer of consts" c """void f(){const int *x; x[0] = 1;}"""
  vercors should error withCode "disallowedConstAssignment" in "Assign to param pointer of consts" c """void f(const int *x){x[0] = 1;}"""

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
  context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
  context x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
@*/
int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
  if(n == 1){
    return x0[0] + x1[0];
  }
  else {
    return f(n-1, x1, x0);
  }
}"""
}

class QualifierSpecWIP2 extends VercorsSpec {

}

class QualifierSpecWIP extends VercorsSpec {
  
}