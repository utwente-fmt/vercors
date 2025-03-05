package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class ConstQualifierSpec extends VercorsSpec {
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

  vercors should verify using silicon in "Const pointers add" c
"""
#include <stdlib.h>

/*@
  context_everywhere n > 0;
  context_everywhere x0 != NULL ** \pointer_length(x0) == n;
  context_everywhere x1 != NULL ** \pointer_length(x1) == n;
  ensures \result != NULL ** \pointer_length(\result) == n ** (\forall* int i; 0<=i && i<n; Perm(&\result[i], write));
  ensures (\forall int i; 0 <= i && i <n; \result[i] == x0[i] + x1[i]);
@*/
int* add(int n, const int* x0, const int* x1){
  int* res = (int*) malloc(n*sizeof(int));
  //@ assume res != NULL;
  /*@
    loop_invariant 0 <= i && i <= n;
    loop_invariant res != NULL ** \pointer_length(res) == n ** (\forall* int i; 0<=i && i<n; Perm(&res[i], write));
    loop_invariant (\forall int j; 0 <= j && j < i; res[j] == x0[j] + x1[j]);
  @*/
  for(int i=0; i<n; i++){
    res[i] = x0[i] + x1[i];
  }
  return res;
}
"""

  vercors should error withCode "disallowedQualifiedCoercion" in "Cast unique pointer" c
    """
struct vec {
  int a;
  int b;
};

  /*@ requires Perm(&v.a, write);
  @*/
int f(struct vec v){
    v.a = 1;
    const int* n = (const int*) &v;
    //@ assert *n == 1;
}
"""

  vercors should error withCode "noPermissionForConstPointer" in "Specify perm const pointer" c
  """
/*@
 requires x!= NULL ** \pointer_length(x)==1 ** Perm(x, 1\2);
@*/
void f(const int* x){
  int y = x[0];
}
"""

  vercors should verify using silicon in "Return head of const pointer" c
    """
//@ context a!= NULL ** \pointer_length(a)==1;
int foo(const int *a) {
    return *a;
}
"""

  vercors should verify using silicon in "Take address of const int" c
    """
int f() {
    const int a = 0;
    const int *b = &a;
    return *b;
}
"""

  vercors should error withCode "disallowedQualifiedType" in "Cannot take address for unique pointer" c
    """
int f() {
    /*@unique<1>@*/ int a = 0;
    /*@unique<1>@*/ int *b = &a;
    return *b;
}
"""

  vercors should verify using silicon in "Take address of const int param" c
    """
int f(const int a) {
    const int *b = &a;
    return *b;
}
"""


  /* TODO: This is possible if we want it. But it would be best to do this in one go together
   *  with making unique types work for pointer fields && it takes quite some work.
   */
  vercors should error withCode "disallowedQualifiedType" in  "Take address of const int field" c
    """
struct vec {
  const int a;
};

//@ context Perm(v, 1\2);
int f(struct vec v) {
  const int *b = &v.a;
  return *b;
}
"""

  vercors should verify using silicon in "Take address of const global int" c
    """
const int a = 5;

//@ ensures \result == 5;
int foo() {
   const int * b = &a;
   return *b;
}
"""
}

class StructQualifierSpec extends VercorsSpec {
  vercors should verify using silicon in "Unique pointer field of struct containing unique struct" c """
  struct vec2 {
    int* xxs;
  };

  struct vec {
    int y;
    int* xs;
    /*@unique_pointer_field<xxs, 3>@*/ struct vec2 v;
  };

  /*@
    context xs != NULL && xs2 != NULL;
    requires x1 != NULL ** \pointer_length(x1)==1 ** Perm(x1, write) ** Perm(*x1, write);
    context Perm(v, write) ** v.xxs != NULL;
  @*/

  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  x1, /*@ unique<2> @*/ int* xs,
      /*@unique_pointer_field<xxs, 3>@*/ struct vec2 v, /*@ unique<3>@*/ int* xs2){
    /*@unique_pointer_field<xs, 2>@*/ struct vec test;
    test.y = 5;

    x1->xs = xs;
    x1->v = v;
    //@ assert xs != NULL;
    //@ assert x1->xs != NULL;
    // assert x1->v.xxs != NULL;
    x1->v.xxs = xs2;
    //@ assert x1->v.xxs != NULL;
    return 0;
  }
  """

  vercors should verify using silicon in "Unique pointer field of struct" c """
  struct vec {
    int* xs;
  };

  /*@
    context xs != NULL;
    context x1 != NULL ** \pointer_length(x1)==1 ** Perm(x1, write) ** Perm(*x1, write);
  @*/
  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  x1, /*@ unique<2> @*/ int* xs){
    x1->xs = xs;
    //@ assert x1->xs != NULL;
    return 0;
  }
  """

  vercors should error withCode "disallowedQualifiedCoercion" in "Assign wrong type to unique pointer field of struct" c """
  struct vec {
    int* xs;
  };

  /*@
    context xs != NULL;
    context x1 != NULL ** \pointer_length(x1)==1 ** Perm(x1, write) ** Perm(*x1, write);
  @*/
  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  x1, int* xs){
    x1->xs = xs;
    //@ assert x1->xs != NULL;
    return 0;
  }
  """

  vercors should verify using silicon in "Unique pointer field of struct containing struct" c """
  struct vec2 {
    int* xxs;
  };

  struct vec {
    int* xs;
    struct vec2 v;
  };

  /*@
    context xs != NULL;
    context x1 != NULL ** \pointer_length(x1)==1 ** Perm(x1, write) ** Perm(*x1, write);
  @*/
  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  x1, /*@ unique<2> @*/ int* xs){
    x1->xs = xs;
    //@ assert xs != NULL;
    return 0;
  }
  """

  vercors should error withCode "disallowedQualifiedCoercion" in "Assign wrong type to unique pointer field of struct of struct" c """
  struct vec2 {
    int* xxs;
  };

  struct vec {
    int* xs;
    /*@unique_pointer_field<xxs, 3>@*/ struct vec2 v;
  };

  /*@
    context xs != NULL && xs2 != NULL;
    context x1 != NULL ** \pointer_length(x1)==1 ** Perm(x1, write) ** Perm(*x1, write);
    context Perm(v, write) ** v.xxs != NULL;
  @*/

  int f(struct vec*  x1, int* xs, struct vec2 v, int* xs2){
    x1->xs = xs;
    x1->v = v;
    //@ assert xs != NULL;
    //@ assert x1->xs != NULL;
    // assert x1->v.xxs != NULL;
    x1->v.xxs = xs2;
    //@ assert x1->v.xxs != NULL;
    return 0;
  }
  """

  vercors should verify using silicon in "Multiple unique pointer fields for struct" c """
  struct vec {
    int* xs;
    int* ys;
  };

  /*@
    context xs1 != NULL && ys1 != NULL;
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
  @*/
  int f(/*@unique_pointer_field<ys, 1>@*/ /*@unique_pointer_field<xs, 2>@*/ struct vec*  v,
    /*@ unique<2> @*/ int* xs1,
    /*@ unique<1> @*/ int* ys1){
      v->xs = xs1;
      //@ assert v->xs != NULL;
      v->ys = ys1;
      //@ assert v->ys != NULL;
    }
  """

  vercors should error withCode "wrongUniqueFieldStruct" in "Multiple same unique pointer fields for struct" c """
  struct vec {
    int* xs;
    int* ys;
  };

  /*@
    context xs1 != NULL && ys1 != NULL;
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
  @*/
  int f(/*@unique_pointer_field<xs, 1>@*/ /*@unique_pointer_field<xs, 2>@*/ struct vec*  v,
    /*@ unique<2> @*/ int* xs1,
    /*@ unique<1> @*/ int* ys1){
      v->xs = xs1;
      //@ assert v->xs != NULL;
      v->ys = ys1;
      //@ assert v->ys != NULL;
    }
  """

  vercors should verify using silicon in "Coerce struct" c """
  struct vec {
    int* xs;
    int* ys;
  };

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100);
    ensures \result == v->xs;
  @*/
  /*@ unique<1> @*/ int* get_xs(/*@unique_pointer_field<xs, 1>@*/ struct vec* v){
    return v->xs;
   }

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
    context v->xs != NULL;
  @*/
  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  v){
      /*@ unique<2> @*/ int* xs2 = get_xs(v);
      //@ assert xs2 != NULL;
    }
  """

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Coerce struct not allowed" c """
  struct vec {
    int* xs;
    int* ys;
  };

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100);
    ensures \result == v->xs;
  @*/
  int* get_xs(struct vec* v){
    return v->xs;
   }

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
    context v->xs != NULL;
  @*/
  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  v){
      /*@ unique<2> @*/ int* xs2 = get_xs(v);
      //@ assert xs2 != NULL;
    }
  """

  vercors should verify using silicon in "Coerce self reference struct" c """
  struct vec {
    int* xs;
    int* ys;
    struct vec* link;
  };

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100);
    ensures \result == v->xs;
  @*/
  /*@unique<1>@*/ int* get_xs(/*@unique_pointer_field<xs, 1>@*/ struct vec* v){
    return v->xs;
   }

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
    context v->xs != NULL;
  @*/
  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  v){
      /*@ unique<2> @*/ int* xs2 = get_xs(v);
      //@ assert xs2 != NULL;
    }
  """

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "coercion with invalid args inside ADTs" c
"""
struct vec {
    int* xs;
  };

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100);
    ensures v->xs  == \result;
  @*/
  int* get_xs(struct vec* v){
    return v->xs;
   }

  /*@
  ghost
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(v->xs, 1\100) ** v->xs != NULL;
    context 1 \in m.keys && m[1].size >0 ==> m[1][0].fst != NULL;
    ensures \result != NULL;
  int* get_xs2(struct vec* v, map<int, seq<tuple<int*, void*> > > m){
    if( 1 \in m.keys && m[1].size >0){
      return m[1][0].fst;
    }
    return v->xs;
  }

  @*/

  /*@
    given map<int, seq<tuple<int*, void*> > > m;
    context 1 \in m.keys && m[1].size >0 ==> m[1][0].fst != NULL;
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
    context v->xs != NULL;
  @*/
  int f(/*@unique_pointer_field<xs, 1>@*/ struct vec*  v){
      /*@unique<1>@*/int* xs2 = get_xs(v);
      //@ assert xs2 != NULL;
      //@ assert get_xs2(v, m) != NULL;
    }
"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in  "Coerce self reference struct" c """
  struct vec {
    int* xs;
    int n;
  };

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100) ** Perm(&v->n, 1\100);
    ensures \result == &(v->n);
  @*/
  int* get_xs(struct vec* v){
    return &(v->n);
   }

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
    context v->xs != NULL;
    context v->n == 42;
  @*/
  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  v){
      /*@ unique<2> @*/ int* xs2 = get_xs(v);
      //@ assert xs2 != NULL;
      //@ assert xs2[0] == 42;
    }
  """
}

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



  vercors should error withCode "disallowedQualifiedCoercion" in "different uniques param - 1" c """void f(/*@ unique<1> @*/ int* x0){ int* x1 = x0;}"""
  vercors should error withCode "disallowedQualifiedCoercion" in "different uniques param - 2" c """void f(/*@ unique<1> @*/ int* x0){ /*@ unique<2> @*/ int* x1 = x0;}"""
  vercors should error withCode "disallowedQualifiedCoercion" in "different uniques local" c """void f(){/*@ unique<1> @*/ int x0[2] = {1,2}; /*@ unique<2> @*/ int* x1; x1 = x0;}"""
  vercors should error withCode "disallowedQualifiedCoercion" in "multiple uniques parameter" c """void f(/*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){x1 = x0;}"""

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

  vercors should verify using silicon in "Call function in contract, which needs coercion" c """/*@
  context n > 0;
  context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
  context x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
  @*/
  int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
    //@ assert h(x0, x1) == x0[0] + x1[0];
    //@ assert g(x0, x1) == x0[0] + x1[0];
    return 0;
  }

  /*@
    context x0 != NULL ** \pointer_length(x0) > 0 ** Perm(&x0[0], 1\4);
    context x1 != NULL ** \pointer_length(x1) > 0 ** Perm(&x1[0], 1\4);
    ensures \result == h(x0, x1);
  @*/
  int g(int* x0, /*@ unique<2> @*/ int* x1){
    return x0[0] + x1[0];
  }


  /*@
    requires x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\4);
    requires y != NULL ** \pointer_length(y) > 0 ** Perm(&y[0], 1\4);
    ensures \result == x[0]+y[0];
  pure int h(int* x, unique<2>int * y) = x[0]+y[0];
  @*/
"""

  vercors should verify using silicon in "Call non-unique function" c """/*@
  context n > 0;
  context x0 != NULL ** \pointer_length(x0) == n ** (\forall* int i; 0<=i && i<n; Perm(&x0[i], 1\2));
  context x1 != NULL ** \pointer_length(x1) == n ** (\forall* int i; 0<=i && i<n; Perm(&x1[i], 1\2));
  @*/
  int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){
    //@ assert h(x0) + h(x1) == x0[0] + x1[0];
    return 0;
  }

  /*@
    requires x != NULL ** \pointer_length(x) > 0 ** Perm(&x[0], 1\2);
    ensures \result == x[0];
  pure int h(int* x) = x[0];
  @*/
  """

  vercors should verify using silicon in "Can coerce to function with all the same types" c """/*@
  context n > 1;
  context x0 != NULL ** \pointer_length(x0) == n;
  @*/
  int f(int n, /*@ unique<1> @*/ int* x0, /*@ unique<1> @*/ int* x1){
    h(x0, x1);
  }

  /*@ unique<1> @*/ int* h(int /*@ unique<2> @*/ * y, int /*@ unique<1> @*/ * x){
    return x;
  }"""

  vercors should verify using silicon in "Arguments are same even if original function parameters were diff" c """/*@
context n > 1;
context x0 != NULL ** \pointer_length(x0) == n;
@*/
int f(int n, /*@ unique<1> @*/ int* x0, int* x1){
  h(x0, x0);
}

/*@ unique<1> @*/ int* h(int /*@ unique<1> @*/ * x, int /*@ unique<2> @*/ * y){
  return x;
}"""

  vercors should verify using silicon in "Indirect recursive procedure call with uniques and coercion - 2" c """/*@
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

  vercors should verify using silicon in "Recursive procedure call wrong uniques" c """/*@
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

  vercors should error withCode "disallowedQualifiedType" in "multiple uniques" c """void f(/*@ unique<1> @*/ /*@ unique<2> @*/ int* x0){}"""

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

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "coercion with invalid args inside ADTs" c
    """
struct vec {
    int* xs;
  };

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100);
    ensures v->xs  == \result;
  @*/
  int* get_xs(struct vec* v){
    return v->xs;
   }

  /*@
  ghost
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(v->xs, 1\100) ** v->xs != NULL;
    context 1 \in m.keys && m[1].size >0 ==> m[1][0].fst != NULL;
    ensures \result != NULL;
  int* get_xs2(struct vec* v, map<int, seq<tuple<int*, void*> > > m){
    if( 1 \in m.keys && m[1].size >0){
      return m[1][0].fst;
    }
    return v->xs;
  }

  @*/

  /*@
    given map<int, seq<tuple<int*, void*> > > m;
    context 1 \in m.keys && m[1].size >0 ==> m[1][0].fst != NULL;
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
    context v->xs != NULL;
  @*/
  int f(/*@unique_pointer_field<xs, 1>@*/ struct vec*  v){
      /*@unique<1>@*/int* xs2 = get_xs(v);
      //@ assert xs2 != NULL;
      //@ assert get_xs2(v, m) != NULL;
    }
"""

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "struct has more int pointers" c """
  struct s {
    int* sx;
  };

  struct vec {
    int* xs;
    struct s* s;
  };

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100);
    ensures \result == v->xs;
  @*/
  int* get_xs(struct vec* v){
    return v->xs;
  }

  /*@
    context xs != NULL;
    context x1 != NULL ** \pointer_length(x1)==1 ** Perm(x1, write) ** Perm(*x1, write);
  @*/
  int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  x1, /*@ unique<2> @*/ int* xs){
    get_xs(x1);
    return 0;
  }
  """

  vercors should error withCode "disallowedQualifiedMethodCoercion" in "Unique coercion with wrong given in method" c
    """
struct vec {
    int* xs;
  };

  /*@
    given int* ys;
    yields int* res;
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100);
    ensures v->xs  == \result;
  @*/
  int* get_xs(struct vec* v){
    return v->xs;
   }

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
    context v->xs != NULL;
  @*/
  int f(/*@unique_pointer_field<xs, 1>@*/ struct vec*  v, int* ys){
      /*@unique<1>@*/int* xs2 = get_xs(v) /*@ given { ys = ys }  @*/;
      //@ assert xs2 != NULL;
    }
"""

  vercors should verify using silicon in "Unique coercion with given in method" c
    """
struct vec {
    int* xs;
  };

  /*@
    given int* ys;
    yields int* res;
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, 1\100) ** Perm(&v->xs, 1\100);
    ensures v->xs  == \result;
  @*/
  int* get_xs(struct vec* v){
    return v->xs;
   }

  /*@
    context v != NULL ** \pointer_length(v)==1 ** Perm(v, write) ** Perm(*v, write);
    context v->xs != NULL;
  @*/
  int f(/*@unique_pointer_field<xs, 1>@*/ struct vec*  v, /*@unique<1>@*/ int* ys){
      /*@unique<1>@*/int* xs2 = get_xs(v) /*@ given { ys = ys }  @*/;
      //@ assert xs2 != NULL;
    }
"""

  vercors should error withCode "disallowedQualifiedCoercion" in "Unique pointer of addr of local" c
    """
struct vec {
  int* xs;
  int n;
};

int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  v){
    int n = 2;
    /*@ unique<2> @*/ int* xs2 = &n;
  }
"""

  vercors should error withCode "disallowedQualifiedCoercion" in "Unique pointer of addr of struct field" c
    """
struct vec {
  int* xs;
  int n;
};

int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  v){
    /*@ unique<2> @*/ int* xs2 = &v->n;
  }
"""

  vercors should error withCode "disallowedQualifiedType" in "Unique pointer of addr of unique param" c
    """
struct vec {
  int* xs;
  int n;
};

int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  v, /*@unique<2>@*/ int n){
    /*@ unique<2> @*/ int* xs2 = &n;
  }
"""

  vercors should error withCode "disallowedQualifiedCoercion" in "Unique pointer of addr of param" c
    """
struct vec {
  int* xs;
  int n;
};

int f(/*@unique_pointer_field<xs, 2>@*/ struct vec*  v, int n){
    /*@ unique<2> @*/ int* xs2 = &n;
  }
"""

  vercors should error withCode "disallowedQualifiedCoercion" in "Cast unique pointer" c
    """
struct vec {
  int a;
  int b;
};

int f(struct vec v){
    /*@unique<1>@*/ int* n = (/*@unique<1>@*/ int*) &v;
}
"""

  vercors should error withCode "unsupportedCast" in "Cast int to unique pointer" c
    """
void f(){
  int x = 10342234;
  /*@unique<1>@*/ int* n = (/*@unique<1>@*/ int *) x;
}
"""

  vercors should error withCode "disallowedQualifiedCoercion" in "Addr of unique pointer" c
    """
/*@
 requires x!= NULL ** \pointer_length(x)==1 ** Perm(x, 1\2);
@*/
void f(/*@unique<1>@*/ int* x){
  int* y = &(x[0]);
}
"""
}