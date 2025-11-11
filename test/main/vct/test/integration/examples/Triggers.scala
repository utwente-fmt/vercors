package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class Triggers extends VercorsSpec {
  vercors should verify using silicon example "concepts/triggers/simplify_quantifiers.pvl"
  vercors should verify using silicon in "predicate application in trigger" pvl """
  resource p(int i);
  requires (\forall* int i; {:p(i):});
  void foo();
  """
  vercors should verify using silicon in "predicate application in old in trigger" pvl """
  requires a != null ** (\forall* int i = 0..a.length; Value(a[i]));
  ensures (\forall int i = 0..a.length; {:\old(a[i]):} == 0);
  void foo(int[] a);
  """
  vercors should error withCode "inlinedPatterns:disallowedTrigger" in "top-level arithmetic in trigger" pvl """
  int p(int i);
  requires (\forall* int i; {:p(i) + 1:} == 0);
  void foo();
  """
  vercors should verify using silicon in "trigger pattern with cast of non-null pointer" c """
  //@ requires a != NULL ** (\forall* int i = 0 .. 6; Perm(*{:(float *)&a[i]:}, write));
  void foo(int a[6]);
  """
  vercors should error withCode "inlinedPatterns:disallowedTrigger" in "trigger pattern with cast of nullable pointer" c """
  //@ requires a != NULL ** \pointer_length(a) == 6 ** (\forall* int i = 0 .. 6; Perm(*{:(float *)&a[i]:}, write));
  void foo(int *a);
  """
}
