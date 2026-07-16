package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class AssignSuchThatSpec extends VercorsSpec {
  vercors should verify using silicon in "examples using assign such that" pvl """
  requires |s| > 0;
  ensures \result in s;
int test1(set<int> s){
  int x, y;
  x :| x \in s;
  return x;
}

  requires |s| > 0;
  ensures \result in s;
int test2(set<int> s){
  int x, y;
  x :| x \in s;
  y :| y \in s;
  // Non deterministic assignment of x and y, so not the same
  /*[/expect assertFailed:false]*/
  assert x == y;
  /*[/end]*/
  return x;
}

  requires |s| > 0;
  ensures \result in s;
int test3(set<int> s){
  int x :| x \in s;
  return x;
}

  requires |s| > 0;
int test4(set<int> s){
  /*[/expect assignSuchThatFailed]*/
  set<int> t :| |t| > 0 && t < s;
  return 0;
  /*[/end]*/
}
  requires |s| > 0;
int test6(set<int> s){
  int[] xs;
  /*[/expect arrayPerm]*/
  xs :| xs != null && xs.length > 0 && xs[0]==0 ;
  /*[/end]*/
  return 0;
}


  context xs != null && xs.length > 0;
  context Perm(xs[*], 1\2);
  requires (\exists int i; 0 <= i && i < xs.length && xs[i] == 0);
int test8(set<int> s, int[] xs){
  int i :| 0 <= i && i < xs.length && xs[i] == 0;
  return 0;
}

  requires |s| > 0;
  ensures \result in s;
pure int test9(set<int> s){
  int x :| x \in s;
  return x;
}

  requires |s| > 0;
  ensures \result in s;
  ensures (\let int x :| x \in s; x) in s;
pure int test10(set<int> s) = (\let int x :| x \in s; x);
  """

  vercors should error withCode "resolutionError:type" in "Condition needs to be boolean valued" pvl """
   requires |s| > 0;
int test5(set<int> s){
  int[] xs;
  xs :| xs != null ** xs.length > 0 ** (Perm(xs[0], 1\2)) ** xs[0]==0 ;
  return 0;
}
  """

  vercors should error withCode "disallowedAssignmentTargetForSuchThat" in "Can only assign local variables" pvl """
  requires xs != null && xs.length > 0;
int test7(set<int> s, int[] xs){
  xs[0] :| xs[0] == 0;
  return 0;
}

  """

  vercors should error withCode "letSuchThatOnlyInPure" in "Do us let such that in normal methods" pvl """
  requires |s| > 0;
  ensures \result in s;
int test8(set<int> s){
  int x;
  x = (\let int x :| x \in s; x);
  return x;
}
  """
}