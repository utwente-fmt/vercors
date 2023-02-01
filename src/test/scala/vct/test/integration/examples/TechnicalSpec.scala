package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalSpec extends VercorsSpec {
  vercors should verify using silicon in "example using triggers" java """
    class C {
      Object[] arr;

      C() {
        int T = 5;
        arr = new Object[T];
        /*@
          loop_invariant Value(arr) ** \array(arr, T);
          loop_invariant Perm(arr[*], write);
          loop_invariant 0 <= i && i <= T;
          loop_invariant (\forall int j = 0 .. i, int k = 0 .. i; j != k ==> {:arr[j]:} != {:arr[k]:});
          loop_invariant (\forall int j, int k; 0 <= j && j < i && 0 <= k && k < i; j != k ==> {:arr[j]:} != {:arr[k]:});
        @*/
        for (int i = 0; i < T; i++) {
          arr[i] = new Object();
          //@ assume (\forall int j = 0 .. i; arr[j] != arr[i]);
        }
      }
    }
  """

  vercors should error withCode "resolutionError" in "constructor using `this`" pvl """
    class err {
      int x;

      requires PointsTo(x,write,3);
      ensures  PointsTo(x,write,4);
      constructor(){
        x=x+1;
      }
    }
  """

  vercors should error withCode "notALocation" in "example asserting read permission over argument" pvl """
    class C {}
    requires Value(arg);
    void main(C arg);
  """

  vercors should error withCode "notALocation" in "example asserting permission over argument" pvl """
    class C {}
    requires Perm(arg, write);
    void main(C arg);
  """

  vercors should verify using anyBackend in "example showing comparison of unrelated types" pvl """
    void test() {
      /*[/expect assertFailed:false]*/
      assert 1 == false;
      /*[/end]*/
    }
  """

  vercors should error withCode "resolutionError" in "example quantifying a resource with \\forall" pvl """
    class Test {
      int x;
      void test() {
        assert (\forall int i; true; Value(x));
      }
    }
  """

  vercors should error withCode "resolutionError" in "example unfolding abstract predicate" pvl """
    resource p();

    requires p();
    void test() {
      unfold p();
    }
  """

  vercors should error withCode "resolutionError" in "example unfolding abstract predicate inline" pvl """
    resource p();

    requires p();
    pure int f() = \Unfolding p() \in 0;
  """

  vercors should verify using anyBackend in "example with incorrect boolean logic" pvl """
    /*[/expect postFailed:false]*/
    requires false || true;
    ensures false && true;
    void m(){}
    /*[/end]*/
  """

  vercors should verify using anyBackend in "example with vacuously quantified read permission" pvl """
    class rewriterIssue {
      int x;

      /*[/expect postFailed:perm]*/
      // assumes nothing
      requires (\forall* int i; false ; (\forall* int j; 0 <= j && j < 1; Value(x)));
      // yet ensures something, should fail
      ensures Value(x);
      void m(){
      }
      /*[/end]*/
    }
  """

  vercors should verify using anyBackend in "another example with vacuously quantified read permission" pvl """
    class rewriterIssue {
      int x;

      /*[/expect postFailed:perm]*/
      // assumes nothing
      requires (\forall* int i; false ; Value(x));
      // yet ensures something
      ensures Value(x);
      void m(boolean y){
      }
      /*[/end]*/
    }
  """

  vercors should verify using anyBackend in "example showing negative scale of negative permission" pvl """
    class rewriterIssue {
      int x;

      /*[/expect postFailed:perm]*/

      // assumes nothing
      requires (\forall* int i; 0 <= i && i < -5 ; Perm(x,1/-5));
      // yet ensures something
      ensures Perm(x,1);
      void m(boolean y){
      }

      /*[/end]*/
    }
  """
//  https://github.com/utwente-fmt/vercors/issues/815
//  vercors should verify using anyBackend in "example with vacuously quantified permission" pvl """
//    class rewriterIssue {
//      int x;
//
//      /*[/expect postFailed:perm]*/
//      // assume sanity of the array, but no permissions
//      requires ar !=null && ar.length > 1;
//      requires  (\forall* int i; 0 <= i && i < -1;
//           (\forall* int j;0 <= j && j < -1;
//             (\forall* int k;0 <= k && k < 1;
//               Perm(ar[k * ( -1 * -1 ) + ( j * -1 + i) ], 1) )));
//      // ensure a permission
//      ensures  Perm(ar[0],1);
//      // yet it passes
//      void m(int i,int[] ar){
//      }
//      /*[/end]*/
//    }
//  """

  vercors should verify using anyBackend example "technical/keywords/allowed-c.c"
  vercors should verify using anyBackend example "technical/keywords/allowed-java.java"
  vercors should error withCode "parseError" example "technical/keywords/disallowed-c-inline.c"
  vercors should error withCode "parseError" example "technical/keywords/disallowed-java-assert.java"

  vercors should verify using silicon example "technical/array-item-access.pvl"
  vercors should error withCode "parseError" in "example attaching contract to wrong constructor" java """
    class C {
      void m() {
        //@ loop_invariant true;
        l: if (true) { }
      }
    }
  """

  vercors should verify using anyBackend in "example assigning null to array type" java """
    public class BasicArray {
      public void test() {
        int[] a = null;
      }
    }
  """

  vercors should verify using anyBackend in "example with subscript after parentheses" pvl """
    requires 0 < |xs|;
    void test(seq<int> xs, seq<int> ys) {
        assert xs[0] == (xs + ys)[0];
        assert xs[0] == \old(xs)[0];
    }
  """

  vercors should verify using anyBackend in "example with function return class, implemented with = null" pvl """
    class Test {}
    pure Test f() = null;
  """

  vercors should verify using anyBackend in "example with given within parallel region" pvl """
    given frac p;
    void call();

    void test() {
      par T0 {
        call() given { p = 1 };
      }
    }
  """

  vercors should verify using anyBackend in "example showing invocations in conditions are ok" pvl """
    int bar() { return 0; }
    int foo() { return 0; }

    void act(int i) {
        return;
    }

    void test() {
      if(bar() == bar()) {
          act(0);
      } else if(bar() == foo()) {
          act(1);
      } else if(foo() == bar()) {
          act(2);
      } else {
          act(3);
      }
    }
  """

  vercors should verify using anyBackend in "example with return containing a non-direct invocation" pvl """
    boolean isBlack() { return true; }
    boolean isRed() { return !isBlack(); }
  """

  vercors should verify using anyBackend in "example with implicit Exception present" java
    "class Test { void foo() { new Exception(); } }"
  vercors should verify using anyBackend in "example with implicit RuntimeException present" java
    "class Test { void foo() { new RuntimeException(); } }"
  vercors should verify using anyBackend in "example containing an empty class" java
    "class OnlyClass {}"
  vercors should verify using anyBackend in "example containing an empty class extending something" java
    "class OnlyClass extends Object {}"
  vercors should verify using anyBackend in "example asserting this instanceof the defining class" java """
    class MyClass {
      void foo() {
        MyClass myClass = new MyClass();
        assert myClass instanceof MyClass;
      }
    }
  """
  vercors should verify using anyBackend in "example showing \\pointer iff forall i \\pointer_index" c """
    //@ context \pointer(ar, len, write);
    void test(int ar[], int len) {
        for(int i = 0; i < len; i++)
        //@ context \pointer_index(ar, i, write);
        {}
    }
  """
  vercors should error withCode "parseError" in "example with preceding garbage" pvl "} class Test{}"
  vercors should error withCode "parseError" in "example with trailing garbage" pvl "class Test{} }"
  vercors should verify using anyBackend in "example returning null for class type" java """
    public class List {
      public List nothing() {
        return null;
      }
    }
  """
  vercors should verify using silicon example "technical/satcheck-check.pvl"
  vercors should verify using silicon in "example that shows 2*read does not imply disjointness" pvl """
    class Test { int x; }

    requires Value(left.x) ** Value(right.x);
    void test(Test left, Test right) {
      if(left == right) {
        /*[/expect assertFailed:false]*/
        assert false;
        /*[/end]*/
      }
    }
  """
  vercors should verify using silicon in "example that shows 2*read does not imply disjointness, even with a nested field" pvl """
    class Test { Test t; }

    requires Value(left.t) ** Value(right.t);
    requires Value(left.t.t) ** Value(right.t.t);
    void test(Test left, Test right) {
      if(left == right) {
        /*[/expect assertFailed:false]*/
        assert false;
        /*[/end]*/
      }
    }
  """
  vercors should verify using silicon example "technical/TestFuturePermsFail.pvl"
  vercors should verify using silicon example "technical/TestFuturePermsPass.pvl"

  vercors should error withCode "resolutionError" in "example writing to variable within par-block" pvl """
    void test() {
      int x = 0;
      par {
        x = 1;
      }
    }
  """

  vercors should verify using silicon in "example that requires correct associativity of scale" pvl """
    resource p();

    requires [1\2]p() ** [1\2]p();
    void test() {
      assert perm(p()) == write;
    }
  """

  vercors should verify using anyBackend in "example that shows given parameters clobber fields" java """
    public class A {
      int i;

      public void main() {
        B b = new B();

        b.testA()/*@ given { a = 9 } @*/;
        b.testI()/*@ given { i = 9 } @*/;
      }
    }

    class B {
      /*@
        given int a;
      @*/
      public void testA() {
      }

      /*@
        given int i;
      @*/
      public void testI() {
      }
    }
  """

  vercors should verify using anyBackend in "example that shows qualified names are equal types to their unqualified equivalent" java """
    class QualifiedNames {
      void foo() {
          Exception e = new java.lang.Exception();
      }

      void bar () {
          java.lang.Exception e = new Exception();
      }
    }
  """

  vercors should verify using anyBackend in "assigning results from impure method calls to final fields should preserve information" java
    """
      class C {
          final Integer x;
          //@ ensures this.x != null && this.x.intValue() == 0;
          C() {
              this.x = new Integer(0);
          }
      }
    """

  vercors should verify using silicon in "example using adts" java
    """
    /*@ adt MyADT {
      pure boolean f();
      axiom f();
    }

    @*/

    class C {
      void m() {
          //@ assert MyADT.f();
      }
    }
    """

  vercors should verify using silicon in "example using string primitive" pvl
    """
    void g() {
        "xuz";
        assert "abc" == "abc";
        assert "abc" != "xyz";

        string xxx;
        string s1 = "aaa";
        string s2 = "bbb";
        string s3 = s1 + s2;
    }
    """

  vercors should verify using silicon in "example using plus operator overloading" pvl
    """
    class C {
      int x;

      ensures Perm(x, 1) ** x == v;
      constructor(int v) {
        x = v;
      }

      context Perm(x, 1\2) ** Perm(other.x, 1\2);
      ensures Perm(\result.x, 1) ** \result.x == x + other.x;
      C +(C other) {
        return new C(x + other.x);
      }
    }

    void m() {
      assert (new C(1) + new C(2)).x == 3;
    }
    """
}
