package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class MagicWandSpec extends VercorsSpec {
  vercors should verify using anyBackend in "Testing the magic wand feature" pvl
    """
      void test() {
        Test obj = new Test();
        package Perm(obj.f, write) -* Perm(obj.f, write) {}
        assert Perm(obj.f, write);
      }

      class Test {
        int f;
      }
    """

  vercors should verify using anyBackend in "Testing the magic wand feature with footprint" pvl
    """
      void test() {
        Test obj = new Test();
        Test obj2 = new Test();
        package Perm(obj.f, write) -* Perm(obj2.f, write) ** Perm(obj.f, write) {}
        /*[/expect assertFailed:perm]*/
        assert Perm(obj2.f, write);
        /*[/end]*/
      }

      class Test {
        int f;
      }
    """

  vercors should verify using anyBackend in "Testing the magic wand feature with oversight footprint" pvl
    """
      void test() {
        Test obj = new Test();
        Test obj2;
        /*[/expect packageFailed:perm]*/
        package Perm(obj.f, write) -* Perm(obj2.f, write) ** Perm(obj.f, write) {}
        /*[/end]*/
      }

      class Test {
        int f;
      }
    """

  vercors should verify using anyBackend in "Testing you can't throw errors in package block" java
    """
      class Test {

        void test() {
          /*[/expect packageThrows]*/
          /*@ package true -* true
          {
            throw new Exception();
          } @*/
        }
        /*[/end]*/
        int f;
      }
    """

  vercors should verify using anyBackend in "Testing that when applied a wand is exhaustively consumed" pvl
    """
      void test () {
        package true -* true {}
        apply true -* true;
        /*[/expect applyFailed:perm]*/
        apply true -* true;
        /*[/end]*/
      }
    """

  vercors should verify using silicon in "Testing that conditional footprint works" pvl
    """
     void test() {
      boolean b;
      Test a = new Test();
      package (b ==> Perm(a.a, write)) -* Perm(a.a, write) {}
      assert b ==> Perm(a.a, write);
      assert !b ==> (perm(a.a) == none);
     }

     class Test {
      boolean a;
     }
    """
}
