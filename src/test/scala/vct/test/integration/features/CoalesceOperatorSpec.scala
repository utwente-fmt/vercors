package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class CoalesceOperatorSpec extends VercorsSpec {
  vercors should verify using anyBackend in "Testing that the coalesce operator ensures the object is not null" pvl
    """
      class Test {
        resource P();

        requires a?.P();
        void test(Test a) {
          if (a == null)
          {
            assert perm(a.P()) == none;
          }
          else {
            assert perm(a.P()) == write;
          }
        }
      }
    """

  vercors should verify using anyBackend in "Testing failure when using coalesce operator" pvl
    """
      class Test {
        resource P();

        requires a?.P();
        void test(Test a) {
          /*[/expect assertFailed:perm]*/
          assert a.P();
          /*[/end]*/
        }
      }
    """
}
