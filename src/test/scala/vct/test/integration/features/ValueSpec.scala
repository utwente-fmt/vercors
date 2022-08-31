package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class ValueSpec extends VercorsSpec {

  vercors should verify using anyBackend in "Example using Value for read permission" pvl
    """
      class Test {

        int x;

        requires Perm(x, read);
        ensures Value(x);
        void test1(int t) {

        }

        requires Value(x);
        ensures Perm(x, read);
        void test2() {

        }

        resource a1();

        requires Value(a1());
        ensures Perm(a1(), read);
        void test3() {

        }

      }
    """

}
