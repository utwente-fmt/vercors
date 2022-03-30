package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class ArrayValuesSpec extends VercorsSpec {
  vercors should verify using silicon in "example using \\values" pvl """
    requires array != null;
    requires (\forall* int i = 0 .. array.length; Perm(array[i], 1\2));
    requires \values(array, 0, array.length) == [1, 2, 3];
    void test(int[] array) {

    }
  """

  vercors should verify using silicon in "java example using \\values" java """
    class Test {
      /*@
      requires array != null;
      requires (\forall* int i = 0 .. array.length; Perm(array[i], 1\2));
      requires \values(array, 0, array.length) == [1, 2, 3];
      @*/
      void test(int[] array) {
      }
    }
  """
}
