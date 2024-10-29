package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class ChooseSpec extends VercorsSpec {
  vercors should verify using silicon in "example using deterministic choice" pvl """
    void test() {
      assert \choose({0..5}) < 5;
      assert \choose({0..5}) == \choose({0..5});
    }

    void that() {
      /*[/expect assertFailed:false]*/
      assert \choose({0..5}) == 0;
      /*[/end]*/
    }

    void notThat() {
      /*[/expect assertFailed:false]*/
      assert \choose({0..5}) != 0;
      /*[/end]*/
    }
  """

  vercors should verify using silicon in "example using non-deterministic choice" pvl """
    void test() {
      assert \choose_fresh({0..5}) < 5;
      /*[/expect assertFailed:false]*/
      assert \choose_fresh({0..5}) == \choose_fresh({0..5});
      /*[/end]*/
    }
  """
}
