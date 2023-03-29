package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class CContracts extends VercorsSpec {
  vercors should verify using anyBackend in "example with a forward declaration" c """
    int f();

    int f() {
      return 5;
    }
  """

  vercors should verify using anyBackend in "example with a contracted forward declaration" c """
    //@ ensures \result == 5;
    int f();

    int f() {
      return 5;
    }
  """

  vercors should verify using anyBackend in "example failing on the contract at a forward declaration" c """
    //@ ensures \result == 5;
    int f();

    /*[/expect postFailed:false]*/
    int f() {
      return 4;
    }
    /*[/end]*/
  """

  vercors should verify using anyBackend in "example with a contracted definition" c """
    int f();

    //@ ensures \result == 5;
    int f() {
      return 5;
    }
  """

  vercors should error withCode "multipleContracts" in "example with duplicate contract on function definition" c """
    //@ requires 1 == 2;
    int f();

    //@ ensures \result == 5;
    int f() {
      return 5;
    }
  """
}
