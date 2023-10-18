package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalVeymontSpec extends VercorsSpec {
  vercors should error withCode "communicateNotSupported" in "example using communicate" pvl
  """
     class Storage {
        int x;
     }
     seq_program Example() {
        thread alice = Storage();
        thread bob = Storage();

        run {
          communicate alice.x <- bob.x;
          communicate bob.x -> alice.x;
          assert alice.x == bob.x;
        }
     }
  """

  vercors should error withCode "parseError" in "parameterized sends not yet supported " pvl
  """
    seq_program Example() {
      thread alice[10] = Storage();
      thread bob[10] = Storage();
      run {
        communicate alice[i: 0 .. 9].x <- bob[i + 1].y;
      }
    }
  """
}
