package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalVeyMontSpec extends VercorsSpec {
  vercors should error withCode "communicateNotSupported" in "example using communicate" pvl
  """
     class Storage {
        int x;
     }
     seq_program Example() {
        endpoint alice = Storage();
        endpoint bob = Storage();

        seq_run {
          communicate alice.x <- bob.x;
          communicate bob.x -> alice.x;
          // assert alice.x == bob.x; // To be enabled when endpoint field dereference is implemented
        }
     }
  """

  vercors should error withCode "endpointUseNotSupported" in "plain endpoint field dereference should be possible" pvl
  """
     class Storage {
        int x;
     }
     seq_program Example() {
        endpoint alice = Storage();

        seq_run {
          assert alice.x == 0;
        }
     }
  """

  vercors should error withCode "noSuchName" in "non-existent thread name in communicate fails" pvl
  """
  seq_program Example() {
     seq_run {
       communicate charlie.x <- charlie.x;
     }
  }
  """

  vercors should error withCode "noSuchName" in "non-existent field in communicate fails" pvl
  """
  class Storage { int x; }
  seq_program Example() {
     endpoint charlie = Storage();
     seq_run {
       communicate charlie.nonExistent <- charlie.nonExistent;
     }
  }
  """

  vercors should error withCode "parseError" in "parameterized sends not yet supported " pvl
  """
    class Storage { int x; }
    seq_program Example() {
      endpoint alice[10] = Storage();
      endpoint bob[10] = Storage();
      seq_run {
        communicate alice[i: 0 .. 9].x <- bob[i + 1].y;
      }
    }
  """

  vercors should error withCode "noRunMethod" in "run method should always be present" pvl
  """
  seq_program Example() { }
  """

  vercors should error withCode "parseError" in "endpoints can only have class types" pvl
  """
  seq_program Example() {
    endpoint alice = int();
  }
  """

  vercors should error withCode "endpointUseNotSupported" in "Endpoint fields should be assignable" pvl
  """
  class Storage { int x; int y; }
  seq_program Example() {
    endpoint alice = Storage();

    seq_run {
      alice.x := alice.y;
    }
  }
  """
}
