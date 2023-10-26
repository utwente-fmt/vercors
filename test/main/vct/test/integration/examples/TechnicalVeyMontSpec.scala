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

        run {
          communicate alice.x <- bob.x;
          communicate bob.x -> alice.x;
          // assert alice.x == bob.x; // To be enabled when endpoint field dereference is implemented
        }
     }
  """

  // To be enabled when endpoint field dereference is implemented
  // vercors should verify in "plain endpoint field dereference should be possible" pvl
  // """
  //    class Storage {
  //       int x;
  //    }
  //    seq_program Example() {
  //       endpoint alice = Storage();
  //       endpoint bob = Storage();

  //       run {
  //         assert alice.x == bob.x;
  //       }
  //    }
  // """

  vercors should error withCode "noSuchName" in "non-existent thread name in communicate fails" pvl
  """
  seq_program Example() {
     run {
       communicate charlie.x <- charlie.x;
     }
  }
  """

  vercors should error withCode "noSuchName" in "non-existent field in communicate fails" pvl
  """
  class Storage { int x; }
  seq_program Example() {
     endpoint charlie = Storage();
     run {
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
      run {
        communicate alice[i: 0 .. 9].x <- bob[i + 1].y;
      }
    }
  """

  vercors should error withCode "noRunMethod" in "run method should always be present" pvl
  """
  seq_program Example() { }
  """

  vercors should error withCode "forbiddenEndpointType" in "endpoints can only have class types" pvl
  """
  seq_program Example() {
    endpoint alice = int();
    run {

    }
  }
  """
}
