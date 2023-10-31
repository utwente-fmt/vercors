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
          assert alice.x == bob.x;
        }
     }
  """

  vercors should fail withCode "perm" using silicon in "plain endpoint field dereference should be possible" pvl
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

  vercors should error withCode "seqAssignNotSupported" in "Endpoint fields should be assignable" pvl
  """
  class Storage { int x; int y; }
  seq_program Example() {
    endpoint alice = Storage();

    seq_run {
      alice.x := alice.y;
    }
  }
  """

  vercors should error withCode "resolutionError" in "instance method in seq_program cannot have arguments" pvl
  """
  seq_program Example() {
    void m(int x) { }

    seq_run { }
  }
  """

  vercors should error withCode "resolutionError" in "instance method in seq_program must have a body" pvl
  """
  seq_program Example() {
    void m();

    seq_run { }
  }
  """

  vercors should error withCode "resolutionError" in "instance method in seq_program must have void return type" pvl
  """
  seq_program Example() {
    int m() { }

    seq_run { }
  }
  """

  vercors should error withCode "resolutionError" in "seq_prog excludes certain statements" pvl
  """
  class C { }
  seq_program Example(C c) {
    seq_run {
      lock c;
    }
  }
  """

  vercors should error withCode "resolutionError" in "Dereferencing anything other than the receiving endpoint in the arguments of a endpoint method invocation is not supported yet" pvl
  """
  class C { C d; void foo(int x); int x; }
  seq_program Example(C c) {
    endpoint c = C();
    endpoint d = C();
    seq_run {
      c.foo(d.x);
    }
  }
  """

  vercors should error withCode "resolutionError" in "Only method calls on endpoints or seq_program are allowed within seq_program" pvl
  """
  class C { C d; void foo(); }
  seq_program Example(C c) {
    endpoint c = C();
    seq_run {
      c.d.foo();
    }
  }
  """

  vercors should verify using silicon in "Empty seq_program must verify" pvl
  """
  seq_program C() {
    seq_run {

    }
  }
  """
}
