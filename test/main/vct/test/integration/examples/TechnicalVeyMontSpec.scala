package vct.test.integration.examples

import hre.io.LiteralReadable
import vct.main.modes.Verify
import vct.options.Options
import vct.options.types.Verbosity
import vct.test.integration.helper.VercorsSpec

class TechnicalVeyMontSpec extends VercorsSpec {
  (vercors
    should verify
    using silicon
    flag "--veymont-generate-permissions"
    in "example using communicate"
    pvl
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
    """)

  (vercors
    should fail
    withCode "assertFailed:false"
    using silicon
    flag "--veymont-generate-permissions"
    in "plain endpoint field dereference should be possible"
    pvl
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
    """)

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

  (vercors
    should verify
    using silicon
    flag "--veymont-generate-permissions"
    in "Endpoint fields should be assignable"
    pvl
    """
    class Storage { int x; int y; }
    seq_program Example() {
      endpoint alice = Storage();

      seq_run {
        alice.x := alice.y;
      }
    }
    """)

  vercors should error withCode "resolutionError:seqProgInstanceMethodArgs" in "instance method in seq_program cannot have arguments" pvl
  """
  seq_program Example() {
    void m(int x) { }

    seq_run { }
  }
  """

  vercors should error withCode "resolutionError:seqProgInstanceMethodBody" in "instance method in seq_program must have a body" pvl
  """
  seq_program Example() {
    void m();

    seq_run { }
  }
  """

  vercors should error withCode "resolutionError:seqProgInstanceMethodNonVoid" in "instance method in seq_program must have void return type" pvl
  """
  seq_program Example() {
    int m() { }

    seq_run { }
  }
  """

  vercors should error withCode "resolutionError:seqProgStatement" in "seq_prog excludes certain statements" pvl
  """
  class C { }
  seq_program Example(C c) {
    seq_run {
      lock c;
    }
  }
  """

  vercors should error withCode "resolutionError:seqProgReceivingEndpoint" in "Dereferencing anything other than the receiving endpoint in the arguments of a endpoint method invocation is not supported yet" pvl
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

  vercors should error withCode "resolutionError:seqProgInvocation" in "Only method calls on endpoints or seq_program are allowed within seq_program" pvl
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

  vercors should error withCode "resolutionError:type" in "Assign must be well-typed" pvl
  """
  class C { int x; }
  seq_program C() {
    endpoint charlie = C();
    seq_run {
      charlie.x := true;
    }
  }
  """

  vercors should error withCode "resolutionError:type" in "Communicating parties must agree on the type" pvl
  """
  class C { int c; }
  class A { bool a; }
  seq_program C() {
    endpoint alice = A();
    endpoint charlie = C();
    seq_run {
      communicate charlie.c <- alice.a;
    }
  }
  """

  (vercors
    should verify
    using silicon
    flag "--veymont-generate-permissions"
    in "assignment should work"
    pvl
    """
    class Storage {
      int x;

      ensures x == 0;
      constructor() {
        x = 0;
      }
    }
    seq_program Example() {
       endpoint alice = Storage();

       requires alice.x == 0;
       ensures alice.x == 0;
       seq_run {
         assert alice.x == 0;
       }
    }
    """)

  // TODO: Eventually should be postconditionFailed if the assignment statement works succesfully
  (vercors
    should fail
    withCode "postconditionFailed?"
    using silicon
    flag "--veymont-generate-permissions"
    in "assigning should change state"
    pvl
    """
    class Storage {
       int x;

       ensures x == v;
       constructor(int v) {
         x = v;
       }
    }
    seq_program Example() {
       endpoint alice = Storage(0);

       requires alice.x == 0;
       ensures alice.x == 0;
       seq_run {
         alice.x := 1;
       }
    }
    """)

  vercors should error withCode "resolutionError:seqProgReceivingEndpoint" in "Assignment statement only allows one endpoint in the assigned expression" pvl
  """
  class Storage {
     int x;
  }
  seq_program Example() {
     endpoint alice = Storage();
     endpoint bob = Storage();

     seq_run {
       alice.x := bob.x;
     }
  }
  """

  (vercors
    should fail
    withCode "branchNotUnanimous"
    using silicon
    flag "--veymont-generate-permissions"
    in "Parts of condition in branch have to agree inside seqprog"
    pvl
    """
    class Storage {
       int x;
    }
    seq_program Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       seq_run {
          if (alice.x == 0 && bob.x == 0) {
            // Alice might go here, bob might not: error
          }
       }
    }
    """)

  (vercors
    should fail
    withCode "branchNotUnanimous"
    using silicon
    flag "--veymont-generate-permissions"
    in "Parts of condition in branch have to agree inside seqprog, including conditions for all endpoints"
    pvl
    """
    class Storage {
       int x;
    }

    pure int f() = 3;

    seq_program Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       seq_run {
          if (alice.x == 0 && f() == 3) {
            // Alice might go here, bob will definitely, because of the second expression: error
          }
       }
    }
    """)

  (vercors
    should verify
    using silicon
    flag "--veymont-generate-permissions"
    in "If there is only one endpoint, the conditions don't have to agree, as there is only one endpoint"
    pvl
    """
    class Storage {
       int x;
    }

    pure int f() = 3;

    seq_program Example() {
       endpoint alice = Storage();

       seq_run {
          if (alice.x == 0 && f() == 3) {
            // Alice might go here, bob will definitely, because of the second expression: error
          }
       }
    }
    """)

  vercors should error withCode "seqProgParticipantErrors" in "`if` cannot depend on bob, inside an `if` depending on alice" pvl
  """
  class Storage {
    int x;
  }
  seq_program Example() {
     endpoint alice = Storage();
     endpoint bob = Storage();

     seq_run {
        if (alice.x == 0) {
          if (bob.x == 0) {
            // Error
          }
        }
     }
  }
  """

  vercors should error withCode "seqProgParticipantErrors" in "If alice branches, bob cannot communicate" pvl
  """
  class Storage {
    int x;
  }
  seq_program Example() {
     endpoint alice = Storage();
     endpoint bob = Storage();

     seq_run {
        if (alice.x == 0) {
          communicate alice.x <- bob.x;
        }
     }
  }
  """

  vercors should error withCode "seqProgParticipantErrors" in "If alice branches, bob cannot assign" pvl
  """
  class Storage {
    int x;
  }
  seq_program Example() {
     endpoint alice = Storage();
     endpoint bob = Storage();

     seq_run {
        if (alice.x == 0) {
          bob.x := 3;
        }
     }
  }
  """

  (vercors
    should verify
    using silicon
    flag "--veymont-generate-permissions"
    in "Programs where branch conditions agree should verify"
    pvl
    """
    class Storage {
       bool x;
    }
    seq_program Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       seq_run {
         alice.x := true;
         bob.x := true;
         while (alice.x && bob.x) {
           bob.x := false;
           communicate alice.x <- bob.x;
         }
       }
    }
    """)

  (vercors
    should fail
    withCode "loopUnanimityNotEstablished"
    using silicon
    flag "--veymont-generate-permissions"
    in "Programs where branch condition unanimity cannot be established should fail"
    pvl
    """
    class Storage {
       bool x;
    }
    seq_program Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       seq_run {
         while (alice.x && bob.x) {

         }
       }
    }
    """)

  (vercors
    should fail
    withCode "loopUnanimityNotMaintained"
    using silicon
    flag "--veymont-generate-permissions"
    in "Programs where branch condition unanimity cannot be maintained should fail"
    pvl
    """
    class Storage {
       bool x;
    }
    seq_program Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       seq_run {
         alice.x := true;
         bob.x := true;
         while (alice.x && bob.x) {
           alice.x := false;
         }
       }
    }
    """)

  vercors should error withCode "seqProgParticipantErrors" in "Loops should also limit the number of participants" pvl
  """
  class Storage {
     bool x;
  }
  seq_program Example() {
     endpoint alice = Storage();
     endpoint bob = Storage();
     endpoint charlie = Storage();

     seq_run {
       alice.x := true;
       bob.x := true;
       while (alice.x && bob.x) {
         alice.x := false;
         charlie.x := true;
       }
     }
  }
  """

  (vercors
    should verify
    using silicon
    flag "--veymont-generate-permissions"
    in "Loops should also limit the number of participants when combined with branches"
    pvl
    """
    class Storage {
       bool x;
    }
    seq_program Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();
       endpoint charlie = Storage();

       seq_run {
         alice.x := true;
         bob.x := true;
         while (alice.x && bob.x) {
           alice.x := false;
           if (bob.x == true) {
            bob.x := false;
           }
         }
       }
    }
    """)

  vercors should error withCode "seqProgParticipantErrors" in "Loops should also limit the number of participants when combined with branches" pvl
  """
  class Storage {
     bool x;
  }
  seq_program Example() {
     endpoint alice = Storage();
     endpoint bob = Storage();
     endpoint charlie = Storage();

     seq_run {
       alice.x := true;
       bob.x := true;
       while (alice.x && bob.x) {
         alice.x := false;
         if (bob.x == true) {
          bob.x := false;
          charlie.x := true;
         }
       }
     }
  }
  """

  (vercors should verify
    using silicon
    flag "--veymont-generate-permissions"
    in "Permission should be generated for constructors as well" pvl
    """
    class Storage {
      int x;

      ensures x == 2;
      int m() {
        x = 2;
      }
    }

    seq_program Example() {
      endpoint alice = Storage();
      seq_run {
        alice.m();
        assert alice.x == 2;
      }
    }
    """)

  (vercors
    should fail
    withCode "accessPerm"
    using silicon
    in "When no permission is generated, a failure should occur on endpoint field access"
    pvl
    """
    class Storage {
      int x;
    }

    seq_program Example() {
      endpoint alice = Storage();
      endpoint bob = Storage();
      seq_run {
        communicate alice.x <- bob.x;
      }
    }
    """)

  (vercors
    should fail
    withCode "seqAssignPerm"
    using silicon
    in "When no permission is generated, a failure should occur on seq assign field access"
    pvl
    """
    class Storage {
      int x;
    }

    seq_program Example() {
      endpoint alice = Storage();
      seq_run {
        alice.x := 3;
      }
    }
    """)
}
