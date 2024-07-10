package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalVeyMontSpec
    extends VercorsSpec {

      (vercors should verify using silicon flag
    "--veymont-generate-permissions" in "example using communicate" pvl """
       class Storage {
          int x;
       }
       choreography Example() {
          endpoint alice = Storage();
          endpoint bob = Storage();

          ensures (\chor alice.x == bob.x);
          run {
            channel_invariant (\chor \msg == bob.x);
            communicate alice.x <- bob.x;
            channel_invariant (\chor \msg == bob.x);
            communicate bob.x -> alice.x;
          }
       }
    """)

  (vercors should fail withCode "assertFailed:false" using silicon flag
    "--veymont-generate-permissions" in
    "plain endpoint field dereference should be possible" pvl """
       class Storage {
          int x;
       }
       choreography Example() {
          endpoint alice = Storage();

          run {
            assert alice.x == 0;
          }
       }
    """)

  vercors should error withCode "noSuchName" in
    "non-existent thread name in communicate fails" pvl """
  choreography Example() {
     run {
       communicate charlie.x <- charlie.x;
     }
  }
  """

  vercors should error withCode "noSuchName" in
    "non-existent field in communicate fails" pvl """
  class Storage { int x; }
  choreography Example() {
     endpoint charlie = Storage();
     run {
       communicate charlie.nonExistent <- charlie.nonExistent;
     }
  }
  """

  vercors should error withCode "parseError" in
    "parameterized sends not yet supported " pvl """
    class Storage { int x; }
    choreography Example() {
      endpoint alice[10] = Storage();
      endpoint bob[10] = Storage();
      run {
        communicate alice[i: 0 .. 9].x <- bob[i + 1].y;
      }
    }
  """

  vercors should error withCode "noRunMethod" in
    "run method should always be present" pvl """
  choreography Example() { }
  """

  vercors should error withCode "parseError" in
    "endpoints can only have class types" pvl """
  choreography Example() {
    endpoint alice = int();
  }
  """

  (vercors should verify using silicon flag "--veymont-generate-permissions" in
    "Endpoint fields should be assignable" pvl """
    class Storage { int x; int y; }
    choreography Example() {
      endpoint alice = Storage();

      run {
        alice.x := alice.y;
      }
    }
    """)

  vercors should error withCode "resolutionError:seqProgInstanceMethodArgs" in
    "instance method in choreography cannot have arguments" pvl """
  choreography Example() {
    void m(int x) { }

    run { }
  }
  """

  vercors should error withCode "resolutionError:seqProgInstanceMethodBody" in
    "instance method in choreography must have a body" pvl """
  choreography Example() {
    void m();

    run { }
  }
  """

  vercors should error withCode
    "resolutionError:seqProgInstanceMethodNonVoid" in
    "instance method in choreography must have void return type" pvl """
  choreography Example() {
    int m() { }

    run { }
  }
  """

  vercors should error withCode "resolutionError:chorStatement" in
    "`choreography` excludes certain statements" pvl """
  class C { }
  choreography Example(C c) {
    run {
      lock c;
    }
  }
  """

  vercors should verify using silicon in
    "Dereferencing other endpoints in arguments is possible if permissions are available for the endpoint context" pvl
    """
    class C { C d; void foo(int x); int x; }
    choreography Example(C c) {
      endpoint c = C();
      endpoint d = C();
      requires Perm[c](d.x, 1);
      run {
        c.foo(d.x);
      }
    }
    """

  vercors should fail withCode "perm" using silicon in
    "Dereferencing other endpoints in arguments is not possible if permissions are not available for the endpoint context" pvl
    """
    class C { C d; void foo(int x); int x; }
    choreography Example(C c) {
      endpoint c = C();
      endpoint d = C();
      requires Perm[d](d.x, 1);
      run {
        c.foo(d.x);
      }
    }
    """

  vercors should verify using silicon in "Empty choreography must verify" pvl
    """
  choreography C() {
    run {

    }
  }
  """

  vercors should error withCode "resolutionError:type" in
    "Assign must be well-typed" pvl """
  class C { int x; }
  choreography C() {
    endpoint charlie = C();
    run {
      charlie.x := true;
    }
  }
  """

  vercors should error withCode "resolutionError:type" in
    "Communicating parties must agree on the type" pvl """
  class C { int c; }
  class A { bool a; }
  choreography C() {
    endpoint alice = A();
    endpoint charlie = C();
    run {
      communicate charlie.c <- alice.a;
    }
  }
  """

  (vercors should verify using silicon flag "--veymont-generate-permissions" in
    "assignment should work" pvl """
    class Storage {
      int x;

      ensures x == 0;
      constructor() {
        x = 0;
      }
    }
    choreography Example() {
       endpoint alice = Storage();

       requires alice.x == 0;
       ensures alice.x == 0;
       run {
         assert alice.x == 0;
       }
    }
    """)

  (vercors should fail withCode "postFailed:false" using silicon flag
    "--veymont-generate-permissions" in "Postcondition of run can fail" pvl """
    class Storage {
       int x;
    }
    choreography Example() {
       endpoint alice = Storage();

       ensures alice.x == 0;
       run {
       }
    }
    """)

  (vercors should fail withCode "postFailed:false" using silicon flag
    "--veymont-generate-permissions" in
    "Postcondition of choreography can fail" pvl """
    class Storage {
       int x;
    }

    ensures 1 == 0;
    choreography Example() {
       endpoint alice = Storage();

       run {
       }
    }
    """)

  vercors should fail withCode "perm" using silicon flag
    "--veymont-generate-permissions" in
    "Assignment statement only allows one endpoint in the assigned expression" pvl
    """
    class Storage {
       int x;
    }
    choreography Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       run {
         alice.x := bob.x;
       }
    }
    """

  (vercors should fail withCode "branchNotUnanimous" using silicon flag
    "--veymont-generate-permissions" in
    "Parts of condition in branch have to agree inside seqprog" pvl """
    class Storage {
       int x;
    }
    choreography Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       run {
          if (alice.x == 0 && bob.x == 0) {
            // Alice might go here, bob might not: error
          }
       }
    }
    """)

  (vercors should fail withCode "branchNotUnanimous" using silicon flag
    "--veymont-generate-permissions" in
    "Parts of condition in branch have to agree inside seqprog, including conditions for all endpoints" pvl
    """
    class Storage {
       int x;
    }

    pure int f() = 3;

    choreography Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       run {
          if (alice.x == 0 && f() == 3) {
            // Alice might go here, bob will definitely, because of the second expression: error
          }
       }
    }
    """)

  (vercors should verify using silicon flag "--veymont-generate-permissions" in
    "If there is only one endpoint, the conditions don't have to agree, as there is only one endpoint" pvl
    """
    class Storage {
       int x;
    }

    pure int f() = 3;

    choreography Example() {
       endpoint alice = Storage();

       run {
          if (alice.x == 0 && f() == 3) {
            // Alice might go here, bob will definitely, because of the second expression: error
          }
       }
    }
    """)

  vercors should error withCode "seqProgParticipantErrors" in
    "`if` cannot depend on bob, inside an `if` depending on alice" pvl """
  class Storage {
    int x;
  }
  choreography Example() {
     endpoint alice = Storage();
     endpoint bob = Storage();

     run {
        if (alice.x == 0) {
          if (bob.x == 0) {
            // Error
          }
        }
     }
  }
  """

  vercors should error withCode "seqProgParticipantErrors" in
    "If alice branches, bob cannot communicate" pvl """
  class Storage {
    int x;
  }
  choreography Example() {
     endpoint alice = Storage();
     endpoint bob = Storage();

     run {
        if (alice.x == 0) {
          communicate alice.x <- bob.x;
        }
     }
  }
  """

  vercors should error withCode "seqProgParticipantErrors" in
    "If alice branches, bob cannot assign" pvl """
   class Storage {
     int x;
   }
   choreography Example() {
      endpoint alice = Storage();
      endpoint bob = Storage();

      run {
         if (alice.x == 0) {
           bob.x := 3;
         }
      }
   }
   """

  (vercors should verify using silicon flag "--veymont-generate-permissions" in
    "Programs where branch conditions agree should verify" pvl """
    class Storage {
       bool x;
    }
    choreography Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       run {
         alice.x := true;
         bob.x := true;
         while (alice.x && bob.x) {
           bob.x := false;
           communicate alice.x <- bob.x;
         }
       }
    }
    """)

  (vercors should fail withCode "loopUnanimityNotEstablished" using silicon flag
    "--veymont-generate-permissions" in
    "Programs where branch condition unanimity cannot be established should fail" pvl
    """
    class Storage {
       bool x;
    }
    choreography Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       run {
         while (alice.x && bob.x) {

         }
       }
    }
    """)

  (vercors should fail withCode "loopUnanimityNotMaintained" using silicon flag
    "--veymont-generate-permissions" in
    "Programs where branch condition unanimity cannot be maintained should fail" pvl
    """
    class Storage {
       bool x;
    }
    choreography Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();

       run {
         alice.x := true;
         bob.x := true;
         while (alice.x && bob.x) {
           alice.x := false;
         }
       }
    }
    """)

  vercors should error withCode "seqProgParticipantErrors" in
    "Loops should also limit the number of participants" pvl """
   class Storage {
      bool x;
   }
   choreography Example() {
      endpoint alice = Storage();
      endpoint bob = Storage();
      endpoint charlie = Storage();

      run {
        alice.x := true;
        bob.x := true;
        while (alice.x && bob.x) {
          alice.x := false;
          charlie.x := true;
        }
      }
   }
   """

  (vercors should verify using silicon flag "--veymont-generate-permissions" in
    "Loops should also limit the number of participants when combined with branches" pvl
    """
    class Storage {
       bool x;
    }
    choreography Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();
       endpoint charlie = Storage();

       run {
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

  vercors should error withCode "seqProgParticipantErrors" in
    "Loops should also limit the number of participants when combined with branches, failing" pvl
    """
   class Storage {
      bool x;
   }
   choreography Example() {
      endpoint alice = Storage();
      endpoint bob = Storage();
      endpoint charlie = Storage();

      run {
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

  (vercors should verify using silicon flag "--veymont-generate-permissions" in
    "Permission should be generated for constructors as well" pvl """
    class Storage {
      int x;

      ensures x == 2;
      int m() {
        x = 2;
      }
    }

    choreography Example() {
      endpoint alice = Storage();
      run {
        alice.m();
        assert alice.x == 2;
      }
    }
    """)

  (vercors should fail withCode "perm" using silicon in
    "When no permission is generated, a failure should occur on endpoint field access" pvl
    """
     class Storage {
       int x;
     }

     choreography Example() {
       endpoint alice = Storage();
       endpoint bob = Storage();
       run {
         communicate alice.x <- bob.x;
       }
     }
     """)

  (vercors should fail withCode "perm" using silicon in
    "When no permission is generated, a failure should occur on seq assign field access" pvl
    """
    class Storage {
      int x;
    }

    choreography Example() {
      endpoint alice = Storage();
      run {
        alice.x := 3;
      }
    }
    """)

  (vercors should verify using silicon flag "--veymont-generate-permissions" in
    "Permissions are generated for loop invariants, procedures, functions, instance methods, instance functions" pvl
    """
    class Storage {
      int x;

      ensures x == \old(x);
      ensures \result == x;
      int imx() {
        return x;
      }

      pure int ifx() = x;

      ensures x == \old(x);
      void all() {
        assert imx() == ifx();
        assert ifx() == px(this);
        assert px(this) == fx(this);
      }
    }

    ensures s.x == \old(s.x);
    ensures \result == s.x;
    int px(Storage s) {
      return s.x;
    }

    ensures \result == s.x;
    pure int fx(Storage s) = s.x;

    choreography Example(int N) {
      endpoint alice = Storage();
      ensures alice.x == 10;
      run {
        alice.x := 0;
        loop_invariant 0 <= alice.x && alice.x <= 10;
        while(alice.x < 10) {
          alice.x := alice.x + 1;
        }

        alice.all();
      }
    }
    """)

  (vercors should verify using silicon flag "--veymont-generate-permissions" in
    "Permission generation should only generate permissions that are strictly necessary" pvl
    """
    class Storage {
      int x;
    }

    choreography Example() {
      endpoint alice = Storage();
      endpoint bob = Storage();
      run {
        alice.x := 0;
        bob.x := 3;
        loop_invariant 0 <= alice.x && alice.x <= 10;
        while(alice.x < 10) {
          alice.x := alice.x + 1;
        }
        assert bob.x == 3;
      }
    }
    """)

  // Auxiliary methods turned off indefinitely for now. Though can be implemented with just engineering work.
//  (vercors
//    should verify
//    using silicon
//    flag "--veymont-generate-permissions"
//    in "Calling auxiliary methods in seq_prog should be possible"
//    pvl
//    """
//    class Storage {
//      int x;
//    }
//
//    choreography Example(int N) {
//      endpoint alice = Storage();
//
//      ensures alice.x == \old(alice.x) + 1;
//      void step() {
//        alice.x := alice.x + 1;
//      }
//
//      ensures alice.x == \old(alice.x + 1);
//      run {
//        step();
//      }
//    }
//    """)

//  (vercors
//    should verify
//    using silicon
//    flag "--veymont-generate-permissions"
//    in "VeyMont should conservatively generate permissions for auxiliary methods"
//    pvl
//    """
//    class Storage {
//      int x;
//    }
//
//    choreography Example(int N) {
//      endpoint alice = Storage();
//      endpoint bob = Storage();
//
//      ensures alice.x == \old(alice.x) + 1;
//      void step() {
//        alice.x := alice.x + 1;
//      }
//
//      ensures alice.x == \old(alice.x + 1);
//      run {
//        bob.x := 3;
//        step();
//        assert bob.x == 3;
//      }
//    }
//    """)

//  (vercors
//    should fail
//    withCode "assertFailed:false"
//    using silicon
//    flag "--veymont-generate-permissions"
//    in "Permissions should be generated when an endpoint participates in an auxiliary method"
//    pvl
//    """
//    class Storage {
//      int x;
//    }
//
//    choreography Example(int N) {
//      endpoint alice = Storage();
//      endpoint bob = Storage();
//
//      ensures alice.x == \old(alice.x) + 1;
//      void step() {
//        bob.x := 0;
//        alice.x := alice.x + 1;
//      }
//
//      ensures alice.x == \old(alice.x + 1);
//      run {
//        bob.x := 3;
//        step();
//        assert bob.x == 3;
//      }
//    }
//    """)

  (vercors should fail withCode "chorRunPreFailed:false" using silicon in
    "Precondition of run should be checked" pvl """
    class C { }
    choreography Example(int x) {
      endpoint c = C();
      requires x == 0;
      run {
      }
    }
    """)

  (vercors should fail withCode "postFailed:false" using silicon in
    "Precondition of run is never witnessed if there are no endpoints" pvl """
    ensures x == 0;
    choreography Example(int x) {
      requires x == 0;
      ensures x == 0;
      run {
      }
    }
    """)

  (vercors should fail withCode "preFailed:false" using silicon in
    "Precondition of endpoint constructor should be checked" pvl """
    class Storage {
      requires x == 0;
      constructor (int x) { }
    }

    choreography Example() {
      endpoint alice = Storage(1);

      run {
      }
    }
    """)

  (vercors should error withCode "resolutionError:chorStatement" in
    "Assignment should not be allowed in choreographies" pvl """
    class Storage {
      int x;
    }

    choreography Example() {
      endpoint alice = Storage();
      run {
        alice.x = 0;
      }
    }
    """)

  (vercors should fail withCode "participantsNotDistinct" using silicon flag
    "--veymont-generate-permissions" in
    "Endpoints participating in a communicate should be distinct" pvl """
    class Storage {
      int x;
    }

    choreography Example() {
      endpoint alice = Storage();
      run {
        communicate alice.x <- alice.x;
      }
    }
    """)

  // Disabled indefinitely until EncodePermissionStratification.scala is refactored to fully support generics
//  vercors should verify using silicon flag
//    "--veymont-generate-permissions" example
//    "technical/veymont/genericEndpoints.pvl"

  (vercors should fail withCode "branchNotUnanimous" using silicon flag
    "--veymont-generate-permissions" in "branch unanimity for if" pvl """
       class Storage {
          int x;
       }
       choreography Example() {
          endpoint alice = Storage();
          endpoint bob = Storage();

          run {
            alice.x := 0;
            bob.x := 1;
            if (alice.x == 0 && bob.x == 0) {
              communicate alice.x -> bob.x;
            }
          }
       }
    """)

  vercors should fail withCode "perm" using silicon in
    "deref should be safe" pvl """
  class Storage { int x; int y; }

  choreography runPostFails() {
    endpoint alice = Storage();
    requires Perm(alice.x, 1);
    run {
      alice.x := alice.y;
    }
  }
  """

  vercors should fail withCode "perm" using silicon in
    "assigning to a deref should fail if there is no permission" pvl """
  class Storage { int x; int y; }
  choreography runPostFails() {
    endpoint alice = Storage();
    requires Perm(alice.x, 1);
    run {
      alice.x := alice.y;
    }
  }
  """

  vercors should verify using silicon in
    "assigning to a deref should succeed if there is permission" pvl """
  class Storage { int x; int y; }
  choreography runPostFails() {
    endpoint alice = Storage();
    requires Perm(alice.x, 1) ** Perm(alice.y, 1);
    run {
      alice.x := alice.y;
    }
  }
  """
}
