package vct.test.integration.examples.veymont

import vct.test.integration.helper.VeyMontSpec

class TechnicalVeyMontSpec extends VeyMontSpec {
  choreography(
    desc = "Plain assignment is allowed, but considered unsound",
    pvl = """
      class C { int x; }
      choreography Chor() {
        endpoint a = C();
        requires Perm(a.x, 1);
        run {
          a.x = 3;
          assert a.x == 3;
        }
      }
        """,
  )

  choreography(
    desc = "\\endpoint not allowed in \\chor",
    error = "choreography:resolutionError:endpointExprInChor",
    pvl = """
      class C {}
      choreography Chor() {
        endpoint a = C();
        requires (\chor (\endpoint a; true));
        run {

        }
      }
        """,
  )

  choreography(
    desc = "\\chor not allowed in \\endpoint",
    error = "choreography:resolutionError:chorInEndpointExpr",
    pvl = """
      class C {}
      choreography Chor() {
        endpoint a = C();
        requires (\endpoint a; (\chor true));
        run {

        }
      }
        """,
  )

  implementation(
    desc = "Run contract can depend on choreography contract",
    pvl = """
  class Storage {}

  requires x > 0;
  choreography Chor(int x) {
    endpoint alex = Storage();

    requires x > 0;
    run { }
  }""",
  )

  choreography(
    desc = "Bobby may receive permission for its target location",
    pvl = """
  class Storage { int x; int y; }
  choreography Chor() {
    endpoint alex = Storage();
    endpoint bobby = Storage();
    requires Perm(alex.x, 1) ** Perm[alex](bobby.x, 1);
    run {
      channel_invariant Perm(bobby.x, 1);
      communicate alex.x -> bobby.x;
    }
  }""",
    fail = "perm",
  )

  choreography(
    desc = "Bobby might not have permission to assign",
    fail = "perm",
    pvl = """
  class Storage { int x; int y; }
  choreography Chor() {
    endpoint alex = Storage();
    endpoint bobby = Storage();
    requires Perm(alex.x, 1);
    run {
      communicate alex.x -> bobby.x;
    }
  }
  """,
  )

  choreography(
    desc = "Channel invariant might not be established",
    fail = "channelInvariantNotEstablished:false",
    pvl = """
  class Storage { int x; int y; }
  choreography Chor() {
    endpoint alex = Storage();
    endpoint bobby = Storage();
    requires Perm(alex.x, 1) ** Perm(bobby.x, 1);
    run {
      channel_invariant \msg == 3;
      communicate alex.x -> bobby.x;
    }
  }
  """,
  )

  choreography(
    desc = "example using communicate",
    flag = "--generate-permissions",
    pvl = """
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
    """,
  )

  choreography(
    desc = "plain endpoint field dereference should be possible",
    flag = "--generate-permissions",
    fail = "assertFailed:false",
    pvl = """
       class Storage {
          int x;
       }
       choreography Example() {
          endpoint alice = Storage();

          run {
            assert alice.x == 0;
          }
       }
    """,
  )

  choreography(
    error = "choreography:noSuchName",
    desc = "non-existent thread name in communicate fails",
    pvl = """
  choreography Example() {
     run {
       communicate charlie.x <- charlie.x;
     }
  }
  """,
  )

  choreography(
    error = "choreography:noSuchName",
    desc = "non-existent field in communicate fails",
    pvl = """
  class Storage { int x; }
  choreography Example() {
     endpoint charlie = Storage();
     run {
       communicate charlie.nonExistent <- charlie.nonExistent;
     }
  }
  """,
  )

  choreography(
    error = "choreography:parseError",
    desc = "parameterized sends not yet supported",
    pvl = """
    class Storage { int x; }
    choreography Example() {
      endpoint alice[10] = Storage();
      endpoint bob[10] = Storage();
      run {
        communicate alice[i: 0 .. 9].x <- bob[i + 1].y;
      }
    }
  """,
  )

  choreography(
    error = "choreography:noRunMethod",
    desc = "run method should always be present",
    pvl = """
  choreography Example() { }
  """,
  )

  choreography(
    error = "choreography:parseError",
    desc = "endpoints can only have class types",
    pvl = """
  choreography Example() {
    endpoint alice = int();
  }
  """,
  )

  choreography(
    flag = "--generate-permissions",
    desc = "Endpoint fields should be assignable",
    pvl = """
    class Storage { int x; int y; }
    choreography Example() {
      endpoint alice = Storage();

      run {
        alice.x := alice.y;
      }
    }
    """,
  )

  choreography(
    error = "choreography:resolutionError:chorStatement",
    desc = "`choreography` excludes certain statements",
    pvl = """
  class C { }
  choreography Example(C c) {
    run {
      lock c;
    }
  }
  """,
  )

  choreography(
    desc =
      "Dereferencing other endpoints in arguments is possible if permissions are available for the endpoint context",
    pvl = """
    class C { C d; void foo(int x); int x; }
    choreography Example(C c) {
      endpoint c = C();
      endpoint d = C();
      requires Perm[c](d.x, 1);
      run {
        c.foo(d.x);
      }
    }
    """,
  )

  choreography(
    fail = "perm",
    desc =
      "Dereferencing other endpoints in arguments is not possible if permissions are not available for the endpoint context",
    pvl = """
    class C { C d; void foo(int x); int x; }
    choreography Example(C c) {
      endpoint c = C();
      endpoint d = C();
      requires Perm[d](d.x, 1);
      run {
        c.foo(d.x);
      }
    }
    """,
  )

  choreography(
    desc = "Empty choreography must verify",
    pvl = """
  choreography C() {
    run {

    }
  }
  """,
  )

  choreography(
    error = "choreography:resolutionError:type",
    desc = "Assign must be well-typed",
    pvl = """
  class C { int x; }
  choreography C() {
    endpoint charlie = C();
    run {
      charlie.x := true;
    }
  }
  """,
  )

  choreography(
    error = "choreography:resolutionError:type",
    desc = "Communicating parties must agree on the type",
    pvl = """
  class C { int c; }
  class A { bool a; }
  choreography C() {
    endpoint alice = A();
    endpoint charlie = C();
    run {
      communicate charlie.c <- alice.a;
    }
  }
  """,
  )

  choreography(
    flag = "--generate-permissions",
    desc = "assignment should work",
    pvl = """
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
    """,
  )

  choreography(
    fail = "postFailed:false",
    flag = "--generate-permissions",
    desc = "Postcondition of run can fail",
    pvl = """
    class Storage {
       int x;
    }
    choreography Example() {
       endpoint alice = Storage();

       ensures alice.x == 0;
       run {
       }
    }
    """,
  )

  choreography(
    fail = "postFailed:false",
    flag = "--generate-permissions",
    desc = "Postcondition of choreography can fail",
    pvl = """
    class Storage {
       int x;
    }

    ensures 1 == 0;
    choreography Example() {
       endpoint alice = Storage();

       run {
       }
    }
    """,
  )

  choreography(
    fail = "perm",
    flag = "--generate-permissions",
    desc =
      "Assignment statement only allows one endpoint in the assigned expression",
    pvl = """
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
    """,
  )

  choreography(
    fail = "branchNotUnanimous",
    flag = "--generate-permissions",
    desc = "Parts of condition in branch have to agree inside seqprog",
    pvl = """
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
    """,
  )

  choreography(
    fail = "branchNotUnanimous",
    flag = "--generate-permissions",
    desc =
      "Parts of condition in branch have to agree inside seqprog, including conditions for all endpoints",
    pvl = """
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
    """,
  )

  choreography(
    flag = "--generate-permissions",
    desc =
      "If there is only one endpoint, the conditions don't have to agree, as there is only one endpoint",
    pvl = """
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
    """,
  )

  choreography(
    error = "choreography:seqProgParticipantErrors",
    desc = "`if` cannot depend on bob, inside an `if` depending on alice",
    pvl = """
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
  """,
  )

  choreography(
    error = "choreography:seqProgParticipantErrors",
    desc = "If alice branches, bob cannot communicate",
    pvl = """
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
  """,
  )

  choreography(
    error = "choreography:seqProgParticipantErrors",
    desc = "If alice branches, bob cannot assign",
    pvl = """
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
   """,
  )

  choreography(
    flag = "--generate-permissions",
    desc = "Programs where branch conditions agree should verify",
    pvl = """
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
    """,
  )

  choreography(
    fail = "loopUnanimityNotEstablished",
    flag = "--generate-permissions",
    desc =
      "Programs where branch condition unanimity cannot be established should fail",
    pvl = """
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
    """,
  )

  choreography(
    fail = "loopUnanimityNotMaintained",
    flag = "--generate-permissions",
    desc =
      "Programs where branch condition unanimity cannot be maintained should fail",
    pvl = """
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
    """,
  )

  choreography(
    error = "choreography:seqProgParticipantErrors",
    desc = "Loops should also limit the number of participants",
    pvl = """
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
   """,
  )

  choreography(
    flag = "--generate-permissions",
    desc =
      "Loops should also limit the number of participants when combined with branches",
    pvl = """
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
    """,
  )

  choreography(
    error = "choreography:seqProgParticipantErrors",
    desc =
      "Loops should also limit the number of participants when combined with branches, failing",
    pvl = """
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
   """,
  )

  choreography(
    flag = "--generate-permissions",
    desc = "Permission should be generated for constructors as well",
    pvl = """
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
    """,
  )

  choreography(
    fail = "perm",
    desc =
      "When no permission is generated, a failure should occur on endpoint field access",
    pvl = """
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
     """,
  )

  choreography(
    fail = "perm",
    desc =
      "When no permission is generated, a failure should occur on seq assign field access",
    pvl = """
    class Storage {
      int x;
    }

    choreography Example() {
      endpoint alice = Storage();
      run {
        alice.x := 3;
      }
    }
    """,
  )

  choreography(
    flag = "--generate-permissions",
    desc =
      "Permissions are generated for loop invariants, procedures, functions, instance methods, instance functions",
    pvl = """
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
    """,
  )

  choreography(
    flag = "--generate-permissions",
    desc =
      "Permission generation should only generate permissions that are strictly necessary",
    pvl = """
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
    """,
  )

  // Auxiliary methods turned off indefinitely for now. Though can be implemented with just engineering work.
//  (vercors
//    should verify
//    using silicon
//    flag "--generate-permissions"
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
//    flag "--generate-permissions"
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
//    flag "--generate-permissions"
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

  choreography(
    fail = "chorRunPreFailed:false",
    desc = "Precondition of run should be checked",
    pvl = """
    class C { }
    choreography Example(int x) {
      endpoint c = C();
      requires x == 0;
      run {
      }
    }
    """,
  )

  choreography(
    fail = "postFailed:false",
    desc = "Precondition of run is never witnessed if there are no endpoints",
    pvl = """
    ensures x == 0;
    choreography Example(int x) {
      requires x == 0;
      ensures x == 0;
      run {
      }
    }
    """,
  )

  choreography(
    fail = "preFailed:false",
    desc = "Precondition of endpoint constructor should be checked",
    pvl = """
    class Storage {
      requires x == 0;
      constructor (int x) { }
    }

    choreography Example() {
      endpoint alice = Storage(1);

      run {
      }
    }
    """,
  )

  choreography(
    fail = "participantsNotDistinct",
    flag = "--generate-permissions",
    desc = "Endpoints participating in a communicate should be distinct",
    pvl = """
    class Storage {
      int x;
    }

    choreography Example() {
      endpoint alice = Storage();
      run {
        communicate alice.x <- alice.x;
      }
    }
    """,
  )

  // Disabled indefinitely until EncodePermissionStratification.scala is refactored to fully support generics
//  vercors should verify using silicon flag
//    "--generate-permissions" example
//    "technical/veymont/genericEndpoints.pvl"

  choreography(
    fail = "branchNotUnanimous",
    flag = "--generate-permissions",
    desc = "branch unanimity for if",
    pvl = """
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
    """,
  )

  choreography(
    fail = "perm",
    desc = "deref should be safe",
    pvl = """
  class Storage { int x; int y; }

  choreography runPostFails() {
    endpoint alice = Storage();
    requires Perm(alice.x, 1);
    run {
      alice.x := alice.y;
    }
  }
  """,
  )

  choreography(
    fail = "perm",
    desc = "assigning to a deref should fail if there is no permission",
    pvl = """
  class Storage { int x; int y; }
  choreography runPostFails() {
    endpoint alice = Storage();
    requires Perm(alice.x, 1);
    run {
      alice.x := alice.y;
    }
  }
  """,
  )

  choreography(
    desc = "assigning to a deref should succeed if there is permission",
    pvl = """
  class Storage { int x; int y; }
  choreography runPostFails() {
    endpoint alice = Storage();
    requires Perm(alice.x, 1) ** Perm(alice.y, 1);
    run {
      alice.x := alice.y;
    }
  }
  """,
  )

  val wd = "technical/veymont"

  choreography(
    flag = "--generate-permissions",
    input = example(s"$wd/checkLTS/ltstest.pvl"),
  )

  choreography(
    flag = "--generate-permissions",
    input = example(s"$wd/checkLTS/simpleifelse.pvl"),
  )

  choreography(
    error = "choreography:resolutionError:seqProgInvocation",
    flag = "--generate-permissions",
    input = example(s"$wd/checkMainSyntaxAndWellFormedness/ConstructorCall.pvl"),
  )

  choreography(
    error = "choreography:seqProgParticipantErrors",
    input = example(s"$wd/checkMainSyntaxAndWellFormedness/IfCondition.pvl"),
  )

  choreography(
    flag = "--generate-permissions",
    input = example(
      s"$wd/checkMainSyntaxAndWellFormedness/MainConstructorWithArgs.pvl"
    ),
  )

  // Disabled indefinitely while submethods are not supported.
  // vercors should verify using silicon flag
  //   "--generate-permissions" example
  //   s"$wd/checkMainSyntaxAndWellFormedness/MainMethodCall.pvl"
  choreography(
    flag = "--generate-permissions",
    input = example(
      s"$wd/checkMainSyntaxAndWellFormedness/NewNonRoleObject.pvl"
    ),
  )
  choreography(
    flag = "--generate-permissions",
    input = example(s"$wd/checkMainSyntaxAndWellFormedness/NewRoleObject.pvl"),
  )

  choreography(
    error = "choreography:resolutionError:seqProgInvocation",
    flag = "--generate-permissions",
    input = example(
      s"$wd/checkMainSyntaxAndWellFormedness/NonRoleMethodCall.pvl"
    ),
  )

  choreography(
    error = "choreography:resolutionError:seqProgInvocation",
    flag = "--generate-permissions",
    input = example(s"$wd/checkMainSyntaxAndWellFormedness/PureMethodCall.pvl"),
  )

  choreography(
    error = "choreography:resolutionError:seqProgEndpointAssign",
    flags = Seq("--generate-permissions"),
    input = example(
      s"$wd/checkMainSyntaxAndWellFormedness/RoleFieldAssignment.pvl"
    ),
  )

  choreography(
    error = "choreography:resolutionError:chorStatement",
    flag = "--generate-permissions",
    input = example(s"$wd/checkMainSyntaxAndWellFormedness/WaitStatement.pvl"),
  )

  choreography(
    fail = "loopUnanimityNotMaintained",
    flag = "--generate-permissions",
    input = example(s"$wd/checkMainSyntaxAndWellFormedness/WhileCondition.pvl"),
  )

  choreography(
    flag = "--generate-permissions",
    input = example(s"$wd/checkTypesNonMain/RoleFieldType2.pvl"),
  )
  choreography(
    flag = "--generate-permissions",
    input = example(s"$wd/checkTypesNonMain/RoleMethodType4.pvl"),
  )

  choreography(input = example(s"$wd/first.pvl"))

  choreography(
    flag = "--generate-permissions",
    input = example(s"$wd/subFieldAssign.pvl"),
  )

  choreography(fail = "perm", input = example(s"$wd/subFieldAssignError.pvl"))

  // The next three tests highlight a shortcoming of the stratified permissions encoding w.r.t predicates.
  // See the EncodePermissionStratification pass for more info.
  choreography(
    desc =
      "Because of the partial encoding of stratified predicates, functions expecting exact permission amounts that look correct will fail to verify",
    fail = "preFailed:perm",
    pvl = """
      resource P(C c) = Perm(c.x, 1) ** c.x == 0;

      requires P(c);
      ensures unfolding P(c) in c.x == 0;
      pure boolean foo(C c) = (unfolding P(c) in c.x == 0);

      class C {
        int x;

        ensures P(this);
        constructor() {
          x = 0;
          fold P(this);
        }
      }

      choreography Chor() {
        endpoint a = C();
        requires (\endpoint a; P(a));
        run {
          assert (\chor foo(a));
        }
      }
      """,
  )

  choreography(
    desc =
      "Functions that expect only wildcard permissions will succesfully verify, despite the partial encoding of stratified predicates",
    pvl = """
      resource P(C c) = Perm(c.x, 1) ** c.x == 0;

      requires Value(P(c));
      ensures (unfolding Value(P(c)) in c.x == 0);
      pure boolean foo(C c) = (unfolding Value(P(c)) in c.x == 0);

      class C {
        int x;

        ensures P(this);
        constructor() {
          x = 0;
          fold P(this);
        }
      }

      choreography Chor() {
        endpoint a = C();
        requires (\endpoint a; P(a));
        run {
          assert (\chor foo(a));
        }
      }
      """,
  )

  choreography(
    desc =
      "If you precisely half all permissions in a function, you can have exact permission amounts for predicates in a function contract, despite the partial encoding of stratified predicates.",
    pvl = """
      resource P(C c) = Perm(c.x, 1) ** c.x == 0;

      requires Perm(P(c), 1\4);
      ensures (unfolding Perm(P(c), 1\4) in c.x == 0);
      pure boolean foo(C c) = (unfolding Perm(P(c), 1\4) in c.x == 0);

      class C {
        int x;

        ensures P(this);
        constructor() {
          x = 0;
          fold P(this);
        }
      }

      choreography Chor() {
        endpoint a = C();
        requires (\endpoint a; P(a));
        run {
          assert (\chor foo(a));
        }
      }
      """,
  )
}
