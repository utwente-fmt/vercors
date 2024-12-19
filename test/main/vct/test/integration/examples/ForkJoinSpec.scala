package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class ForkJoinSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/forkjoin/forkfail.pvl"
  vercors should verify using anyBackend example "concepts/forkjoin/forkjoininforloop.pvl"
  vercors should verify using anyBackend example "concepts/forkjoin/forkpass.pvl"
  vercors should verify using anyBackend example "concepts/forkjoin/fibonacci.pvl"
  vercors should verify using anyBackend examples("concepts/forkjoin/OwickiGries.pvl", "concepts/forkjoin/Worker.pvl")
  vercors should error withCode "runnableMethodMissing" example "concepts/forkjoin/TestFork.pvl"
  vercors should verify using anyBackend in "The context_everywhere of a run method" pvl
    """
    pure int f();

    class C {
      context_everywhere f() == 3;
      run {
        assert f() == 3;
      }
    }

    requires f() == 3;
    void main() {
        C c = new C();
        fork c;
        join c;
    }
    """





}
