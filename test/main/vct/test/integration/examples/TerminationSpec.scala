package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TerminationSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/decreases/integer.pvl"
  vercors should verify using anyBackend example "concepts/decreases/not-decreasing.pvl"
  vercors should verify using anyBackend example "concepts/decreases/not-bounded.pvl"

  // Issue issues/1232
  vercors should fail withCode "loopTerminationFailed" using silicon in "No decrease for while" java
"""
public class MyClass {

    //@ decreases;
    private int f() {
        while (true) {}
        return 5;
    }

}
"""

  // Issue issues/1065
  vercors should fail withCode "callDecreasesFailed" using silicon in "Calling non-decreasing function" pvl
    """
decreases;
pure int x() = y();

pure int y() = 0;

"""

  vercors should fail withCode "loopDecreasesFailed" using silicon in "Cannot prove decrease for while" java
    """
public class MyClass {

    //@ decreases;
    private int f() {
        //@ decreases;
        while (true) {
        }
        return 5;
    }
}
"""

}


